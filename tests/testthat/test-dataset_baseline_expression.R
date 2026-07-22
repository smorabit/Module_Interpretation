## dataset_baseline_expression_tool() (docs/milestones/milestone_dataset_tools.md
## Part 6): dataset-wide mean expression/detection/CV, top-N dominant genes,
## ribosomal/mitochondrial mass, and depth distribution, computed from
## expression(ms) (+ counts(ms) when available). Offline throughout --
## llegir_example_moduleset() plus small hand-built fixtures.

make_no_expression_ms <- function(){
    structure(list(), class = 'no_expression_ModuleSet')
}
capabilities.no_expression_ModuleSet <- function(ms, ...) c(expression = FALSE)
registerS3method('capabilities', 'no_expression_ModuleSet', capabilities.no_expression_ModuleSet)

# two ribosomal genes (RPL1, RPS2) and one mitochondrial gene (MT-ND1), each
# held constant across cells, alongside two flat non-marker genes -- mean
# expression across cells equals the gene's constant value, so ribosomal mass
# = (10 + 10) / 50 = 40% and mitochondrial mass = 20 / 50 = 40% exactly
make_ribo_mito_ms <- function(){
    genes <- c('RPL1', 'RPS2', 'MT-ND1', 'GENEX1', 'GENEX2')
    expr <- matrix(rep(c(10, 10, 20, 5, 5), 4), nrow = 5, ncol = 4)
    rownames(expr) <- genes
    colnames(expr) <- paste0('cell', 1:4)
    structure(list(expr = expr, data_level = 'cell', aggregated = FALSE), class = 'ribo_mito_ModuleSet')
}
expression.ribo_mito_ModuleSet <- function(ms, ...) ms$expr
capabilities.ribo_mito_ModuleSet <- function(ms, ...) c(expression = TRUE, counts = FALSE)
pkg_versions.ribo_mito_ModuleSet <- function(ms, ...) list(dummy = '1.0')
registerS3method('expression', 'ribo_mito_ModuleSet', expression.ribo_mito_ModuleSet)
registerS3method('capabilities', 'ribo_mito_ModuleSet', capabilities.ribo_mito_ModuleSet)
registerS3method('pkg_versions', 'ribo_mito_ModuleSet', pkg_versions.ribo_mito_ModuleSet)

test_that('dataset_baseline_expression_tool() returns a valid baseline_expression fragment', {
    ms <- llegir_example_moduleset()
    frag <- dataset_baseline_expression_tool(list(ms = ms, params = list()))

    expect_true(validate_dataset_fragment(frag))
    expect_equal(frag$type, 'baseline_expression')
    expect_true(all(c('gene_name', 'mean_expr', 'detection_rate', 'cv') %in% colnames(frag$result)))
    expect_lte(nrow(frag$result), 15)
    expect_true(grepl('cells', frag$compact_summary, fixed = TRUE))

    metrics <- vapply(frag$top_findings, function(f) f$metric %||% NA_character_, character(1))
    expect_true('pct_ribo_mass' %in% metrics)
    expect_true('pct_mito_mass' %in% metrics)
    expect_true('depth_distribution' %in% metrics)
})

test_that('dataset_baseline_expression_tool() honors params$top_n', {
    ms <- llegir_example_moduleset()
    frag <- dataset_baseline_expression_tool(list(ms = ms, params = list(top_n = 3)))

    expect_equal(nrow(frag$result), 3)
    expect_true(all(frag$result$mean_expr == sort(frag$result$mean_expr, decreasing = TRUE)))
})

test_that('dataset_baseline_expression_tool() skips gracefully without the expression capability', {
    ctx <- list(ms = make_no_expression_ms(), params = list())
    expect_message(result <- dataset_baseline_expression_tool(ctx), 'skipped')
    expect_null(result)
})

test_that('dataset_baseline_expression_tool() computes ribosomal/mitochondrial mass correctly', {
    ctx <- list(ms = make_ribo_mito_ms(), params = list())
    frag <- dataset_baseline_expression_tool(ctx)

    expect_true(validate_dataset_fragment(frag))
    pct_ribo <- Find(function(f) identical(f$metric, 'pct_ribo_mass'), frag$top_findings)$value
    pct_mito <- Find(function(f) identical(f$metric, 'pct_mito_mass'), frag$top_findings)$value
    expect_equal(pct_ribo, 40)
    expect_equal(pct_mito, 40)
    expect_equal(frag$result$gene_name[1], 'MT-ND1')
})

test_that('run_dataset_context() runs the registered baseline_expression tool end to end', {
    ms <- llegir_example_moduleset()
    dc <- run_dataset_context(ms, list(list(id = 'baseline_expression', params = list())))

    expect_equal(length(dc$dataset_fragments), 1)
    expect_equal(dc$dataset_fragments[[1]]$type, 'baseline_expression')
})
