## dataset_variance_structure_tool() (docs/milestones/milestone_dataset_tools.md
## Part 5): principal-component regression against metadata covariates on the
## pseudo-bulk view, reusing pb_ms / pb_fixture from synthetic_pseudobulk.R
## (tests/testthat/setup.R). Offline throughout, no live syntheses.

# a batch-driven fixture: half the genes shift lambda with batch (a real
# relative-abundance shift that survives CPM normalization) while the other
# half stay flat, so batch drives the top PC; condition is assigned
# independent of batch, so the top PC's variance is a technical artifact
# rather than the biological contrast -- the confounding caveat should fire
make_confounded_pb_ms <- function(seed = 42, n_samples = 16, n_driven = 10, n_flat = 10){
    set.seed(seed)
    batch <- rep(c('batch1', 'batch2'), each = n_samples / 2)
    condition <- sample(rep(c('case', 'control'), length.out = n_samples))
    sample_id <- paste0('sample', seq_len(n_samples))

    base_lambda <- 200
    lambda_driven <- ifelse(batch == 'batch1', base_lambda * 8, base_lambda)
    driven_counts <- t(vapply(seq_len(n_driven), function(i) stats::rpois(n_samples, lambda_driven), numeric(n_samples)))
    flat_counts <- t(vapply(seq_len(n_flat), function(i) stats::rpois(n_samples, base_lambda), numeric(n_samples)))
    counts <- rbind(driven_counts, flat_counts)
    rownames(counts) <- c(paste0('DRIVEN', seq_len(n_driven)), paste0('FLAT', seq_len(n_flat)))
    colnames(counts) <- sample_id

    meta <- data.frame(sample = sample_id, condition = condition, batch = batch, row.names = sample_id)
    gene_sets <- list(module_a = rownames(counts)[seq_len(n_driven)])
    pseudobulk_ModuleSet(counts, gene_sets, meta, group_col = 'batch', sample_col = 'sample')
}

test_that('dataset_variance_structure_tool() returns a valid variance_structure fragment', {
    ctx <- list(ms = pb_ms, params = list(covariates = c('condition', 'n_cells'), condition_col = 'condition'))
    frag <- dataset_variance_structure_tool(ctx)

    expect_true(validate_dataset_fragment(frag))
    expect_equal(frag$type, 'variance_structure')
    expect_true(all(c('component', 'covariate', 'R2', 'adj_R2', 'pval', 'fdr') %in% colnames(frag$result)))
    expect_true(grepl('pseudo-bulk samples', frag$compact_summary, fixed = TRUE))
})

test_that('dataset_variance_structure_tool() skips gracefully without a pseudo-bulk view', {
    ms <- llegir_example_moduleset()
    ctx <- list(ms = ms, params = list(covariates = c('diagnosis'), condition_col = 'diagnosis'))
    expect_message(result <- dataset_variance_structure_tool(ctx), 'skipped')
    expect_null(result)
})

test_that('dataset_variance_structure_tool() errors without covariates or condition_col', {
    expect_error(
        dataset_variance_structure_tool(list(ms = pb_ms, params = list(condition_col = 'condition'))),
        'covariates'
    )
    expect_error(
        dataset_variance_structure_tool(list(ms = pb_ms, params = list(covariates = c('condition')))),
        'condition_col'
    )
})

test_that('dataset_variance_structure_tool() flags condition_confounded_with_batch when batch dominates the top PC', {
    ms <- make_confounded_pb_ms()
    ctx <- list(ms = ms, params = list(covariates = c('condition', 'batch'), condition_col = 'condition'))
    frag <- dataset_variance_structure_tool(ctx)

    expect_true(validate_dataset_fragment(frag))
    expect_true('condition_confounded_with_batch' %in% unlist(frag$caveats))
})

test_that('run_dataset_context() runs the registered variance_structure tool end to end', {
    dc <- run_dataset_context(
        pb_ms,
        list(list(id = 'variance_structure', params = list(covariates = c('condition', 'n_cells'), condition_col = 'condition')))
    )

    expect_equal(length(dc$dataset_fragments), 1)
    expect_equal(dc$dataset_fragments[[1]]$type, 'variance_structure')
})
