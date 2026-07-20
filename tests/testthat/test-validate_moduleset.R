## validate_moduleset(): the full adapter contract check
## (docs/milestone_abstract_moduleset.md Part 3). Confirms it passes for all
## four package adapters and fails loudly -- with a message naming the
## specific violation -- on malformed inputs, including a bare list with no
## ModuleSet methods at all and a custom class that implements every generic
## but violates one contract rule at a time.

test_that('validate_moduleset() passes for hdWGCNA_ModuleSet', {
    skip_if_not(csf_data_available, 'CSF dev object not available')
    expect_true(validate_moduleset(ms_test))
})

test_that('validate_moduleset() passes for components_ModuleSet', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_true(validate_moduleset(go_components_ms))
})

test_that('validate_moduleset() passes for gene_list_ModuleSet', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_true(validate_moduleset(go_gene_list_ms_ucell))
    expect_true(validate_moduleset(go_gene_list_ms_decoupler))
})

test_that('validate_moduleset() passes for synthetic_ModuleSet', {
    expect_true(validate_moduleset(llegir_example_moduleset()))
})

test_that('validate_moduleset() reports pseudobulk = FALSE for every package adapter', {
    expect_false(capabilities(llegir_example_moduleset())[['pseudobulk']])
    skip_if_not(csf_data_available, 'CSF dev object not available')
    expect_false(capabilities(ms_test)[['pseudobulk']])
})

test_that('validate_moduleset() fails loudly on a bare list with no ModuleSet methods', {
    bad_ms <- structure(list(), class = 'not_a_moduleset')
    expect_error(validate_moduleset(bad_ms), 'modules\\(\\)')
})

## a minimal, otherwise-contract-compliant fake adapter used below to isolate
## one violation at a time -- built directly on components_ModuleSet so every
## generic dispatches correctly except the one deliberately broken per test.
## Setting `counts` directly (bypassing the components_ModuleSet() constructor's
## own dimension check) is enough to break the counts/expression alignment
## rule, since capabilities.components_ModuleSet() derives counts = TRUE
## straight from `!is.null(ms$counts)`.
.fake_ms <- function(override = list()){
    gene_table <- data.frame(module = 'm1', gene_name = c('G1', 'G2'), weight = c(0.9, 0.5))
    expr <- matrix(rnorm(20), nrow = 2, dimnames = list(c('G1', 'G2'), paste0('c', 1:10)))
    meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))
    ms <- components_ModuleSet(gene_table, expr, meta, group_col = 'cell_type')
    modifyList(ms, override)
}

test_that('validate_moduleset() fails loudly when counts() dimensions do not match expression()', {
    bad_ms <- .fake_ms(list(counts = matrix(1, nrow = 2, ncol = 3)))
    expect_error(validate_moduleset(bad_ms), 'counts\\(\\)')
})

test_that('validate_moduleset() fails loudly when data_level is not a length-1 character', {
    bad_ms <- .fake_ms(list(data_level = c('cell', 'sample')))
    expect_error(validate_moduleset(bad_ms), 'data_level')
})

test_that('validate_moduleset() fails loudly when aggregated is not a length-1 logical', {
    bad_ms <- .fake_ms(list(aggregated = 'no'))
    expect_error(validate_moduleset(bad_ms), 'aggregated')
})

test_that('run_orchestrator() validates ms by default and can skip validation', {
    bad_ms <- structure(list(), class = 'not_a_moduleset')
    expect_error(
        run_orchestrator(bad_ms, list(list(fn = hub_genes_tool, params = list())), output_dir = tempfile()),
        'validate_moduleset'
    )
    # with validate = FALSE, it instead fails later, inside modules(ms) itself
    expect_error(
        run_orchestrator(bad_ms, list(list(fn = hub_genes_tool, params = list())), output_dir = tempfile(), validate = FALSE),
        'no applicable method'
    )
})
