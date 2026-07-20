## gene_list_ModuleSet: modules from named gene lists, scored on the fly via
## UCell or decoupleR (docs/milestone_extensibility.md task 4). Also exercises
## the capability-aware graceful-skip path (task 5): a variant with no
## group_col/sample_col declared must not crash cluster_dme_tool /
## module_by_metadata_tool, just skip them.

test_that('gene_list_ModuleSet() has no gene_weights, regardless of scoring method', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_false(has_capability(go_gene_list_ms_ucell, 'gene_weights'))
    expect_false(has_capability(go_gene_list_ms_decoupler, 'gene_weights'))
    expect_true(all(is.na(gene_membership(go_gene_list_ms_ucell, 'module_a')$kme)))
})

test_that('gene_list_ModuleSet() computes module scores on the fly (UCell)', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_true(has_capability(go_gene_list_ms_ucell, 'module_scores'))
    scores <- module_scores(go_gene_list_ms_ucell, module = 'module_a')
    expect_true(is.numeric(scores))
    expect_equal(length(scores), ncol(expression(go_gene_list_ms_ucell)))
    expect_equal(pkg_versions(go_gene_list_ms_ucell)$UCell, as.character(utils::packageVersion('UCell')))
})

test_that('gene_list_ModuleSet() computes module scores on the fly (decoupleR)', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_true(has_capability(go_gene_list_ms_decoupler, 'module_scores'))
    scores <- module_scores(go_gene_list_ms_decoupler, module = 'module_a')
    expect_true(is.numeric(scores))
    expect_equal(length(scores), ncol(expression(go_gene_list_ms_decoupler)))
    expect_equal(pkg_versions(go_gene_list_ms_decoupler)$decoupleR, as.character(utils::packageVersion('decoupleR')))
})

test_that('the evidence pipeline runs end to end on a gene_list_ModuleSet and produces valid packets', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    for (ms in list(go_gene_list_ms_ucell, go_gene_list_ms_decoupler)) {
        packet <- run_module(ms, 'module_a', go_tool_config(), input_hash = 'go_gene_list_test')
        expect_equal(length(packet$fragments), 4)
        for (frag in packet$fragments) expect_true(validate_evidence_fragment(frag))
    }
})

test_that('cluster_dme_tool() skips gracefully (returns NULL, not an error) without a grouping capability', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_false(has_capability(go_gene_list_ms_nocap, 'grouping'))
    ctx <- list(ms = go_gene_list_ms_nocap, module_id = 'module_a', params = list(group_by = 'cell_type'))
    expect_message(result <- cluster_dme_tool(ctx), 'skipped')
    expect_null(result)
})

test_that('module_by_metadata_tool() skips gracefully without a sample_ids capability, for categorical columns', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    expect_false(has_capability(go_gene_list_ms_nocap, 'sample_ids'))
    ctx <- list(ms = go_gene_list_ms_nocap, module_id = 'module_a', params = list(column = 'diagnosis', column_type = 'categorical'))
    expect_message(result <- module_by_metadata_tool(ctx), 'skipped')
    expect_null(result)
})

test_that('run_module() omits skipped tools from the packet instead of failing the module', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')
    packet <- suppressMessages(run_module(go_gene_list_ms_nocap, 'module_a', go_tool_config(), input_hash = 'go_nocap_test'))
    ids <- vapply(packet$fragments, function(f) f$fragment_id, character(1))
    # cluster_dme and metadata::diagnosis both need capabilities this ModuleSet lacks;
    # hub_genes and geneset_enrichment only need gene_membership()/expression(), always present
    expect_setequal(ids, c('hub_genes', 'geneset_enrichment'))
    for (frag in packet$fragments) expect_true(validate_evidence_fragment(frag))
})
