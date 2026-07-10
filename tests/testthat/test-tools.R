## core tools: each must return a fragment that passes validate_evidence_fragment
## and carries the right `type`, run against a real module from the CSF object.

test_that('hub_genes_tool() returns a valid ranked_genes fragment', {
    ctx <- list(ms = ms_test, module_id = mod_test, params = list(n_hubs = 10))
    frag <- hub_genes_tool(ctx)
    expect_true(validate_evidence_fragment(frag))
    expect_equal(frag$type, 'ranked_genes')
    expect_equal(nrow(frag$result), 10)
    expect_true(all(c('gene_name', 'kme') %in% colnames(frag$result)))
})

test_that('cluster_dme_tool() returns a valid state_expression fragment', {
    ctx <- list(ms = ms_test, module_id = mod_test, params = list(group_by = 'lv2_annot'))
    frag <- cluster_dme_tool(ctx)
    expect_true(validate_evidence_fragment(frag))
    expect_equal(frag$type, 'state_expression')
    expect_true(frag$effect_strength >= 0)
    expect_true(frag$direction %in% c('up', 'down'))
})

test_that('cluster_dme_tool() errors without group_by', {
    ctx <- list(ms = ms_test, module_id = mod_test, params = list())
    expect_error(cluster_dme_tool(ctx), 'group_by')
})

test_that('module_by_metadata_tool() returns a valid categorical_association fragment', {
    ctx <- list(ms = ms_test, module_id = mod_test,
                params = list(column = 'diagnosis', column_type = 'categorical'))
    frag <- module_by_metadata_tool(ctx)
    expect_true(validate_evidence_fragment(frag))
    expect_equal(frag$type, 'categorical_association')
    expect_equal(frag$fragment_id, 'metadata::diagnosis')
})

test_that('module_by_metadata_tool() errors on an unknown column', {
    ctx <- list(ms = ms_test, module_id = mod_test, params = list(column = 'not_a_column'))
    expect_error(module_by_metadata_tool(ctx), 'not found')
})

test_that('geneset_enrichment_tool() returns a valid geneset_enrichment fragment', {
    ctx <- list(ms = ms_test, module_id = mod_test, params = list(n_hubs = 25))
    frag <- geneset_enrichment_tool(ctx)
    expect_true(validate_evidence_fragment(frag))
    expect_equal(frag$type, 'geneset_enrichment')
    # network-dependent: assert only the contract holds, not that terms came back
    expect_true(frag$effect_strength >= 0)
})
