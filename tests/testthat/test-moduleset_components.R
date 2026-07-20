## components_ModuleSet: the generic tidy-components adapter
## (docs/milestone_extensibility.md task 2). Confirms it satisfies the same
## adapter contract as hdWGCNA_ModuleSet and that the evidence pipeline runs
## on it end to end, using the real-GO-gene-set fixture from
## synthetic_extensibility.R.

test_that('components_ModuleSet() validates its inputs', {
    gene_table <- data.frame(module = 'm1', gene_name = c('G1', 'G2'), weight = c(0.9, 0.5))
    expr <- matrix(rnorm(20), nrow = 2, dimnames = list(c('G1', 'G2'), paste0('c', 1:10)))
    meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))

    expect_error(components_ModuleSet(data.frame(x = 1), expr, meta), 'module.*gene_name')
    expect_error(components_ModuleSet(gene_table, expr, meta[1:5, , drop = FALSE]), 'align')
    expect_error(components_ModuleSet(gene_table, expr, meta, group_col = 'not_a_column'), 'group_col')
    expect_error(components_ModuleSet(gene_table, expr, meta, sample_col = 'not_a_column'), 'sample_col')
    expect_s3_class(components_ModuleSet(gene_table, expr, meta), 'components_ModuleSet')
})

test_that('components_ModuleSet() defaults kme to NA and gene_weights to FALSE without a weight column', {
    gene_table <- data.frame(module = 'm1', gene_name = c('G1', 'G2'))
    expr <- matrix(rnorm(20), nrow = 2, dimnames = list(c('G1', 'G2'), paste0('c', 1:10)))
    meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))
    ms <- components_ModuleSet(gene_table, expr, meta)
    expect_false(has_capability(ms, 'gene_weights'))
    expect_true(all(is.na(gene_membership(ms, 'm1')$kme)))
})

test_that('components_ModuleSet() reports module_scores/clusters/sample_ids as declared', {
    gene_table <- data.frame(module = 'm1', gene_name = 'G1', weight = 1)
    expr <- matrix(rnorm(10), nrow = 1, dimnames = list('G1', paste0('c', 1:10)))
    meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))

    bare_ms <- components_ModuleSet(gene_table, expr, meta)
    caps <- capabilities(bare_ms)
    expect_false(caps[['module_scores']])
    expect_false(caps[['grouping']])
    expect_false(caps[['sample_ids']])
    expect_null(module_scores(bare_ms, module = 'm1'))

    full_ms <- components_ModuleSet(
        gene_table, expr, meta,
        scores = data.frame(m1 = rnorm(10), row.names = colnames(expr)),
        group_col = 'cell_type'
    )
    full_caps <- capabilities(full_ms)
    expect_true(full_caps[['module_scores']])
    expect_true(full_caps[['grouping']])
    expect_false(full_caps[['sample_ids']])
})

test_that('components_ModuleSet() adapter contract matches hdWGCNA_ModuleSet\'s shapes', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    mods <- modules(go_components_ms)
    expect_setequal(mods, c('module_a', 'module_b'))

    gm <- gene_membership(go_components_ms, 'module_a')
    expect_true(all(c('gene_name', 'module', 'kme') %in% colnames(gm)))
    expect_equal(gm$kme, sort(gm$kme, decreasing = TRUE))

    all_scores <- module_scores(go_components_ms)
    expect_true(is.data.frame(all_scores))
    expect_equal(nrow(all_scores), ncol(expression(go_components_ms)))
    one_score <- module_scores(go_components_ms, module = 'module_a')
    expect_true(is.numeric(one_score))

    expect_equal(ncol(expression(go_components_ms)), nrow(metadata(go_components_ms)))
    caps <- capabilities(go_components_ms)
    # pseudobulk is FALSE for every current adapter (docs/milestone_abstract_moduleset.md Part 3);
    # everything else is declared TRUE for this fully-populated fixture
    expect_true(all(caps[names(caps) != 'pseudobulk']))
    expect_false(caps[['pseudobulk']])
})

test_that('the evidence pipeline runs end to end on a components_ModuleSet and produces valid packets', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    packet <- run_module(go_components_ms, 'module_a', go_tool_config(), input_hash = 'go_components_test')
    expect_equal(length(packet$fragments), 4)
    for (frag in packet$fragments) expect_true(validate_evidence_fragment(frag))

    ids <- vapply(packet$fragments, function(f) f$fragment_id, character(1))
    expect_setequal(ids, c('hub_genes', 'cluster_dme', 'metadata::diagnosis', 'geneset_enrichment'))

    # module_a's hub genes are exactly its own real GO BP term's genes, so
    # that term should come back as (one of) the top enrichment hit(s)
    enrich <- packet$fragments[[which(ids == 'geneset_enrichment')]]
    expect_true(go_module_a %in% enrich$result$term)
})
