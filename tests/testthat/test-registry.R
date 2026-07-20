## tool registry: register_tool()/get_tool()/list_tools() (docs/milestone_extensibility.md
## Part 2b). Core tools are registered via .onLoad() (R/registry.R), so
## list_tools()/get_tool() work on package load without any test setup.
## custom_coherence_tool below is the "one small worked custom tool" proving
## the register_tool() API end to end: real (if simple) bespoke logic -- mean
## pairwise gene-gene expression correlation within the module, a coherence
## statistic no core tool computes -- registered and run exactly like a core
## tool, including the same graceful-skip and schema-validation machinery.

test_that('core tools are registered via the same register_tool() mechanism', {
    expect_true(all(c('hub_genes', 'cluster_dme', 'module_by_metadata', 'geneset_enrichment', 'signature_correlation') %in% list_tools()))
    spec <- get_tool('cluster_dme')
    expect_s3_class(spec, 'tool_spec')
    expect_equal(spec$requires, c('grouping', 'module_scores'))
    expect_error(get_tool('not_a_real_tool'), 'not registered')
})

test_that('register_tool() validates its arguments', {
    expect_error(register_tool(id = 1, fn = hub_genes_tool, type = 'ranked_genes', description = 'x'), 'id must be')
    expect_error(register_tool(id = 'x', fn = 'not_a_function', type = 'ranked_genes', description = 'x'), 'fn must be')
    expect_error(register_tool(id = 'x', fn = hub_genes_tool, type = 'not_a_type', description = 'x'), 'invalid type')
    expect_error(register_tool(id = 'x', fn = hub_genes_tool, type = 'ranked_genes', description = 'x', requires = 1), 'requires must be')
})

test_that('module_by_metadata\'s requires is param-dependent (function form)', {
    spec <- get_tool('module_by_metadata')
    expect_true(is.function(spec$requires))
    expect_setequal(.tool_spec_requires(spec, list(column_type = 'categorical')), c('module_scores', 'sample_ids'))
    expect_equal(.tool_spec_requires(spec, list(column_type = 'continuous')), 'module_scores')
})

# a real (if small) custom tool: mean pairwise gene-gene expression
# correlation within the module -- an internal coherence statistic that no
# core tool computes -- ranked one row per gene, emitted as 'ranked_genes'
custom_coherence_tool <- function(ctx){
    genes <- gene_membership(ctx$ms, ctx$module_id)$gene_name
    expr <- as.matrix(expression(ctx$ms))[genes, , drop = FALSE]
    cor_mat <- stats::cor(t(expr))
    diag(cor_mat) <- NA
    mean_cor <- rowMeans(cor_mat, na.rm = TRUE)
    result <- data.frame(gene_name = genes, mean_cor = unname(mean_cor))
    result <- result[order(-result$mean_cor), ]
    rownames(result) <- NULL

    evidence_fragment(
        fragment_id = 'custom_coherence',
        tool_id = 'custom_coherence',
        module_id = ctx$module_id,
        type = 'ranked_genes',
        result = result,
        compact_summary = paste0('most internally coherent gene: ', result$gene_name[1]),
        top_findings = list(list(gene = result$gene_name[1], mean_cor = result$mean_cor[1])),
        effect_strength = max(result$mean_cor),
        direction = 'na',
        provenance = make_provenance(tool_version = '0.1', pkg_versions = pkg_versions(ctx$ms))
    )
}

test_that('a registered custom tool runs through run_module() exactly like a core tool', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    register_tool(
        'custom_coherence', custom_coherence_tool, type = 'ranked_genes',
        description = 'Mean pairwise gene-gene expression correlation within the module',
        requires = 'expression'
    )
    packet <- run_module(go_components_ms, 'module_a', list(list(id = 'custom_coherence', params = list())))
    expect_equal(length(packet$fragments), 1)
    expect_true(validate_evidence_fragment(packet$fragments[[1]]))
    expect_equal(packet$fragments[[1]]$fragment_id, 'custom_coherence')
})

test_that('run_module() skips a capability-mismatched tool (referenced by id) and records the reason, not fatally', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    expect_false(has_capability(go_gene_list_ms_nocap, 'grouping'))
    packet <- suppressMessages(run_module(
        go_gene_list_ms_nocap, 'module_a',
        list(
            list(id = 'hub_genes', params = list(n_hubs = 5)),
            list(id = 'cluster_dme', params = list(group_by = 'cell_type'))
        )
    ))
    ids <- vapply(packet$fragments, function(f) f$fragment_id, character(1))
    expect_setequal(ids, 'hub_genes')
    expect_equal(length(packet$provenance$skipped), 1)
    expect_equal(packet$provenance$skipped[[1]]$tool_id, 'cluster_dme')
    expect_match(packet$provenance$skipped[[1]]$reason, 'grouping')
})

test_that('run_module() fails loudly on a malformed fragment from a registered tool', {
    skip_if_not(go_data_available, 'GO Biological Process data not available')

    bad_tool <- function(ctx) list(not = 'an evidence_fragment')
    register_tool('bad_tool', bad_tool, type = 'ranked_genes', description = 'deliberately malformed', requires = character(0))
    expect_error(
        run_module(go_components_ms, 'module_a', list(list(id = 'bad_tool', params = list()))),
        'evidence_fragment'
    )
})
