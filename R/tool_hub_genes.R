## hub_genes: top module genes ranked by membership (kME). Touches only the
## ModuleSet adapter (gene_membership, pkg_versions).

hub_genes_tool <- function(ctx){
    n_hubs <- ctx$params$n_hubs %||% 25

    gm <- gene_membership(ctx$ms, ctx$module_id)
    top <- head(gm, n_hubs)

    top_findings <- lapply(seq_len(nrow(top)), function(i){
        list(gene = top$gene_name[i], kme = top$kme[i])
    })

    compact_summary <- paste0(
        'top ', nrow(top), ' hub genes by kME: ',
        paste(head(top$gene_name, 10), collapse = ', '),
        if (nrow(top) > 10) ', ...' else ''
    )

    evidence_fragment(
        fragment_id = 'hub_genes',
        tool_id = 'hub_genes',
        module_id = ctx$module_id,
        type = 'ranked_genes',
        result = top,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = if (nrow(top) > 0) max(top$kme) else 0,
        direction = 'na',
        provenance = make_provenance(
            tool_version = '0.1',
            params = list(n_hubs = n_hubs),
            pkg_versions = pkg_versions(ctx$ms)
        )
    )
}
