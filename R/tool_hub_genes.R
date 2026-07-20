## hub_genes: top module genes ranked by membership (kME). Touches only the
## ModuleSet adapter (gene_membership, pkg_versions).

#' Evidence tool: top module genes ranked by membership (kME)
#'
#' A core evidence tool. Touches only the `ModuleSet` adapter contract
#' ([gene_membership()], [pkg_versions()]), so it works against any backend.
#'
#' @param ctx A tool context list: `list(ms, module_id, params)`, as built by
#'   [run_module()]. `ctx$params$n_hubs` (default 25) is the number of top
#'   genes to keep.
#' @return An `evidence_fragment` of type `'ranked_genes'`.
#' @examples
#' ms <- llegir_example_moduleset()
#' hub_genes_tool(list(ms = ms, module_id = modules(ms)[1], params = list(n_hubs = 10)))
#' @export
hub_genes_tool <- function(ctx){
    n_hubs <- ctx$params$n_hubs %||% 25

    gm <- gene_membership(ctx$ms, ctx$module_id)
    top <- utils::head(gm, n_hubs)

    top_findings <- lapply(seq_len(nrow(top)), function(i){
        list(gene = top$gene_name[i], kme = top$kme[i])
    })

    compact_summary <- paste0(
        'top ', nrow(top), ' hub genes by kME: ',
        paste(utils::head(top$gene_name, 10), collapse = ', '),
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
            pkg_versions = pkg_versions(ctx$ms),
            module_method = ctx$module_method %||% NA_character_
        )
    )
}
