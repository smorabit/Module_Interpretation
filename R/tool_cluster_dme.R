## cluster_dme: which cell states (a grouping column, e.g. lv2_annot) express
## this module. Touches only the ModuleSet adapter (module_scores, metadata,
## pkg_versions) plus the shared categorical_group_test() helper.

#' Evidence tool: which cell states express this module
#'
#' A core evidence tool. Touches only the `ModuleSet` adapter contract
#' ([module_scores()], [metadata()], [pkg_versions()]) plus the shared
#' [categorical_group_test()] helper, so it works against any backend.
#'
#' @param ctx A tool context list: `list(ms, module_id, params)`, as built by
#'   [run_module()]. `ctx$params$group_by` (required) is the metadata column
#'   naming the cell-state grouping (e.g. `'lv2_annot'`).
#' @return An `evidence_fragment` of type `'state_expression'`, or `NULL` if
#'   `ctx$ms` lacks the `grouping` or `module_scores` capability (see
#'   [capabilities()]) -- a graceful skip, not an error.
#' @examples
#' ms <- llegir_example_moduleset()
#' cluster_dme_tool(list(ms = ms, module_id = modules(ms)[1], params = list(group_by = 'cell_type')))
#' @export
cluster_dme_tool <- function(ctx){
    group_by <- ctx$params$group_by
    if (is.null(group_by)) stop('cluster_dme requires params$group_by')

    if (!has_capability(ctx$ms, 'grouping') || !has_capability(ctx$ms, 'module_scores')) {
        message('cluster_dme: skipped, module set lacks the grouping/module_scores capability')
        return(NULL)
    }

    scores <- module_scores(ctx$ms, module = ctx$module_id)
    groups <- metadata(ctx$ms)[[group_by]]
    if (is.null(groups)) stop('metadata column not found: ', group_by)

    keep <- !is.na(groups)
    test <- categorical_group_test(scores[keep], groups[keep])
    result <- test$table
    top <- result[1, ]

    top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
        list(
            cluster = result$group[i],
            mean_score = result$mean_score[i],
            rank_biserial = result$rank_biserial[i],
            fdr = result$fdr[i]
        )
    })

    compact_summary <- paste0(
        'strongest state: ', top$group,
        ' (r=', round(top$rank_biserial, 2), ', FDR=', signif(top$fdr, 2),
        '); omnibus Kruskal p=', signif(test$omnibus_p, 2)
    )

    evidence_fragment(
        fragment_id = 'cluster_dme',
        tool_id = 'cluster_dme',
        module_id = ctx$module_id,
        type = 'state_expression',
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = abs(top$rank_biserial),
        significance = test$omnibus_p,
        direction = top$direction,
        provenance = make_provenance(
            tool_version = '0.1',
            # method = 'per_cell' now; a pseudobulk-aggregated variant can slot in
            # here later since categorical_group_test() only needs scores + groups
            params = list(group_by = group_by, method = 'per_cell'),
            pkg_versions = pkg_versions(ctx$ms),
            module_method = ctx$module_method %||% NA_character_
        )
    )
}
