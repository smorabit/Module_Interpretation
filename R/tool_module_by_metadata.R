## module_by_metadata: module score vs. a declared metadata column. categorical
## (diagnosis, sample) -> group means + Kruskal/one-vs-rest Wilcoxon, shared
## with cluster_dme; continuous -> Pearson/Spearman correlation, kept generic
## for future datasets even though CSF only exercises the categorical branch.

module_by_metadata_tool <- function(ctx){
    column <- ctx$params$column
    if (is.null(column)) stop('module_by_metadata requires params$column')
    column_type <- ctx$params$column_type %||% 'categorical'

    scores <- module_scores(ctx$ms, module = ctx$module_id)
    meta_col <- metadata(ctx$ms)[[column]]
    if (is.null(meta_col)) stop('metadata column not found: ', column)

    keep <- !is.na(meta_col)
    scores <- scores[keep]
    meta_col <- meta_col[keep]

    if (column_type == 'categorical') {
        test <- categorical_group_test(scores, meta_col)
        result <- test$table
        top <- result[1, ]

        top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
            list(
                group = result$group[i], mean_score = result$mean_score[i],
                rank_biserial = result$rank_biserial[i], fdr = result$fdr[i]
            )
        })
        compact_summary <- paste0(
            column, ': strongest group ', top$group,
            ' (r=', round(top$rank_biserial, 2), ', FDR=', signif(top$fdr, 2),
            '); omnibus Kruskal p=', signif(test$omnibus_p, 2)
        )
        effect_strength <- abs(top$rank_biserial)
        significance <- test$omnibus_p
        direction <- top$direction
        type <- 'categorical_association'
    } else if (column_type == 'continuous') {
        result <- continuous_correlation_test(scores, as.numeric(meta_col))
        compact_summary <- paste0(
            column, ': Pearson r=', round(result$pearson_r, 2),
            ' (p=', signif(result$pearson_p, 2), ')'
        )
        top_findings <- list(list(
            pearson_r = result$pearson_r, pearson_p = result$pearson_p,
            spearman_rho = result$spearman_rho
        ))
        effect_strength <- abs(result$pearson_r)
        significance <- result$pearson_p
        direction <- if (result$pearson_r > 0) 'up' else 'down'
        type <- 'continuous_correlation'
    } else {
        stop('unknown column_type: ', column_type)
    }

    evidence_fragment(
        fragment_id = paste0('metadata::', column),
        tool_id = 'module_by_metadata',
        module_id = ctx$module_id,
        type = type,
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = effect_strength,
        significance = significance,
        direction = direction,
        provenance = make_provenance(
            tool_version = '0.1',
            params = list(column = column, column_type = column_type),
            pkg_versions = pkg_versions(ctx$ms)
        )
    )
}
