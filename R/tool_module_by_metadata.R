## module_by_metadata: module score vs. a declared metadata column. categorical
## (diagnosis, sample) -> group means + Kruskal/one-vs-rest Wilcoxon, shared
## with cluster_dme; continuous -> Pearson/Spearman correlation, kept generic
## for future datasets even though CSF only exercises the categorical branch.
##
## pseudoreplication fix (milestone 1.5): a variable that is constant within
## sample (e.g. diagnosis) is tested at the SAMPLE level, not the cell level
## -- cells within a sample are correlated, so a cell-level test on a
## sample-level variable inflates significance. `sample_col` (default
## 'sample') is the aggregation unit; `level` (default 'auto') picks cell vs.
## sample by checking whether `column` is constant within every sample, and
## can be forced to 'cell' or 'sample' to compare the two directly.
## `column == sample_col` is a special case: it's the aggregation unit
## itself, so it gets a descriptive per-sample summary, not a group test.

#' Evidence tool: module score vs. a declared metadata column
#'
#' A core evidence tool. Touches only the `ModuleSet` adapter contract
#' ([module_scores()], [metadata()], [pkg_versions()]) plus the shared
#' [categorical_group_test()] / [continuous_correlation_test()] helpers.
#' Categorical columns (e.g. diagnosis, sample) get group means plus
#' Kruskal/one-vs-rest Wilcoxon; continuous columns get Pearson/Spearman
#' correlation.
#'
#' @param ctx A tool context list: `list(ms, module_id, params)`, as built by
#'   [run_module()]. `ctx$params$column` (required) is the metadata column to
#'   test. `ctx$params$column_type` is `'categorical'` (default) or
#'   `'continuous'`. `ctx$params$sample_col` (default `'sample'`) is the
#'   sample-id column used for the pseudoreplication fix. `ctx$params$level`
#'   is `'auto'` (default), `'cell'`, or `'sample'`.
#' @return An `evidence_fragment` of type `'categorical_association'` or
#'   `'continuous_correlation'`, or `NULL` if `ctx$ms` lacks a capability this
#'   call needs (`module_scores` always; `sample_ids` for categorical
#'   testing, see [capabilities()]) -- a graceful skip, not an error.
#' @examples
#' ms <- llegir_example_moduleset()
#' module_by_metadata_tool(list(
#'     ms = ms, module_id = modules(ms)[1],
#'     params = list(column = 'diagnosis', column_type = 'categorical')
#' ))
#' @export
module_by_metadata_tool <- function(ctx){
    column <- ctx$params$column
    if (is.null(column)) stop('module_by_metadata requires params$column')
    column_type <- ctx$params$column_type %||% 'categorical'
    sample_col <- ctx$params$sample_col %||% 'sample'
    level_param <- ctx$params$level %||% 'auto'

    if (!has_capability(ctx$ms, 'module_scores')) {
        message('module_by_metadata: skipped, module set lacks the module_scores capability')
        return(NULL)
    }
    # continuous correlation works directly off scores + the raw column, no
    # sample aggregation involved; every other branch aggregates by sample
    # (the pseudoreplication fix) or is descriptive over samples themselves
    if (column_type != 'continuous' && !has_capability(ctx$ms, 'sample_ids')) {
        message('module_by_metadata: skipped, module set lacks the sample_ids capability required for ', column_type, ' testing')
        return(NULL)
    }

    scores <- module_scores(ctx$ms, module = ctx$module_id)
    meta <- metadata(ctx$ms)
    meta_col <- meta[[column]]
    if (is.null(meta_col)) stop('metadata column not found: ', column)

    keep <- !is.na(meta_col)
    scores <- scores[keep]
    meta_col <- meta_col[keep]
    sample_id <- meta[[sample_col]][keep]

    if (column == sample_col) {
        if (is.null(sample_id)) stop('sample_col not found: ', sample_col)
        result <- aggregate_by_sample(scores, sample_id)
        result <- result[order(-result$mean_score), ]
        rownames(result) <- NULL
        top <- result[1, ]

        top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
            list(sample = result$sample[i], mean_score = result$mean_score[i])
        })
        compact_summary <- paste0(
            column, ': per-sample mean ME across ', nrow(result), ' samples; ',
            'highest in ', top$sample, ' (mean=', round(top$mean_score, 2), ')'
        )
        effect_strength <- stats::sd(result$mean_score)
        significance <- NA_real_
        direction <- 'na'
        type <- 'categorical_association'
        level <- 'sample'
        n_units <- nrow(result)
    } else if (column_type == 'categorical') {
        if (is.null(sample_id)) stop('sample_col not found: ', sample_col)
        level <- level_param
        if (level == 'auto') level <- if (is_sample_constant(meta_col, sample_id)) 'sample' else 'cell'

        if (level == 'sample') {
            agg <- aggregate_by_sample(scores, sample_id, group_col = meta_col)
            test <- categorical_group_test(agg$mean_score, agg$group)
            n_units <- nrow(agg)
        } else {
            test <- categorical_group_test(scores, meta_col)
            n_units <- length(scores)
        }

        result <- test$table
        top <- result[1, ]

        top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
            list(
                group = result$group[i], mean_score = result$mean_score[i],
                rank_biserial = result$rank_biserial[i], fdr = result$fdr[i]
            )
        })
        compact_summary <- paste0(
            column, ' (', level, '-level): strongest group ', top$group,
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
        level <- 'cell'
        n_units <- length(scores)
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
            tool_version = '0.2',
            params = list(
                column = column, column_type = column_type, sample_col = sample_col,
                level = level, n_units = n_units
            ),
            pkg_versions = pkg_versions(ctx$ms),
            module_method = ctx$module_method %||% NA_character_
        )
    )
}
