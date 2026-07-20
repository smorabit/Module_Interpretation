## gene_list_ModuleSet: ModuleSet from named gene lists (no membership
## weights), with module scores computed on the fly via UCell or decoupleR.
## Builds a components_ModuleSet under the hood and inherits its class, so
## modules()/gene_membership()/module_scores()/expression()/metadata()/
## capabilities() all dispatch there unmodified -- only score computation and
## pkg_versions() (to record which scoring backend ran) are specific to this
## adapter. This file has no hdWGCNA/Seurat dependency.

#' Build a ModuleSet from named gene lists, scoring on the fly
#'
#' Takes named gene lists as the modules (no membership weights -- every gene
#' counts equally) and computes per-cell/sample module scores on the fly via
#' [UCell::ScoreSignatures_UCell()] or [decoupleR::run_ulm()]. Builds on
#' [components_ModuleSet()]; [capabilities()]`$gene_weights` is `FALSE`
#' (there's no kME-equivalent), `module_scores`/`expression` are `TRUE`, and
#' `grouping`/`sample_ids` follow `group_col`/`sample_col` as usual.
#'
#' @param gene_sets A named list of character vectors, one per module, e.g.
#'   `list(module_a = c('GENE1', 'GENE2'))`. Genes not present in `expression`
#'   are dropped.
#' @param expression A genes-by-cells (or genes-by-samples) numeric matrix.
#' @param metadata A data.frame with one row per cell/sample, aligned to the
#'   columns of `expression`.
#' @param counts Optional genes-by-cells (or genes-by-samples) raw counts
#'   matrix, same dimensions as `expression`. If omitted, [counts()] returns
#'   `NULL` and [capabilities()]`$counts` is `FALSE`.
#' @param group_col Optional name of a `metadata` column declared as the
#'   cell/sample-state grouping.
#' @param sample_col Optional name of a `metadata` column declared as the
#'   sample id.
#' @param method Scoring method: `'UCell'` (default,
#'   [UCell::ScoreSignatures_UCell()]) or `'decoupleR'`
#'   ([decoupleR::run_ulm()] over a network built from `gene_sets` with a
#'   uniform mode-of-regulation, since these gene lists carry no signed
#'   weights).
#' @param data_level Observation-unit descriptor, e.g. `'cell'` or `'sample'`.
#'   Default `'cell'`.
#' @param aggregated Whether `expression` is already aggregated across cells
#'   (e.g. pseudobulk) rather than per-cell. Default `FALSE`.
#' @param ... Passed through to the scoring backend
#'   (`UCell::ScoreSignatures_UCell()`'s `...`, or `decoupleR::run_ulm()`'s
#'   `minsize` etc.).
#' @param ms A `ModuleSet` object; the dispatch target for [pkg_versions()].
#' @return A `gene_list_ModuleSet` object (a `components_ModuleSet` under the hood).
#' @examples
#' expr <- matrix(abs(rnorm(40)), nrow = 4, dimnames = list(paste0('G', 1:4), paste0('c', 1:10)))
#' meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))
#' ms <- gene_list_ModuleSet(list(m1 = c('G1', 'G2')), expr, meta, group_col = 'cell_type')
#' modules(ms)
#' @export
gene_list_ModuleSet <- function(gene_sets, expression, metadata, counts = NULL, group_col = NULL,
                                 sample_col = NULL, method = c('UCell', 'decoupleR'),
                                 data_level = 'cell', aggregated = FALSE, ...){
    method <- match.arg(method)

    gene_table <- do.call(rbind, lapply(names(gene_sets), function(m){
        genes <- intersect(gene_sets[[m]], rownames(expression))
        data.frame(module = m, gene_name = genes)
    }))
    scores <- .score_gene_sets(gene_sets, expression, method = method, ...)

    ms <- components_ModuleSet(
        gene_table, expression, metadata, scores = scores, counts = counts,
        group_col = group_col, sample_col = sample_col,
        data_level = data_level, aggregated = aggregated
    )
    ms$method <- method
    class(ms) <- c('gene_list_ModuleSet', class(ms))
    ms
}

# cells x modules score matrix, matching module_scores()'s documented shape
# and row order (aligned to colnames(expression))
.score_gene_sets <- function(gene_sets, expression, method, ...){
    expr_mat <- as.matrix(expression)
    if (method == 'UCell') {
        raw <- UCell::ScoreSignatures_UCell(matrix = expr_mat, features = gene_sets, ...)
        colnames(raw) <- sub('_UCell$', '', colnames(raw))
        as.data.frame(raw)[colnames(expr_mat), , drop = FALSE]
    } else {
        # decoupleR networks carry a signed mode-of-regulation ('mor'); these
        # gene lists have no such signal, so every gene gets a uniform mor = 1
        network <- do.call(rbind, lapply(names(gene_sets), function(m){
            data.frame(source = m, target = gene_sets[[m]], mor = 1)
        }))
        long <- decoupleR::run_ulm(expr_mat, network, ...)
        wide <- as.data.frame(tidyr::pivot_wider(
            long, id_cols = 'condition', names_from = 'source', values_from = 'score'
        ))
        rownames(wide) <- wide$condition
        wide$condition <- NULL
        wide[colnames(expr_mat), , drop = FALSE]
    }
}

#' @rdname gene_list_ModuleSet
#' @export
pkg_versions.gene_list_ModuleSet <- function(ms, ...){
    versions <- list(llegir = as.character(utils::packageVersion('llegir')))
    versions[[ms$method]] <- as.character(utils::packageVersion(ms$method))
    versions
}
