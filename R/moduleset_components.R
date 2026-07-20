## components_ModuleSet: ModuleSet built directly from tidy components -- a
## module<->gene table, an optional module-scores matrix, an expression
## matrix, and metadata. No backend dependency (no hdWGCNA/Seurat); this is
## the general substrate other adapters build on by extracting their own data
## into these same shapes and delegating. docs/milestone_extensibility.md

#' Build a ModuleSet from tidy components
#'
#' The general-purpose `ModuleSet` substrate: takes a module<->gene table, an
#' optional module-scores matrix, an expression matrix, and metadata
#' directly, with no backend dependency. Other adapters (e.g.
#' [hdWGCNA_ModuleSet()], [gene_list_ModuleSet()]) build on this by
#' extracting their own data into these same shapes and delegating.
#'
#' @param gene_table A data.frame with one row per module-gene assignment:
#'   `module`, `gene_name`, and an optional numeric `weight` column (e.g.
#'   kME). If `weight` is absent, [gene_membership()] reports `kme =
#'   NA_real_` for every gene and [capabilities()]`$gene_weights` is `FALSE`.
#' @param expression A genes-by-cells (or genes-by-samples) numeric matrix.
#' @param metadata A data.frame with one row per cell/sample, aligned to the
#'   columns of `expression`.
#' @param scores Optional module-scores data.frame/matrix: one row per
#'   cell/sample (aligned to `expression`'s columns, same order), one column
#'   per module -- the same shape [module_scores()] documents. If omitted,
#'   [module_scores()] returns `NULL` and [capabilities()]`$module_scores` is
#'   `FALSE`.
#' @param counts Optional genes-by-cells (or genes-by-samples) raw counts
#'   matrix, same dimensions as `expression`. If omitted, [counts()] returns
#'   `NULL` and [capabilities()]`$counts` is `FALSE`.
#' @param group_col Optional name of a `metadata` column declared as the
#'   cell/sample-state grouping. This only drives
#'   [capabilities()]`$grouping` -- core tools still take the grouping column
#'   name as a parameter (e.g. `cluster_dme_tool`'s `group_by`); declaring
#'   `group_col` is how this ModuleSet advertises that it supports the
#'   concept at all.
#' @param sample_col Optional name of a `metadata` column declared as the
#'   sample id, analogous to `group_col` for [capabilities()]`$sample_ids`.
#' @param data_level Observation-unit descriptor, e.g. `'cell'` or `'sample'`.
#'   Default `'cell'`.
#' @param aggregated Whether `expression`/`scores` are already aggregated
#'   across cells (e.g. pseudobulk) rather than per-cell. Default `FALSE`.
#' @param ms A `ModuleSet` object; the dispatch target for the generic
#'   methods below ([modules()], [gene_membership()], [module_scores()],
#'   [expression()], [counts()], [metadata()], [pkg_versions()], [capabilities()]).
#' @param module A single module id, as returned by [modules()].
#' @param ... Passed to methods.
#' @return A `components_ModuleSet` object.
#' @examples
#' gene_table <- data.frame(module = 'm1', gene_name = c('G1', 'G2'), weight = c(0.9, 0.5))
#' expr <- matrix(rnorm(20), nrow = 2, dimnames = list(c('G1', 'G2'), paste0('c', 1:10)))
#' meta <- data.frame(cell_type = rep(c('a', 'b'), 5), row.names = colnames(expr))
#' ms <- components_ModuleSet(gene_table, expr, meta, group_col = 'cell_type')
#' modules(ms)
#' @export
components_ModuleSet <- function(gene_table, expression, metadata, scores = NULL, counts = NULL,
                                  group_col = NULL, sample_col = NULL,
                                  data_level = 'cell', aggregated = FALSE){
    if (!all(c('module', 'gene_name') %in% colnames(gene_table))) {
        stop("gene_table must have 'module' and 'gene_name' columns")
    }
    if (ncol(expression) != nrow(metadata)) {
        stop('expression columns and metadata rows must align (', ncol(expression), ' vs ', nrow(metadata), ')')
    }
    if (!is.null(scores) && nrow(scores) != ncol(expression)) {
        stop('scores rows must align with expression columns (', nrow(scores), ' vs ', ncol(expression), ')')
    }
    if (!is.null(counts) && !identical(dim(counts), dim(expression))) {
        stop('counts dimensions must match expression (', paste(dim(counts), collapse = 'x'),
             ' vs ', paste(dim(expression), collapse = 'x'), ')')
    }
    if (!is.null(group_col) && !(group_col %in% colnames(metadata))) {
        stop('group_col not found in metadata: ', group_col)
    }
    if (!is.null(sample_col) && !(sample_col %in% colnames(metadata))) {
        stop('sample_col not found in metadata: ', sample_col)
    }

    has_weight <- 'weight' %in% colnames(gene_table)
    if (!has_weight) gene_table$weight <- NA_real_

    structure(
        list(
            gene_table = gene_table, expression = expression, metadata = metadata,
            scores = if (is.null(scores)) NULL else as.data.frame(scores),
            counts = counts,
            has_weight = has_weight, group_col = group_col, sample_col = sample_col,
            data_level = data_level, aggregated = aggregated
        ),
        class = 'components_ModuleSet'
    )
}

#' @rdname components_ModuleSet
#' @export
modules.components_ModuleSet <- function(ms, ...) unique(as.character(ms$gene_table$module))

#' @rdname components_ModuleSet
#' @export
gene_membership.components_ModuleSet <- function(ms, module, ...){
    sub <- ms$gene_table[ms$gene_table$module == module, , drop = FALSE]
    if (nrow(sub) == 0) stop('unknown module: ', module)
    df <- data.frame(gene_name = sub$gene_name, module = sub$module, kme = sub$weight)
    # stable sort: ties (including the all-NA case when there's no real weight) keep table order
    df[order(-df$kme), ]
}

#' @rdname components_ModuleSet
#' @export
module_scores.components_ModuleSet <- function(ms, module = NULL, ...){
    if (is.null(ms$scores)) return(NULL)
    if (!is.null(module)) return(ms$scores[[module]])
    ms$scores
}

#' @rdname components_ModuleSet
#' @export
expression.components_ModuleSet <- function(ms, ...) ms$expression

#' @rdname components_ModuleSet
#' @export
counts.components_ModuleSet <- function(ms, ...) ms$counts

#' @rdname components_ModuleSet
#' @export
metadata.components_ModuleSet <- function(ms, ...) ms$metadata

#' @rdname components_ModuleSet
#' @export
pkg_versions.components_ModuleSet <- function(ms, ...){
    list(llegir = as.character(utils::packageVersion('llegir')))
}

#' @rdname components_ModuleSet
#' @export
capabilities.components_ModuleSet <- function(ms, ...){
    c(
        gene_weights = ms$has_weight,
        module_scores = !is.null(ms$scores),
        expression = !is.null(ms$expression),
        counts = !is.null(ms$counts),
        grouping = !is.null(ms$group_col),
        sample_ids = !is.null(ms$sample_col),
        pseudobulk = FALSE
    )
}
