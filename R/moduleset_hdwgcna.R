## hdWGCNA_ModuleSet: ModuleSet adapter backed by a Seurat object with an
## hdWGCNA experiment attached. This is the ONLY file allowed to call
## hdWGCNA / Seurat directly (docs/CLAUDE.md non-negotiable). gene_membership()
## and module_scores() delegate to a components_ModuleSet built from the
## Seurat object's own tables (.hdwgcna_components()) -- the hdWGCNA-specific
## work is just reshaping GetModules()'s wide kME_<module> columns into the
## generic long module/gene_name/weight shape.

#' Build a ModuleSet from a Seurat object with an hdWGCNA experiment
#'
#' The only `ModuleSet` adapter that touches Seurat/hdWGCNA directly; every
#' core evidence tool depends only on the generic `ModuleSet` contract
#' ([modules()], [gene_membership()], [module_scores()], [expression()],
#' [counts()], [metadata()], [pkg_versions()]), never on this backend.
#'
#' @param seurat_obj A `Seurat` object with an hdWGCNA experiment attached
#'   (i.e. run through the hdWGCNA pipeline).
#' @param wgcna_name Name of the hdWGCNA experiment to read from. Defaults to
#'   whichever experiment is currently active on `seurat_obj`.
#' @param data_level Observation-unit descriptor, e.g. `'cell'` or `'sample'`.
#'   Default `'cell'`.
#' @param aggregated Whether `expression()`/`module_scores()` are already
#'   aggregated across cells (e.g. pseudobulk) rather than per-cell. Default `FALSE`.
#' @param ms A `ModuleSet` object; the dispatch target for the generic
#'   methods below ([modules()], [gene_membership()], [module_scores()],
#'   [expression()], [counts()], [metadata()], [pkg_versions()], [capabilities()]).
#' @param module A single module id, as returned by [modules()].
#' @param ... Passed to methods.
#' @return An `hdWGCNA_ModuleSet` object.
#' @examples
#' \dontrun{
#' library(Seurat)
#' seurat_obj <- readRDS('my_hdwgcna_object.rds')
#' ms <- hdWGCNA_ModuleSet(seurat_obj)
#' modules(ms)
#' }
#' @export
hdWGCNA_ModuleSet <- function(seurat_obj, wgcna_name = NULL, data_level = 'cell', aggregated = FALSE){
    if (is.null(wgcna_name)) wgcna_name <- seurat_obj@misc$active_wgcna
    structure(
        list(
            seurat_obj = seurat_obj, wgcna_name = wgcna_name,
            data_level = data_level, aggregated = aggregated
        ),
        class = 'hdWGCNA_ModuleSet'
    )
}

#' @rdname hdWGCNA_ModuleSet
#' @param include_grey Include hdWGCNA's 'grey' bucket for unassigned genes
#'   (not a real co-expression module). Default `FALSE`.
#' @export
modules.hdWGCNA_ModuleSet <- function(ms, include_grey = FALSE, ...){
    mod_df <- hdWGCNA::GetModules(ms$seurat_obj, wgcna_name = ms$wgcna_name)
    mod_ids <- unique(as.character(mod_df$module))
    if (!include_grey) mod_ids <- setdiff(mod_ids, 'grey')
    mod_ids
}

# each gene's weight is its OWN module's kME column (kME_<module>) -- the
# same subset gene_membership() filtered to before this refactor, just
# reshaped once as a long table instead of re-filtered per module. Matrix
# indexing is done on a pure-numeric kME submatrix (not the mixed-type
# mod_df) so values stay numeric rather than being coerced to character.
.hdwgcna_gene_table <- function(seurat_obj, wgcna_name){
    mod_df <- hdWGCNA::GetModules(seurat_obj, wgcna_name = wgcna_name)
    mod_df <- mod_df[mod_df$module != 'grey', , drop = FALSE]

    kme_col_names <- grep('^kME_', colnames(mod_df), value = TRUE)
    kme_mat <- as.matrix(mod_df[, kme_col_names, drop = FALSE])
    col_idx <- match(paste0('kME_', as.character(mod_df$module)), colnames(kme_mat))
    if (anyNA(col_idx)) {
        missing_mods <- unique(as.character(mod_df$module)[is.na(col_idx)])
        stop('no kME column for module(s): ', paste(missing_mods, collapse = ', '))
    }
    weight <- kme_mat[cbind(seq_len(nrow(kme_mat)), col_idx)]

    data.frame(module = as.character(mod_df$module), gene_name = mod_df$gene_name, weight = weight)
}

.hdwgcna_components <- function(ms){
    components_ModuleSet(
        gene_table = .hdwgcna_gene_table(ms$seurat_obj, ms$wgcna_name),
        expression = Seurat::GetAssayData(ms$seurat_obj, assay = Seurat::DefaultAssay(ms$seurat_obj), layer = 'data'),
        metadata = ms$seurat_obj@meta.data,
        scores = hdWGCNA::GetMEs(ms$seurat_obj, wgcna_name = ms$wgcna_name)
    )
}

#' @rdname hdWGCNA_ModuleSet
#' @export
gene_membership.hdWGCNA_ModuleSet <- function(ms, module, ...){
    gene_membership(.hdwgcna_components(ms), module, ...)
}

#' @rdname hdWGCNA_ModuleSet
#' @export
module_scores.hdWGCNA_ModuleSet <- function(ms, module = NULL, ...){
    module_scores(.hdwgcna_components(ms), module = module, ...)
}

#' @rdname hdWGCNA_ModuleSet
#' @export
expression.hdWGCNA_ModuleSet <- function(ms, ...){
    Seurat::GetAssayData(ms$seurat_obj, assay = Seurat::DefaultAssay(ms$seurat_obj), layer = 'data')
}

#' @rdname hdWGCNA_ModuleSet
#' @export
counts.hdWGCNA_ModuleSet <- function(ms, ...){
    Seurat::GetAssayData(ms$seurat_obj, assay = Seurat::DefaultAssay(ms$seurat_obj), layer = 'counts')
}

#' @rdname hdWGCNA_ModuleSet
#' @export
metadata.hdWGCNA_ModuleSet <- function(ms, ...){
    ms$seurat_obj@meta.data
}

#' @rdname hdWGCNA_ModuleSet
#' @export
pkg_versions.hdWGCNA_ModuleSet <- function(ms, ...){
    list(
        hdWGCNA = as.character(utils::packageVersion('hdWGCNA')),
        Seurat = as.character(utils::packageVersion('Seurat')),
        WGCNA = as.character(utils::packageVersion('WGCNA'))
    )
}

# a Seurat object with an hdWGCNA experiment always carries full metadata,
# computed MEs, kME weights, and the assay matrix -- unlike the generic
# components/gene-list adapters, none of these are ever optional here
#' @rdname hdWGCNA_ModuleSet
#' @export
capabilities.hdWGCNA_ModuleSet <- function(ms, ...){
    c(
        gene_weights = TRUE, module_scores = TRUE, expression = TRUE, counts = TRUE,
        grouping = TRUE, sample_ids = TRUE, pseudobulk = FALSE
    )
}
