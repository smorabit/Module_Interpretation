## hdWGCNA_ModuleSet: ModuleSet adapter backed by a Seurat object with an
## hdWGCNA experiment attached. This is the ONLY file allowed to call
## hdWGCNA / Seurat directly (docs/CLAUDE.md non-negotiable).

suppressPackageStartupMessages({
    library(Seurat)
    library(hdWGCNA)
    library(dplyr)
})

# wraps a Seurat object + the name of the hdWGCNA experiment to read from;
# defaults to whichever experiment is currently active on the object
hdWGCNA_ModuleSet <- function(seurat_obj, wgcna_name = NULL){
    if (is.null(wgcna_name)) wgcna_name <- seurat_obj@misc$active_wgcna
    structure(
        list(seurat_obj = seurat_obj, wgcna_name = wgcna_name),
        class = 'hdWGCNA_ModuleSet'
    )
}

# 'grey' is hdWGCNA's bucket for unassigned genes, not a real co-expression module,
# so it's excluded by default
modules.hdWGCNA_ModuleSet <- function(ms, include_grey = FALSE, ...){
    mod_df <- GetModules(ms$seurat_obj, wgcna_name = ms$wgcna_name)
    mod_ids <- unique(as.character(mod_df$module))
    if (!include_grey) mod_ids <- setdiff(mod_ids, 'grey')
    mod_ids
}

# hard-assigned genes for `module`, ranked by that module's own kME column
# (GetModules() has one kME_<module> column per module; hub gene tables key off this)
gene_membership.hdWGCNA_ModuleSet <- function(ms, module, ...){
    mod_df <- GetModules(ms$seurat_obj, wgcna_name = ms$wgcna_name)
    kme_col <- paste0('kME_', module)
    if (!(kme_col %in% colnames(mod_df))) {
        stop('no kME column for module: ', module)
    }
    mod_df %>%
        filter(module == !!module) %>%
        transmute(gene_name = gene_name, module = module, kme = .data[[kme_col]]) %>%
        arrange(desc(kme))
}

# module eigengenes (GetMEs); pass `module` to pull a single column as a vector
module_scores.hdWGCNA_ModuleSet <- function(ms, module = NULL, ...){
    mes <- GetMEs(ms$seurat_obj, wgcna_name = ms$wgcna_name)
    if (!is.null(module)) return(mes[, module])
    mes
}

# normalized expression ('data' layer) for the object's default assay
expression.hdWGCNA_ModuleSet <- function(ms, ...){
    Seurat::GetAssayData(ms$seurat_obj, assay = DefaultAssay(ms$seurat_obj), layer = 'data')
}

metadata.hdWGCNA_ModuleSet <- function(ms, ...){
    ms$seurat_obj@meta.data
}

pkg_versions.hdWGCNA_ModuleSet <- function(ms, ...){
    list(
        hdWGCNA = as.character(utils::packageVersion('hdWGCNA')),
        Seurat = as.character(utils::packageVersion('Seurat')),
        WGCNA = as.character(utils::packageVersion('WGCNA'))
    )
}
