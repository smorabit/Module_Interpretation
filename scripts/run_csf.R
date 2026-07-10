## entry point: run the deterministic evidence core end-to-end on the CSF dev
## object. Run from the repo root:
##   conda activate hdWGCNA
##   Rscript scripts/run_csf.R

source('R/utils.R')
source('R/moduleset.R')
source('R/moduleset_hdwgcna.R')
source('R/fragment.R')
source('R/stats_utils.R')
source('R/tool_hub_genes.R')
source('R/tool_cluster_dme.R')
source('R/tool_module_by_metadata.R')
source('R/tool_geneset_enrichment.R')
source('R/orchestrator.R')

data_path <- 'data/CSF_Myeloid_hdWGCNA.rds'
output_dir <- 'output/evidence_packets'

# core tools for the CSF dataset (docs/milestone_1.md): hub genes, which cell
# state expresses the module (lv2_annot), association with diagnosis and
# sample, and GO enrichment over hub genes. geneset_enrichment needs network
# access (Enrichr); it degrades to a low-evidence fragment if unreachable.
tool_config <- list(
    list(fn = hub_genes_tool, params = list(n_hubs = 25)),
    list(fn = cluster_dme_tool, params = list(group_by = 'lv2_annot')),
    list(fn = module_by_metadata_tool, params = list(column = 'diagnosis', column_type = 'categorical')),
    list(fn = module_by_metadata_tool, params = list(column = 'sample', column_type = 'categorical')),
    list(fn = geneset_enrichment_tool, params = list(n_hubs = 25))
)

seurat_obj <- readRDS(data_path)
ms <- hdWGCNA_ModuleSet(seurat_obj)
input_hash <- digest::digest(file = data_path, algo = 'sha256')

packets <- run_orchestrator(ms, tool_config, output_dir, input_hash = input_hash)

n_ok <- sum(!vapply(packets, is.null, logical(1)))
cat(n_ok, '/', length(packets), 'modules written to', output_dir, '\n')
