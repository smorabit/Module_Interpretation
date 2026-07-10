## run once for the whole test_dir() session (testthat 3e convention).
## wd here is tests/testthat/, hence the ../../ paths.

source('../../R/utils.R')
source('../../R/moduleset.R')
source('../../R/moduleset_hdwgcna.R')
source('../../R/fragment.R')
source('../../R/stats_utils.R')
source('../../R/tool_hub_genes.R')
source('../../R/tool_cluster_dme.R')
source('../../R/tool_module_by_metadata.R')
source('../../R/tool_geneset_enrichment.R')
source('../../R/orchestrator.R')

# not named helper-*.R on purpose: testthat's automatic helper loader sources
# helpers into a private environment that tool functions (bound to .GlobalEnv
# by the source() calls above) can't see for S3 dispatch, so it's sourced
# explicitly here instead, same as everything else in this file
source('synthetic_moduleset.R')

# loaded once and shared read-only across test files; readRDS + adapter
# construction is the slow part of this suite, no need to repeat it per file
so_test <- readRDS('../../data/CSF_Myeloid_hdWGCNA.rds')
ms_test <- hdWGCNA_ModuleSet(so_test)
mod_test <- modules(ms_test)[1]
