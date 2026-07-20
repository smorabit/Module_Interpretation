## interactive dev script: source/run line-by-line in an R session to prove
## the abstract ModuleSet contract (docs/milestone_abstract_moduleset.md Part 4)
## works for a module-GENERATING method other than hdWGCNA co-expression --
## here, a mock NMF/cNMF factorization built entirely from base R matrix ops,
## no external NMF package involved. conda activate hdWGCNA, then open this
## file and run it chunk by chunk.

devtools::load_all()

set.seed(1)

#---------------------------------------------------------
# 1. mock normalized expression matrix + cell metadata
#---------------------------------------------------------

n_genes <- 30
n_cells <- 100
gene_names <- paste0('GENE', seq_len(n_genes))
cell_names <- paste0('cell', seq_len(n_cells))

# a background of low-level noise expression, genes x cells -- the mock NMF
# signal (step 2) gets added on top of this, the same way a real factor's
# reconstruction sits on top of background transcriptional noise
expr <- matrix(
    abs(stats::rnorm(n_genes * n_cells, mean = 0.2, sd = 0.1)),
    nrow = n_genes, ncol = n_cells, dimnames = list(gene_names, cell_names)
)

# three simulated cell states, one per dominant factor below -- gives cluster_dme_tool
# and module_by_metadata_tool a real grouping column to test against, not just noise
cell_state <- sample(c('state_a', 'state_b', 'state_c'), n_cells, replace = TRUE)
sample_id <- sample(paste0('sample', 1:4), n_cells, replace = TRUE)
meta <- data.frame(cell_state = cell_state, sample = sample_id, row.names = cell_names)

#---------------------------------------------------------
# 2. mock NMF loading + usage matrices (3 factors, pure base R)
#---------------------------------------------------------

n_factors <- 3
factor_names <- paste0('Fact', seq_len(n_factors))

# loadings: factors x genes, non-negative like a real NMF gene-spectra matrix.
# each factor gets 10 "characteristic" genes with a strong loading and every
# other gene a small background loading, so hub_genes_tool has real signal
# to rank instead of ties
loadings <- matrix(
    abs(stats::rnorm(n_factors * n_genes, mean = 0.05, sd = 0.02)),
    nrow = n_factors, ncol = n_genes, dimnames = list(factor_names, gene_names)
)
for (f in seq_len(n_factors)) {
    characteristic_genes <- ((f - 1) * 10 + 1):(f * 10)
    loadings[f, characteristic_genes] <- abs(stats::rnorm(10, mean = 1.5, sd = 0.2))
}

# usage: factors x cells, non-negative like a real NMF cell-usage matrix.
# cells in state_a/b/c get elevated usage of Fact1/Fact2/Fact3 respectively,
# so the factor a cell is "high in" lines up with its simulated cell state
usage <- matrix(
    abs(stats::rnorm(n_factors * n_cells, mean = 0.1, sd = 0.05)),
    nrow = n_factors, ncol = n_cells, dimnames = list(factor_names, cell_names)
)
state_of_factor <- c(Fact1 = 'state_a', Fact2 = 'state_b', Fact3 = 'state_c')
for (f in factor_names) {
    dominant_cells <- cell_state == state_of_factor[[f]]
    usage[f, dominant_cells] <- abs(stats::rnorm(sum(dominant_cells), mean = 2, sd = 0.3))
}

# reconstruct each factor's contribution to expression (t(loadings) %*% usage
# is genes x cells, the classic NMF V ~= W H shape) and add it on top of the
# background noise -- this is what makes hub_genes_tool's top genes per
# factor actually correlate with that factor's usage, same as a real cNMF run
expr <- expr + t(loadings) %*% usage

#---------------------------------------------------------
# 3. tidy gene_table: module_id / gene_name / weight, top loading genes only
#---------------------------------------------------------

# one data.frame per factor: its own row of `loadings`, ranked by weight and
# capped to the top 10 genes -- mirrors how gene_membership() ranks by kME
# for a real hdWGCNA_ModuleSet, just built by hand here from the loading matrix
gene_table <- do.call(rbind, lapply(factor_names, function(f){
    weights <- loadings[f, ]
    ranked <- sort(weights, decreasing = TRUE)
    top <- utils::head(ranked, 10)
    data.frame(module_id = f, gene_name = names(top), weight = unname(top))
}))
# components_ModuleSet()'s gene_table contract names the column 'module', not
# 'module_id' -- rename here rather than upstream, so the block above still
# reads as "module_id" the way a cNMF results table naturally would
names(gene_table)[names(gene_table) == 'module_id'] <- 'module'

#---------------------------------------------------------
# 4. build the ModuleSet adapter: components_ModuleSet(), no new backend code
#---------------------------------------------------------

# usage is factors x cells; components_ModuleSet()'s `scores` contract wants
# one row per cell (aligned to expression's columns), one column per module --
# so it needs transposing before it's a valid `scores` argument
usage_scores <- as.data.frame(t(usage))

nmf_ms <- components_ModuleSet(
    gene_table = gene_table,
    expression = expr,
    metadata = meta,
    scores = usage_scores,
    group_col = 'cell_state',
    sample_col = 'sample',
    data_level = 'cell',
    aggregated = FALSE
)

modules(nmf_ms)
gene_membership(nmf_ms, 'Fact1')
capabilities(nmf_ms)

#---------------------------------------------------------
# 5. validate_moduleset(): prove this adapter satisfies the abstract contract
#---------------------------------------------------------

validate_moduleset(nmf_ms)

#---------------------------------------------------------
# 6. wire into run_orchestrator() with two native tools, end to end
#---------------------------------------------------------

# module_method documents how these "modules" were generated -- a factorization,
# not a co-expression network -- and flows into the synthesis prompt, the run
# manifest, and every fragment's provenance (docs/milestone_abstract_moduleset.md Part 4)
desc <- dataset_description(
    species = 'human',
    tissue = 'mock tissue',
    cell_compartment = 'mock cell population',
    assay = 'mock scRNA-seq',
    module_method = 'cNMF factors, k=3'
)

tool_config <- list(
    list(fn = hub_genes_tool, params = list(n_hubs = 10)),
    list(fn = cluster_dme_tool, params = list(group_by = 'cell_state'))
)

packets <- run_orchestrator(
    nmf_ms, tool_config, output_dir = tempfile(),
    module_method = desc$module_method
)

# inspect Fact1's packet: hub_genes should surface exactly the 10 genes this
# script gave Fact1 a strong loading for, and cluster_dme should call state_a
# the strongest cell_state (Fact1's usage was boosted in state_a cells)
packets[['Fact1']]$fragments[[1]]$compact_summary
packets[['Fact1']]$fragments[[2]]$compact_summary

# confirm module_method made it all the way down to fragment provenance
packets[['Fact1']]$fragments[[1]]$provenance$module_method

# ... and into the rendered synthesis prompt, alongside data_level/aggregated
cat(render_dataset_description(desc, data_level = nmf_ms$data_level, aggregated = nmf_ms$aggregated))
