## synthetic_ModuleSet: a test-only ModuleSet that wraps a real backend (any
## ModuleSet) and swaps in hand-picked gene sets as fake "modules". Delegates
## expression()/metadata()/pkg_versions() to the wrapped ModuleSet, so it never
## touches Seurat/hdWGCNA directly and exercises the adapter pattern itself:
## every core tool that only calls the ModuleSet generics works unmodified
## against ground-truth gene sets, which is what the spike-in tests need.
##
## module score = mean z-scored expression across the gene set, per cell
## (a simple, backend-agnostic stand-in for a real module eigengene).
## kme = per-gene correlation with that score, so gene_membership() ranks
## genes the same way a real hdWGCNA kME column would.

synthetic_ModuleSet <- function(base_ms, gene_sets){
    structure(list(base_ms = base_ms, gene_sets = gene_sets), class = 'synthetic_ModuleSet')
}

# a gene with zero variance in this cell population (never expressed, or
# constant) carries no signal: scale() would turn it into a column of NaN,
# and its correlation with anything is undefined
.expressed_genes <- function(expr, genes){
    genes <- intersect(genes, rownames(expr))
    sub <- as.matrix(expr[genes, , drop = FALSE])
    genes[apply(sub, 1, sd) > 0]
}

modules.synthetic_ModuleSet <- function(ms, ...) names(ms$gene_sets)

module_scores.synthetic_ModuleSet <- function(ms, module = NULL, ...){
    expr <- expression(ms$base_ms)
    scores <- lapply(ms$gene_sets, function(genes){
        genes <- .expressed_genes(expr, genes)
        sub <- as.matrix(expr[genes, , drop = FALSE])
        # scale() z-scores each column (gene) across cells; rowMeans then
        # averages across genes for each cell (columns of scale() output sum
        # to ~0 by construction, so colMeans here would be a no-op bug)
        rowMeans(scale(t(sub)))
    })
    scores_df <- as.data.frame(scores)
    if (!is.null(module)) return(scores_df[[module]])
    scores_df
}

gene_membership.synthetic_ModuleSet <- function(ms, module, ...){
    genes <- ms$gene_sets[[module]]
    if (is.null(genes)) stop('unknown synthetic module: ', module)
    expr <- expression(ms$base_ms)
    genes <- .expressed_genes(expr, genes)
    score <- module_scores(ms, module = module)
    kme <- vapply(genes, function(g) cor(as.numeric(expr[g, ]), score), numeric(1))
    df <- data.frame(gene_name = genes, module = module, kme = unname(kme))
    df[order(-df$kme), ]
}

expression.synthetic_ModuleSet <- function(ms, ...) expression(ms$base_ms)
metadata.synthetic_ModuleSet <- function(ms, ...) metadata(ms$base_ms)
pkg_versions.synthetic_ModuleSet <- function(ms, ...) pkg_versions(ms$base_ms)

# positive control: canonical pDC lineage markers, all present in the CSF
# object. Raw expression check (not just the synthetic score) confirms these
# separate cleanly: mean expression in the 'pDC' cluster is ~2.6, vs. ~0.5 in
# the next-highest cluster (DC ITGAX) — a clean, unambiguous ground truth.
# (an earlier draft used interferon-stimulated genes against the
# 'Macrphages IFN producing' cluster, but raw expression showed ISGs actually
# peak in Monocytes classical, not that cluster — the label reflects IFN
# *production*, not ISG *response*, so it was the wrong ground truth, not a
# tool bug. pDC markers avoid that ambiguity.)
pdc_genes <- c(
    'LILRA4', 'CLEC4C', 'GZMB', 'JCHAIN', 'MZB1', 'SPIB', 'IRF7', 'TCF4', 'IL3RA', 'PLD4'
)

# negative control: a random gene set of matched size, fixed seed for
# reproducibility. Drawn from the full feature space, not from any real module.
random_control_genes <- function(so, n = length(pdc_genes), seed = 1){
    set.seed(seed)
    sample(rownames(so), n)
}
