## synthetic_ModuleSet: a ModuleSet that wraps any other ModuleSet (real or
## synthetic) and swaps in hand-picked gene sets as fake "modules". Delegates
## expression()/metadata()/pkg_versions() to the wrapped ModuleSet, so it
## never touches Seurat/hdWGCNA directly and exercises the adapter pattern
## itself: every core tool that only calls the ModuleSet generics works
## unmodified against ground-truth gene sets. Originally a spike-in-only test
## helper (docs/milestone_1.md task 5); exposed here (docs/milestone_packaging.md
## task 4) as the backend for llegir_example_moduleset(), the self-contained
## fixture used across @examples, tests, and the vignette.
##
## module score = mean z-scored expression across the gene set, per cell
## (a simple, backend-agnostic stand-in for a real module eigengene).
## kme = per-gene correlation with that score, so gene_membership() ranks
## genes the same way a real hdWGCNA kME column would.

# a gene with zero variance in this cell population (never expressed, or
# constant) carries no signal: scale() would turn it into a column of NaN,
# and its correlation with anything is undefined
.expressed_genes <- function(expr, genes){
    genes <- intersect(genes, rownames(expr))
    sub <- as.matrix(expr[genes, , drop = FALSE])
    genes[apply(sub, 1, stats::sd) > 0]
}

#' Wrap a ModuleSet with hand-picked gene sets as synthetic modules
#'
#' Wraps `base_ms` (any `ModuleSet`) and swaps in `gene_sets` as fake
#' "modules", so ground-truth gene sets can be run through the same core
#' tools as a real module. Delegates [expression()], [metadata()], and
#' [pkg_versions()] to `base_ms`; module score is the mean z-scored
#' expression across each gene set per cell, and `kme` (returned by
#' [gene_membership()]) is each gene's correlation with that score --
#' a simple, backend-agnostic stand-in for a real module eigengene / kME.
#'
#' @param base_ms A `ModuleSet` to delegate [expression()], [metadata()], and
#'   [pkg_versions()] to.
#' @param gene_sets A named list of character vectors, one per synthetic
#'   module, e.g. `list(module_a = c('GENE1', 'GENE2'))`.
#' @param data_level Observation-unit descriptor, e.g. `'cell'` or `'sample'`.
#'   Default `'cell'`.
#' @param aggregated Whether `expression()` is already aggregated across
#'   cells (e.g. pseudobulk) rather than per-cell. Default `FALSE`.
#' @param ms A `ModuleSet` object; the dispatch target for the generic
#'   methods below ([modules()], [gene_membership()], [module_scores()],
#'   [expression()], [counts()], [metadata()], [pkg_versions()], [capabilities()]).
#' @param module A single module id, as returned by [modules()].
#' @param ... Passed to methods.
#' @return A `synthetic_ModuleSet` object.
#' @examples
#' ms <- llegir_example_moduleset()
#' modules(ms)
#' @export
synthetic_ModuleSet <- function(base_ms, gene_sets, data_level = 'cell', aggregated = FALSE){
    structure(
        list(base_ms = base_ms, gene_sets = gene_sets, data_level = data_level, aggregated = aggregated),
        class = 'synthetic_ModuleSet'
    )
}

#' @rdname synthetic_ModuleSet
#' @export
modules.synthetic_ModuleSet <- function(ms, ...) names(ms$gene_sets)

#' @rdname synthetic_ModuleSet
#' @export
module_scores.synthetic_ModuleSet <- function(ms, module = NULL, ...){
    expr <- expression(ms$base_ms)
    scores <- lapply(ms$gene_sets, function(genes){
        genes <- .expressed_genes(expr, genes)
        sub <- as.matrix(expr[genes, , drop = FALSE])
        rowMeans(scale(t(sub)))
    })
    scores_df <- as.data.frame(scores)
    if (!is.null(module)) return(scores_df[[module]])
    scores_df
}

#' @rdname synthetic_ModuleSet
#' @export
gene_membership.synthetic_ModuleSet <- function(ms, module, ...){
    genes <- ms$gene_sets[[module]]
    if (is.null(genes)) stop('unknown synthetic module: ', module)
    expr <- expression(ms$base_ms)
    genes <- .expressed_genes(expr, genes)
    score <- module_scores(ms, module = module)
    kme <- vapply(genes, function(g) stats::cor(as.numeric(expr[g, ]), score), numeric(1))
    df <- data.frame(gene_name = genes, module = module, kme = unname(kme))
    df[order(-df$kme), ]
}

#' @rdname synthetic_ModuleSet
#' @export
expression.synthetic_ModuleSet <- function(ms, ...) expression(ms$base_ms)

#' @rdname synthetic_ModuleSet
#' @export
counts.synthetic_ModuleSet <- function(ms, ...) NULL

#' @rdname synthetic_ModuleSet
#' @export
metadata.synthetic_ModuleSet <- function(ms, ...) metadata(ms$base_ms)

#' @rdname synthetic_ModuleSet
#' @export
pkg_versions.synthetic_ModuleSet <- function(ms, ...) pkg_versions(ms$base_ms)

#' @rdname synthetic_ModuleSet
#' @export
capabilities.synthetic_ModuleSet <- function(ms, ...){
    # module_scores()/gene_membership() are always computed here from
    # expression(), regardless of what base_ms itself provides, so those two
    # capabilities are always TRUE; expression/grouping/sample_ids genuinely
    # come from base_ms and are delegated; counts() is never computed here
    # (no synthetic notion of raw counts), so it's always FALSE
    base_caps <- capabilities(ms$base_ms)
    c(
        gene_weights = TRUE,
        module_scores = TRUE,
        expression = isTRUE(base_caps[['expression']]),
        counts = FALSE,
        grouping = isTRUE(base_caps[['grouping']]),
        sample_ids = isTRUE(base_caps[['sample_ids']]),
        pseudobulk = FALSE
    )
}

## llegir_example_moduleset(): a fully self-contained fixture (no Seurat/
## hdWGCNA, no external data file) for @examples, tests, and the vignette.
## .example_base_moduleset() simulates a small single-cell-like dataset with
## two hand-built co-expression modules plus background noise genes, so
## gene_membership()/hub-gene ranking carry real signal rather than being
## pure noise; llegir_example_moduleset() wraps it with synthetic_ModuleSet().

# minimal synthetic 'base' ModuleSet: simulated expression matrix + cell
# metadata, nothing else. synthetic_ModuleSet() only ever calls
# expression()/metadata()/pkg_versions() on its wrapped base_ms, so that's
# all this needs to implement.
.example_base_moduleset <- function(seed = 1, n_cells = 200){
    set.seed(seed)

    n_module_genes <- 10
    n_noise_genes <- 20
    module_a_genes <- paste0('GENEA', seq_len(n_module_genes))
    module_b_genes <- paste0('GENEB', seq_len(n_module_genes))
    noise_genes <- paste0('GENEN', seq_len(n_noise_genes))
    all_genes <- c(module_a_genes, module_b_genes, noise_genes)

    diagnosis <- sample(c('case', 'control'), n_cells, replace = TRUE)
    cell_type <- sample(c('myeloid_a', 'myeloid_b'), n_cells, replace = TRUE)
    sample_id <- sample(paste0('sample', 1:4), n_cells, replace = TRUE)

    # module_a is upregulated in 'case', module_b is cell-type structured;
    # both driven by a per-cell latent factor plus gene-specific noise, so
    # genes within a module correlate with each other (real kME signal),
    # unlike the noise genes
    latent_a <- stats::rnorm(n_cells, mean = ifelse(diagnosis == 'case', 1, 0), sd = 0.5)
    latent_b <- stats::rnorm(n_cells, mean = ifelse(cell_type == 'myeloid_a', 1, 0), sd = 0.5)

    module_expr <- function(latent, n_genes){
        t(vapply(seq_len(n_genes), function(i) latent + stats::rnorm(n_cells, sd = 0.3), numeric(n_cells)))
    }

    expr <- rbind(
        module_expr(latent_a, n_module_genes),
        module_expr(latent_b, n_module_genes),
        matrix(stats::rnorm(n_noise_genes * n_cells), nrow = n_noise_genes, ncol = n_cells)
    )
    expr[expr < 0] <- 0
    rownames(expr) <- all_genes
    colnames(expr) <- paste0('cell', seq_len(n_cells))

    meta <- data.frame(
        diagnosis = diagnosis, cell_type = cell_type, sample = sample_id,
        row.names = colnames(expr)
    )

    structure(list(expr = expr, meta = meta), class = 'example_base_ModuleSet')
}

#' @noRd
expression.example_base_ModuleSet <- function(ms, ...) ms$expr
#' @noRd
counts.example_base_ModuleSet <- function(ms, ...) NULL
#' @noRd
metadata.example_base_ModuleSet <- function(ms, ...) ms$meta
#' @noRd
pkg_versions.example_base_ModuleSet <- function(ms, ...){
    list(llegir = as.character(utils::packageVersion('llegir')))
}
#' @noRd
capabilities.example_base_ModuleSet <- function(ms, ...){
    c(
        gene_weights = FALSE, module_scores = FALSE, expression = TRUE, counts = FALSE,
        grouping = TRUE, sample_ids = TRUE, pseudobulk = FALSE
    )
}

#' A small, self-contained synthetic ModuleSet for examples and the vignette
#'
#' Builds a tiny simulated single-cell-like dataset (no Seurat/hdWGCNA
#' involved, no external data file) with two co-expressed gene modules --
#' `'module_a'`, associated with a simulated `diagnosis` column, and
#' `'module_b'`, associated with a simulated `cell_type` column -- plus
#' background noise genes, and wraps it as a `ModuleSet` via
#' [synthetic_ModuleSet()]. Runs fully offline and deterministically (fixed
#' seed), so it is safe to use in `@examples`, tests, and the package
#' vignette without any real dataset.
#'
#' The simulated [metadata()] includes `diagnosis`, `cell_type`, and `sample`
#' columns, matching what [module_by_metadata_tool()] and
#' [cluster_dme_tool()] expect.
#'
#' @param seed Random seed for reproducibility. Default `1`.
#' @return A `synthetic_ModuleSet` object with two modules, `'module_a'` and
#'   `'module_b'`.
#' @examples
#' ms <- llegir_example_moduleset()
#' modules(ms)
#' gene_membership(ms, 'module_a')
#' @export
llegir_example_moduleset <- function(seed = 1){
    base_ms <- .example_base_moduleset(seed = seed)
    genes <- rownames(expression(base_ms))
    synthetic_ModuleSet(base_ms, list(
        module_a = genes[1:10],
        module_b = genes[11:20]
    ))
}
