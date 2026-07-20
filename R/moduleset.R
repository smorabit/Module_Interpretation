## ModuleSet adapter interface (generic contract).
## Core tools call only these generics, never a backend package directly.
## docs/implementation_guide.md#5

#' List the module ids in a module set
#'
#' @param ms A `ModuleSet` object (e.g. one built by [hdWGCNA_ModuleSet()]).
#' @param ... Passed to methods.
#' @return A character vector of module ids.
#' @export
modules <- function(ms, ...) UseMethod('modules')

#' Genes assigned to a module, ranked by membership strength
#'
#' @param ms A `ModuleSet` object.
#' @param module A single module id, as returned by [modules()].
#' @param ... Passed to methods.
#' @return A data.frame with one row per gene, ranked strongest membership first
#'   (e.g. by hdWGCNA's kME).
#' @export
gene_membership <- function(ms, module, ...) UseMethod('gene_membership')

#' Per-cell (or per-sample) module scores
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A data.frame with one column per module (e.g. hdWGCNA module
#'   eigengenes), one row per cell/sample.
#' @export
module_scores <- function(ms, ...) UseMethod('module_scores')

#' Underlying expression matrix backing a module set
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A genes-by-cells (or genes-by-samples) numeric matrix.
#' @note Shadows `base::expression()` once this package is attached; this
#'   package makes no use of `base::expression()` / plotmath. Flagged here as
#'   a known design tradeoff, not fixed in this packaging pass.
#' @export
expression <- function(ms, ...) UseMethod('expression')

#' Underlying raw counts matrix backing a module set
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A genes-by-cells (or genes-by-samples) numeric matrix of raw
#'   counts, aligned to [expression()], or `NULL` if the adapter doesn't
#'   carry raw counts (see [capabilities()]`$counts`).
#' @export
counts <- function(ms, ...) UseMethod('counts')

#' Cell / sample metadata for a module set
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A data.frame with one row per cell/sample, matching the columns of
#'   [expression()] / [module_scores()].
#' @export
metadata <- function(ms, ...) UseMethod('metadata')

#' Backend package versions for provenance logging
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A named list of backend package versions (e.g. `hdWGCNA`,
#'   `Seurat`), so core tools stay backend-agnostic while still recording
#'   which package versions produced the evidence.
#' @export
pkg_versions <- function(ms, ...) UseMethod('pkg_versions')

#' Which capabilities a ModuleSet provides
#'
#' Reports which of a fixed vocabulary this module set supports:
#' `gene_weights` (real per-gene membership weights, e.g. kME, not a uniform
#' placeholder), `module_scores` (per-cell/sample module scores are
#' available), `expression` (the backing expression matrix is available),
#' `counts` (a raw counts matrix is available, see [counts()]), `grouping` (a
#' cell/sample-state grouping column was declared to the adapter),
#' `sample_ids` (a sample-id column was declared), and `pseudobulk` (the
#' underlying data is already pseudobulk-aggregated rather than per-cell --
#' `FALSE` for every current adapter; reserved for a future aggregation
#' backend). Capabilities are declared by the adapter, not inferred from
#' probing `metadata()` -- declaring `grouping`/`sample_ids` is how a source
#' advertises that it supports that concept at all, independent of which
#' particular metadata column a tool is asked to use. Core tools consult this
#' ([has_capability()]) before running so they can skip gracefully instead of
#' erroring when a capability the source doesn't support is required. See
#' [validate_moduleset()] for the full contract check, including that this
#' vector covers the whole vocabulary.
#'
#' @param ms A `ModuleSet` object.
#' @param ... Passed to methods.
#' @return A named logical vector over `c('gene_weights', 'module_scores',
#'   'expression', 'counts', 'grouping', 'sample_ids', 'pseudobulk')`.
#' @export
capabilities <- function(ms, ...) UseMethod('capabilities')

#' Check whether a ModuleSet has a given capability
#'
#' @param ms A `ModuleSet` object.
#' @param name A single capability name, e.g. `'grouping'`; see [capabilities()].
#' @return A single logical; `FALSE` if `name` isn't reported at all.
#' @export
has_capability <- function(ms, name){
    # single-bracket indexing (not `[[`) so a name capabilities() doesn't
    # report comes back NA, not a "subscript out of bounds" error
    isTRUE(capabilities(ms)[name])
}

.moduleset_capability_vocabulary <- c(
    'gene_weights', 'module_scores', 'expression', 'counts', 'grouping', 'sample_ids', 'pseudobulk'
)

# calls a ModuleSet generic and turns a dispatch failure (no applicable
# method, or the method itself erroring) into one consistently worded
# validate_moduleset() error instead of a raw R error from deep inside the
# adapter; `check` is an optional predicate on the successful result
.validate_moduleset_call <- function(ms, generic_name, value, check = NULL){
    result <- tryCatch(value, error = function(e){
        stop(
            'validate_moduleset: ', generic_name, '() did not dispatch for class ',
            paste(class(ms), collapse = '/'), ': ', conditionMessage(e), call. = FALSE
        )
    })
    if (!is.null(check) && !isTRUE(check(result))) {
        stop(
            'validate_moduleset: ', generic_name, '() returned an unexpected shape for class ',
            paste(class(ms), collapse = '/'), call. = FALSE
        )
    }
    result
}

#' Validate that a ModuleSet satisfies the full adapter contract
#'
#' Asserts that every required generic ([modules()], [gene_membership()],
#' [module_scores()], [expression()], [counts()], [metadata()],
#' [pkg_versions()], [capabilities()]) dispatches for `ms`'s class and
#' returns the documented shape; that [capabilities()] covers the full
#' vocabulary (see [capabilities()]) with `pseudobulk = FALSE` (the only
#' value current adapters may report); that `ms$data_level` /
#' `ms$aggregated` are a length-1 character / logical; and that
#' [expression()], `metadata()`, [module_scores()], and [counts()] agree in
#' dimensions wherever their capability is declared `TRUE`. Intended as a
#' one-shot check for anyone writing a new adapter, and run automatically at
#' the top of [run_orchestrator()].
#'
#' @param ms A `ModuleSet` object.
#' @return Invisibly `TRUE` if valid; otherwise throws with the specific
#'   contract violation.
#' @examples
#' validate_moduleset(llegir_example_moduleset())
#' @export
validate_moduleset <- function(ms){
    mods <- .validate_moduleset_call(ms, 'modules', modules(ms), is.character)
    if (length(mods) == 0) stop('validate_moduleset: modules(ms) returned no module ids')

    caps <- .validate_moduleset_call(ms, 'capabilities', capabilities(ms), is.logical)
    missing_caps <- setdiff(.moduleset_capability_vocabulary, names(caps))
    if (length(missing_caps) > 0) {
        stop('validate_moduleset: capabilities() is missing: ', paste(missing_caps, collapse = ', '))
    }
    if (!isFALSE(unname(caps[['pseudobulk']]))) {
        stop('validate_moduleset: capabilities()$pseudobulk must be FALSE for every current adapter')
    }

    gm <- .validate_moduleset_call(ms, 'gene_membership', gene_membership(ms, mods[1]), is.data.frame)
    if (!all(c('gene_name', 'module', 'kme') %in% colnames(gm))) {
        stop('validate_moduleset: gene_membership() must return gene_name/module/kme columns')
    }

    meta <- .validate_moduleset_call(ms, 'metadata', metadata(ms), is.data.frame)
    .validate_moduleset_call(ms, 'pkg_versions', pkg_versions(ms), is.list)

    if (isTRUE(caps[['expression']])) {
        expr <- .validate_moduleset_call(ms, 'expression', expression(ms), function(x) !is.null(dim(x)))
        if (ncol(expr) != nrow(meta)) {
            stop('validate_moduleset: expression() columns (', ncol(expr), ') and metadata() rows (',
                 nrow(meta), ') must align')
        }
        if (isTRUE(caps[['module_scores']])) {
            scores <- .validate_moduleset_call(ms, 'module_scores', module_scores(ms))
            if (!is.null(scores) && nrow(scores) != ncol(expr)) {
                stop('validate_moduleset: module_scores() rows must align with expression() columns')
            }
        }
        if (isTRUE(caps[['counts']])) {
            cnts <- .validate_moduleset_call(ms, 'counts', counts(ms))
            if (is.null(cnts) || !identical(dim(cnts), dim(expr))) {
                stop('validate_moduleset: counts() dimensions must match expression() when the counts capability is TRUE')
            }
        }
    }

    data_level <- ms$data_level
    if (!is.character(data_level) || length(data_level) != 1) {
        stop('validate_moduleset: data_level must be a length-1 character')
    }
    aggregated <- ms$aggregated
    if (!is.logical(aggregated) || length(aggregated) != 1) {
        stop('validate_moduleset: aggregated must be a length-1 logical')
    }

    invisible(TRUE)
}
