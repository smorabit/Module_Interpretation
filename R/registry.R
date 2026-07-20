## Tool registry: register_tool()/get_tool()/list_tools() make tool
## registration a uniform, documented public API for both core and custom
## tools (docs/milestone_extensibility.md Part 2b, docs/custom_tools.md). A
## tool_spec carries id, description, the fragment `type`(s) it may emit, and
## the ModuleSet capabilities it requires -- consulted by run_module() to
## skip gracefully and record why, instead of relying on every tool to
## self-skip (Part 1's ad-hoc pattern, still supported for direct `fn=` use).
##
## Core tools are registered through the exact same register_tool() call,
## from .onLoad() below rather than at this file's top level: R sources R/
## files in (roughly) alphabetical order, so a top-level call here would run
## before tool_signature_correlation.R etc. define the functions it
## references. .onLoad() is invoked by the package-loading machinery only
## after every R/ file has been sourced, so it's the correct place for
## cross-file registration regardless of file order.

.tool_registry <- new.env(parent = emptyenv())

#' Register a tool (core or custom) in the tool registry
#'
#' A tool is any `function(ctx) -> evidence_fragment` (or `NULL`, to skip;
#' see [run_module()]). Registering it makes it runnable from `tool_config`
#' by id (`list(id = 'my_tool', params = list(...))`) and lets
#' [run_module()] check its required `ModuleSet` [capabilities()] before
#' calling it, skipping gracefully and recording why in the packet if
#' they're unmet, rather than the tool having to self-skip. See
#' `docs/custom_tools.md` for a worked template.
#'
#' @param id A unique tool id, e.g. `'hub_genes'` or `'my_custom_tool'`.
#' @param fn A `function(ctx) -> evidence_fragment`, where `ctx` is
#'   `list(ms, module_id, params)` (see [run_module()]).
#' @param type One or more of the `evidence_fragment` controlled vocabulary
#'   (see [evidence_fragment()]) this tool may emit. Descriptive only -- the
#'   fragment a tool actually returns is always checked against the full
#'   contract by [validate_evidence_fragment()] regardless of what's declared
#'   here. A tool whose emitted type depends on its params (e.g.
#'   [module_by_metadata_tool()]) can declare more than one.
#' @param description A one-line, human-readable description of what the tool does.
#' @param requires The `ModuleSet` [capabilities()] this tool needs to run:
#'   either a character vector (e.g. `c('grouping', 'module_scores')`), or a
#'   `function(params) -> character vector` for a tool whose requirement
#'   depends on how it's called (e.g. [module_by_metadata_tool()] only needs
#'   `sample_ids` for categorical columns). Default `character(0)` (no
#'   requirement).
#' @return `id`, invisibly.
#' @examples
#' my_tool <- function(ctx) hub_genes_tool(ctx)
#' register_tool(
#'     'my_tool', my_tool, type = 'ranked_genes',
#'     description = 'demo', requires = character(0)
#' )
#' @export
register_tool <- function(id, fn, type, description, requires = character(0)){
    if (!is.character(id) || length(id) != 1) stop('id must be a single string')
    if (!is.function(fn)) stop('fn must be a function')
    if (!all(type %in% .fragment_types)) stop('invalid type: ', paste(setdiff(type, .fragment_types), collapse = ', '))
    if (!is.function(requires) && !is.character(requires)) stop('requires must be a character vector or a function(params)')

    spec <- structure(
        list(id = id, fn = fn, type = type, description = description, requires = requires),
        class = 'tool_spec'
    )
    assign(id, spec, envir = .tool_registry)
    invisible(id)
}

#' Look up a registered tool spec by id
#'
#' @param id A tool id, as passed to [register_tool()].
#' @return A `tool_spec` object: `list(id, fn, type, description, requires)`.
#' @examples
#' get_tool('hub_genes')
#' @export
get_tool <- function(id){
    if (!exists(id, envir = .tool_registry, inherits = FALSE)) stop('tool not registered: ', id)
    get(id, envir = .tool_registry, inherits = FALSE)
}

#' List every registered tool id
#'
#' @return A sorted character vector of tool ids.
#' @examples
#' list_tools()
#' @export
list_tools <- function(){
    sort(ls(envir = .tool_registry))
}

# the required capabilities for one call: requires may be a static vector or
# a function(params), for a tool whose requirement depends on how it's called
.tool_spec_requires <- function(spec, params){
    if (is.function(spec$requires)) spec$requires(params) else spec$requires
}

.onLoad <- function(libname, pkgname){
    register_tool(
        'hub_genes', hub_genes_tool, type = 'ranked_genes',
        description = 'Top module genes ranked by membership (kME)',
        requires = character(0)
    )
    register_tool(
        'cluster_dme', cluster_dme_tool, type = 'state_expression',
        description = 'Which cell states express this module',
        requires = c('grouping', 'module_scores')
    )
    register_tool(
        'module_by_metadata', module_by_metadata_tool,
        type = c('categorical_association', 'continuous_correlation'),
        description = 'Module score vs. a declared metadata column',
        requires = function(params){
            if ((params$column_type %||% 'categorical') == 'continuous') {
                'module_scores'
            } else {
                c('module_scores', 'sample_ids')
            }
        }
    )
    register_tool(
        'geneset_enrichment', geneset_enrichment_tool, type = 'geneset_enrichment',
        description = "Gene-set overlap enrichment among a module's hub genes",
        requires = 'expression'
    )
    register_tool(
        'signature_correlation', signature_correlation_tool, type = 'signature_correlation',
        description = "Correlate a module's activity with a signature library",
        requires = c('module_scores', 'expression')
    )
}
