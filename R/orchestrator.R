## orchestrator: runs a configured set of tools over every module in a
## ModuleSet and writes validated, hashed evidence packets to disk.
##
## `tool_config` is a list of `list(fn, params)` specs, e.g.:
##   list(
##       list(fn = hub_genes_tool, params = list(n_hubs = 25)),
##       list(fn = cluster_dme_tool, params = list(group_by = 'lv2_annot'))
##   )
## `fn` is any function(ctx) -> evidence_fragment; core tools today, custom
## tools later (M2) just append to this list.

# fragments for one module, in packet form; one bad tool call fails the whole
# module (a partial packet would be worse than no packet)
run_module <- function(ms, module_id, tool_config, input_hash = NA_character_){
    fragments <- lapply(tool_config, function(spec){
        ctx <- list(ms = ms, module_id = module_id, params = spec$params)
        spec$fn(ctx)
    })
    build_evidence_packet(module_id, fragments, input_hash = input_hash)
}

# runs every module independently so one module's failure (e.g. a tool
# erroring on a degenerate module) doesn't take down the whole batch; failures
# are reported via `warning()` and recorded as NULL in the returned list
run_orchestrator <- function(ms, tool_config, output_dir, modules_use = NULL, input_hash = NA_character_){
    if (is.null(modules_use)) modules_use <- modules(ms)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

    packets <- lapply(modules_use, function(mod){
        packet <- tryCatch(
            run_module(ms, mod, tool_config, input_hash = input_hash),
            error = function(e){
                warning('module ', mod, ' failed: ', conditionMessage(e), call. = FALSE)
                NULL
            }
        )
        if (!is.null(packet)) {
            write_evidence_packet(packet, file.path(output_dir, paste0(mod, '.json')))
        }
        packet
    })
    names(packets) <- modules_use
    invisible(packets)
}
