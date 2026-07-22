## orchestrator: runs a configured set of tools over every module in a
## ModuleSet and writes validated, hashed evidence packets to disk.
##
## `tool_config` is a list of `list(fn, params)` specs, e.g.:
##   list(
##       list(fn = top_genes_tool, params = list(n_hubs = 25)),
##       list(fn = cluster_dme_tool, params = list(group_by = 'lv2_annot'))
##   )
## `fn` is any function(ctx) -> evidence_fragment; core tools today, custom
## tools later (M2) just append to this list.

#' Run one module's evidence tools and build its evidence packet
#'
#' Runs every tool in `tool_config` against one module, in order, and bundles
#' the resulting evidence fragments into a validated, hashed evidence packet
#' via [build_evidence_packet()]. One bad tool call fails the whole module --
#' a partial packet would be worse than no packet; a malformed fragment fails
#' loudly the same way, via [validate_evidence_fragment()].
#'
#' @param ms A `ModuleSet`.
#' @param module_id A single module id (as returned by [modules()]).
#' @param tool_config A list of tool specs, each one of:
#'   * `list(fn, params)` -- a direct call to any `function(ctx) ->
#'     evidence_fragment` (or `NULL`, to skip); the tool is responsible for
#'     its own graceful capability-based skip, e.g. [cluster_dme_tool()].
#'   * `list(id, params)` -- `id` is looked up in the tool registry (see
#'     [register_tool()]), and `run_module()` itself checks the tool's
#'     declared required [capabilities()] before calling it. If unmet, the
#'     tool is skipped and the reason is recorded on the packet's
#'     `provenance$skipped` instead of the tool having to self-skip --
#'     core and custom tools registered via [register_tool()] are run
#'     identically this way.
#'
#'   Either form's `params` is passed through as `ctx$params`.
#' @param input_hash Optional hash of the input `ModuleSet`, recorded on the
#'   packet for provenance.
#' @param module_method Optional free-form description of how the modules
#'   themselves were generated, e.g. `'cNMF factors, k=20'`; see
#'   [dataset_description()]. Passed through as `ctx$module_method`, and every
#'   core tool records it on its fragment's provenance via
#'   [make_provenance()]. Default `NA`.
#' @return An evidence packet; see [build_evidence_packet()].
#' @examples
#' ms <- llegir_example_moduleset()
#' run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
#' run_module(ms, modules(ms)[1], list(list(id = 'top_genes', params = list())))
#' @export
run_module <- function(ms, module_id, tool_config, input_hash = NA_character_, module_method = NA_character_){
    results <- lapply(tool_config, function(spec){
        if (!is.null(spec$id)) {
            tool <- get_tool(spec$id)
            required <- .tool_spec_requires(tool, spec$params)
            missing_caps <- required[!vapply(required, function(cap) has_capability(ms, cap), logical(1))]
            if (length(missing_caps) > 0) {
                reason <- paste0('missing capabilities: ', paste(missing_caps, collapse = ', '))
                message(spec$id, ': skipped, ', reason)
                return(list(fragment = NULL, skip = list(tool_id = spec$id, reason = reason)))
            }
            fn <- tool$fn
        } else {
            fn <- spec$fn
        }
        ctx <- list(ms = ms, module_id = module_id, params = spec$params, module_method = module_method)
        list(fragment = fn(ctx), skip = NULL)
    })
    fragments <- Filter(Negate(is.null), lapply(results, `[[`, 'fragment'))
    skipped <- Filter(Negate(is.null), lapply(results, `[[`, 'skip'))
    build_evidence_packet(module_id, fragments, input_hash = input_hash, skipped = skipped)
}

#' Run a configured set of evidence tools over every module in a ModuleSet
#'
#' Runs [run_module()] independently for each module in `modules_use` (or
#' every module in `ms` if not given) and writes each validated, hashed
#' evidence packet to `output_dir` as JSON. One module's failure (e.g. a tool
#' erroring on a degenerate module) doesn't take down the whole batch;
#' failures are reported via `warning()` and recorded as `NULL` in the
#' returned list. `tables_dir`, when given, also persists every fragment's
#' result table (via [write_fragment_tables()]) alongside the JSON packets.
#'
#' @param ms A `ModuleSet`.
#' @param tool_config A list of `list(fn, params)` specs; see [run_module()].
#' @param output_dir Directory to write one evidence packet JSON file per module.
#' @param tables_dir Optional directory to also persist every fragment's
#'   result table.
#' @param modules_use Optional subset of module ids to run; defaults to all
#'   modules in `ms`.
#' @param input_hash Optional hash of the input `ModuleSet`, recorded on each packet.
#' @param module_method Optional free-form description of how the modules
#'   themselves were generated; see [run_module()] / [dataset_description()].
#'   Passed through to every [run_module()] call. Default `NA`.
#' @param validate If `TRUE` (default), run [validate_moduleset()] on `ms`
#'   before doing anything else, so a malformed adapter fails loudly up
#'   front instead of partway through a batch.
#' @return Invisibly, a named list of evidence packets (one per module; `NULL`
#'   for any module that failed).
#' @examples
#' ms <- llegir_example_moduleset()
#' run_orchestrator(ms, list(list(fn = top_genes_tool, params = list())), output_dir = tempfile())
#' @export
run_orchestrator <- function(ms, tool_config, output_dir, tables_dir = NULL, modules_use = NULL,
                              input_hash = NA_character_, module_method = NA_character_, validate = TRUE){
    if (validate) validate_moduleset(ms)
    if (is.null(modules_use)) modules_use <- modules(ms)
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    if (!is.null(tables_dir)) dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

    packets <- lapply(modules_use, function(mod){
        packet <- tryCatch(
            run_module(ms, mod, tool_config, input_hash = input_hash, module_method = module_method),
            error = function(e){
                warning('module ', mod, ' failed: ', conditionMessage(e), call. = FALSE)
                NULL
            }
        )
        if (!is.null(packet)) {
            write_evidence_packet(packet, file.path(output_dir, paste0(mod, '.json')))
            if (!is.null(tables_dir)) write_fragment_tables(packet, tables_dir)
        }
        packet
    })
    names(packets) <- modules_use
    invisible(packets)
}

#' Build a dataset context once per dataset
#'
#' The dataset-level analog of [run_orchestrator()]: runs every tool in
#' `dataset_tool_config` once against the whole `ModuleSet` (never per
#' module) and bundles the resulting `dataset_fragment`s into a validated,
#' hashed dataset context via [build_dataset_context()]. Mirrors
#' [run_module()]'s spec handling exactly, except `ctx` has no `module_id`
#' -- a dataset tool reads `expression(ms)`/`metadata(ms)`/`module_scores(ms)`
#' in full, not for one module.
#'
#' @param ms A `ModuleSet`.
#' @param dataset_tool_config A list of tool specs, each one of:
#'   * `list(fn, params)` -- a direct call to any `function(ctx) ->
#'     dataset_fragment` (or `NULL`, to skip); the tool is responsible for
#'     its own graceful capability-based skip.
#'   * `list(id, params)` -- `id` is looked up in the tool registry (see
#'     [register_tool()]), and `run_dataset_context()` checks the tool's
#'     declared required [capabilities()] before calling it. If unmet, the
#'     tool is skipped and the reason is recorded on the context's
#'     `provenance$skipped`.
#'
#'   Either form's `params` is passed through as `ctx$params`.
#' @param input_hash Optional hash of the input `ModuleSet`, recorded on the
#'   context for provenance.
#' @param module_method Optional free-form description of how the modules
#'   themselves were generated; see [run_module()] / [dataset_description()].
#'   Passed through as `ctx$module_method`. Default `NA`.
#' @param validate If `TRUE` (default), run [validate_moduleset()] on `ms`
#'   before doing anything else.
#' @return A dataset context; see [build_dataset_context()].
#' @examples
#' ms <- llegir_example_moduleset()
#' run_dataset_context(ms, list())
#' @export
run_dataset_context <- function(ms, dataset_tool_config, input_hash = NA_character_,
                                 module_method = NA_character_, validate = TRUE){
    if (validate) validate_moduleset(ms)
    results <- lapply(dataset_tool_config, function(spec){
        if (!is.null(spec$id)) {
            tool <- get_tool(spec$id)
            required <- .tool_spec_requires(tool, spec$params)
            missing_caps <- required[!vapply(required, function(cap) has_capability(ms, cap), logical(1))]
            if (length(missing_caps) > 0) {
                reason <- paste0('missing capabilities: ', paste(missing_caps, collapse = ', '))
                message(spec$id, ': skipped, ', reason)
                return(list(fragment = NULL, skip = list(tool_id = spec$id, reason = reason)))
            }
            fn <- tool$fn
        } else {
            fn <- spec$fn
        }
        ctx <- list(ms = ms, params = spec$params, module_method = module_method)
        list(fragment = fn(ctx), skip = NULL)
    })
    fragments <- Filter(Negate(is.null), lapply(results, `[[`, 'fragment'))
    skipped <- Filter(Negate(is.null), lapply(results, `[[`, 'skip'))
    build_dataset_context(fragments, input_hash = input_hash, skipped = skipped)
}

## synthesis-stage orchestration (docs/milestone_2.md tasks 5-7): mirrors
## run_module()/run_orchestrator() above but for the packet -> interpretation
## stage. Kept in this file rather than a separate one since it's the same
## per-module-then-batch shape, just one stage further down the pipeline.

#' Synthesize one evidence packet into a validated interpretation
#'
#' Runs the full packet -> interpretation pipeline for one module: calls
#' `backend` via [synthesize_interpretation()], enforces citation
#' faithfulness via [enforce_faithfulness()], fuses model and deterministic
#' confidence via [fuse_confidence()], and validates the result. These three
#' steps always run together -- an interpretation that skipped faithfulness
#' or fusion isn't one this engine should emit.
#'
#' @param packet An evidence packet, as built by [build_evidence_packet()] /
#'   [run_module()].
#' @param desc A `dataset_description`; see [dataset_description()].
#' @param backend A synthesis backend function; see [mock_backend()],
#'   [ellmer_backend()], [resolve_backend()].
#' @param temperature Sampling temperature passed to the backend.
#' @param seed Optional seed, recorded on the interpretation's provenance.
#' @param prompt_template_version Prompt template version to record on the
#'   interpretation's provenance.
#' @param schema_path Path to the interpretation JSON schema; defaults to the
#'   schema shipped with the package.
#' @param user_weights Named list of per-`tool_id` weight multipliers for
#'   [calculate_fusion_score()]. Computed once here and shared between the
#'   prompt's EVIDENCE CONFIDENCE MATRIX and [fuse_confidence()], so the
#'   printed fusion string can never drift from what the model was shown.
#'   Default `list()`.
#' @param dataset_context An optional dataset context, as built by
#'   [build_dataset_context()] / [run_dataset_context()], threaded into
#'   [build_user_prompt()] as global framing. Never enters
#'   [calculate_fusion_score()] or [enforce_faithfulness()]. `NULL` (default)
#'   omits the DATASET CONTEXT block.
#' @return A validated `interpretation` object.
#' @examples
#' ms <- llegir_example_moduleset()
#' packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
#' desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
#' synthesize_module(packet, desc, mock_backend())
#' @export
synthesize_module <- function(packet, desc, backend, temperature = 0, seed = NA_real_,
                               prompt_template_version = PROMPT_TEMPLATE_VERSION,
                               schema_path = system.file('schemas', 'interpretation.schema.json', package = 'llegir'),
                               user_weights = list(), dataset_context = NULL){
    fusion <- calculate_fusion_score(packet$fragments, user_weights = user_weights)
    interp <- synthesize_interpretation(
        packet, desc, backend, temperature = temperature, seed = seed,
        prompt_template_version = prompt_template_version, schema_path = schema_path,
        user_weights = user_weights, fusion = fusion, dataset_context = dataset_context
    )
    interp <- enforce_faithfulness(interp, packet)
    interp <- fuse_confidence(interp, packet, user_weights = user_weights, fusion = fusion)
    validate_interpretation(interp)
    interp
}

#' Run synthesis over a batch of evidence packets
#'
#' Runs [synthesize_module()] independently for each packet in `packets`,
#' writing per-module interpretation JSON and rendered Markdown paragraphs
#' (via [render_paragraph()]) to `output_dir`, plus a run-level
#' `review_queue.tsv` ([write_review_queue()]) and `manifest.json`
#' ([write_synthesis_manifest()]). One module's synthesis failure is warned
#' and recorded as `NULL`, mirroring [run_orchestrator()]'s failure isolation.
#'
#' @param packets A named list of evidence packets, e.g. the return value of
#'   [run_orchestrator()].
#' @param desc A `dataset_description`; see [dataset_description()].
#' @param backend A synthesis backend function; see [mock_backend()],
#'   [ellmer_backend()], [resolve_backend()].
#' @param output_dir Directory to write per-module interpretations,
#'   paragraphs, `review_queue.tsv`, and `manifest.json`.
#' @param temperature Sampling temperature passed to the backend.
#' @param seed Optional seed, recorded on each interpretation's provenance.
#' @param prompt_template_version Prompt template version to record on each
#'   interpretation's provenance.
#' @param schema_path Path to the interpretation JSON schema; defaults to the
#'   schema shipped with the package.
#' @param dataset_context An optional dataset context, as built by
#'   [build_dataset_context()] / [run_dataset_context()]. Computed once and
#'   passed to every module's [synthesize_module()] call, exactly like
#'   `desc`. `NULL` (default) omits the DATASET CONTEXT block.
#' @return Invisibly, a named list of interpretation objects (one per module;
#'   `NULL` for any module whose synthesis failed).
#' @examples
#' ms <- llegir_example_moduleset()
#' packets <- run_orchestrator(ms, list(list(fn = top_genes_tool, params = list())), output_dir = tempfile())
#' desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
#' run_synthesis_orchestrator(packets, desc, mock_backend(), output_dir = tempfile())
#' @export
run_synthesis_orchestrator <- function(packets, desc, backend, output_dir, temperature = 0, seed = NA_real_,
                                        prompt_template_version = PROMPT_TEMPLATE_VERSION,
                                        schema_path = system.file('schemas', 'interpretation.schema.json', package = 'llegir'),
                                        dataset_context = NULL){
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    mods <- names(packets)

    interps <- lapply(mods, function(mod){
        packet <- packets[[mod]]
        if (is.null(packet)) return(NULL)
        interp <- tryCatch(
            synthesize_module(
                packet, desc, backend, temperature = temperature, seed = seed,
                prompt_template_version = prompt_template_version, schema_path = schema_path,
                dataset_context = dataset_context
            ),
            error = function(e){
                warning('synthesis failed for module ', mod, ': ', conditionMessage(e), call. = FALSE)
                NULL
            }
        )
        if (!is.null(interp)) {
            write_interpretation(interp, file.path(output_dir, paste0(mod, '.json')))
            writeLines(render_paragraph(interp), file.path(output_dir, paste0(mod, '.md')))
        }
        interp
    })
    names(interps) <- mods

    write_review_queue(interps, file.path(output_dir, 'review_queue.tsv'))
    write_synthesis_manifest(build_synthesis_manifest(interps, desc, prompt_template_version), file.path(output_dir, 'manifest.json'))

    invisible(interps)
}
