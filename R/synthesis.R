## synthesis: evidence packet -> interpretation object via a structured-output
## backend (docs/milestone_2.md task 2). The model's role is bounded: it fills
## the schema below from the compact prompt (R/prompt.R); it never sees the
## raw result tables and never runs analysis code.
##
## A backend is `function(system_prompt, user_prompt, schema_json, packet_hash = NA_character_) -> list(content, meta)`:
##   - `content` is the parsed structured output (a plain R list) matching the
##     model-facing schema (model_output_schema_json()).
##   - `meta` carries backend bookkeeping for provenance: `model`,
##     `model_version`, `ellmer_call` (arbitrary named list, e.g. token counts).
##   - `packet_hash` is passed through for backends that key off it (e.g.
##     cached_backend() below); backends that don't need it just ignore it.
## mock_backend() and ellmer_backend() below both satisfy this contract, so
## synthesize_interpretation() never needs to know which one it's talking to.
##
## docs/dev_economy.md task 1: provider + model are config-selected via
## resolve_backend() (mock | github | gemini), all built on the same
## ellmer_backend() since every ellmer chat_*() constructor returns the same
## Chat object (chat_structured()/get_tokens()/get_model()) -- only the
## provider function and default model id differ.

# some providers' structured-output schema (e.g. Gemini's response_schema,
# an OpenAPI-lite dialect) rejects JSON-Schema-only keywords: a `type` given
# as a ["X", "null"] union array, or draft-07 metadata keys like `$schema`/
# `$id`. Recursively rewrites both to the widely-supported form (single
# `type` + `nullable: true`; `enum` alone implies a string type) so the one
# canonical schema still round-trips through stricter providers.
.normalize_schema_node <- function(node){
    if (!is.list(node)) return(node)
    if (!is.null(node$type) && length(node$type) > 1) {
        non_null <- setdiff(unlist(node$type), 'null')
        if (length(non_null) == 1) {
            node$type <- non_null
            node$nullable <- TRUE
        }
    }
    if (!is.null(node$enum) && is.null(node$type)) node$type <- 'string'
    if (!is.null(node$properties)) node$properties <- lapply(node$properties, .normalize_schema_node)
    if (!is.null(node$items)) node$items <- .normalize_schema_node(node$items)
    node
}

#' Model-facing interpretation schema, derived from the canonical schema
#'
#' The interpretation JSON schema minus fields the model shouldn't fill:
#' `provenance`/`schema_version` are orchestrator bookkeeping, and
#' `confidence$model_score` just duplicates `confidence$score` for audit --
#' asking the model to fill it twice invites drift. Derived from the one
#' canonical schema file (rather than hand-duplicated) so the two never
#' diverge, and normalized to a schema dialect supported across providers.
#'
#' @param schema_path Path to the canonical `interpretation.schema.json`;
#'   defaults to the schema shipped with the package.
#' @return A JSON string (a `jsonlite::toJSON()` scalar) of the model-facing schema.
#' @examples
#' model_output_schema_json()
#' @export
model_output_schema_json <- function(schema_path = system.file('schemas', 'interpretation.schema.json', package = 'llegir')){
    schema <- jsonlite::fromJSON(schema_path, simplifyVector = FALSE)
    schema$`$schema` <- NULL
    schema$`$id` <- NULL
    schema$schema_version <- NULL
    schema$required <- Filter(function(x) !(x %in% c('provenance', 'schema_version')), schema$required)
    schema$properties$provenance <- NULL
    schema$properties$schema_version <- NULL
    schema$properties$confidence$required <- Filter(function(x) x != 'model_score', schema$properties$confidence$required)
    schema$properties$confidence$properties$model_score <- NULL
    schema <- .normalize_schema_node(schema)
    jsonlite::toJSON(schema, auto_unbox = TRUE, null = 'null')
}

#' Offline mock synthesis backend
#'
#' A canned, fixed-response backend: the first-class offline backend used by
#' tests and CI, and never touches the network. It cites fragment_ids
#' (`'top_genes'`, `'geneset_enrichment'`) and directions (`'na'`, `'up'`)
#' that hold on every packet produced by the core tools (`top_genes` is
#' always direction `'na'`, `geneset_enrichment` is always direction
#' `'up'`), so it passes [check_faithfulness()] against any real evidence
#' packet without per-module logic.
#'
#' @return A backend function with the signature
#'   `function(system_prompt, user_prompt, schema_json, packet_hash) -> list(content, meta)`,
#'   suitable for [synthesize_interpretation()].
#' @examples
#' backend <- mock_backend()
#' backend('system prompt', 'user prompt', '{}')$content$proposed_label
#' @export
mock_backend <- function(){
    function(system_prompt, user_prompt, schema_json, packet_hash = NA_character_){
        list(
            content = list(
                module_id = 'MOCK',
                proposed_label = 'Myeloid activation program (mock)',
                one_line_summary = 'Mock synthesis output for offline testing; not derived from the evidence packet.',
                dominant_biology = 'Not evaluated by the mock backend.',
                supporting_claims = list(
                    list(claim = 'Top genes were computed by the deterministic core.', fragment_ids = list('top_genes'), direction = 'na'),
                    list(claim = 'The module has enriched gene-set terms.', fragment_ids = list('geneset_enrichment'), direction = 'up')
                ),
                cell_state = NA_character_,
                condition_dynamics = NA_character_,
                metadata_associations = list(),
                flags = list(),
                confidence = list(score = 0.5, rationale = 'Mock backend: fixed neutral confidence, not evidence-derived.')
            ),
            meta = list(model = 'mock', model_version = 'mock-0.1', ellmer_call = list(backend = 'mock'))
        )
    }
}

# free-tier providers rate-limit per minute; retries a rate-limited call with
# linear backoff rather than failing the whole batch over a transient 429.
.with_rate_limit_retry <- function(fn, max_attempts = 5, base_wait_s = 20){
    for (attempt in seq_len(max_attempts)) {
        result <- tryCatch(list(ok = TRUE, value = fn()), error = function(e) list(ok = FALSE, error = e))
        if (result$ok) return(result$value)
        is_rate_limited <- grepl('429|rate.?limit|RESOURCE_EXHAUSTED', conditionMessage(result$error), ignore.case = TRUE)
        if (!is_rate_limited || attempt == max_attempts) stop(result$error)
        wait_s <- base_wait_s * attempt
        message('rate limited (attempt ', attempt, '/', max_attempts, '), retrying in ', wait_s, 's')
        Sys.sleep(wait_s)
    }
}

# GitHub Models' OpenAI-compatible strict structured-output mode imposes two
# rules Gemini's response_schema dialect doesn't (and errors on the same
# schema without them -- see docs/milestone_2.md task 2's note that providers
# differ in how strictly they honor a complex responseSchema): every object
# node must set `additionalProperties: false`, and every object node's
# `required` must list ALL of its properties (optionality is expressed only
# via a nullable type, e.g. supporting_claims[].strength). Applied only
# inside the github backend below, not to the one canonical schema
# (model_output_schema_json() stays provider-agnostic).
.to_openai_strict_schema <- function(node){
    if (!is.list(node)) return(node)
    if (identical(node$type, 'object') && !is.null(node$properties)) {
        node$additionalProperties <- FALSE
        node$required <- names(node$properties)
    }
    if (!is.null(node$properties)) node$properties <- lapply(node$properties, .to_openai_strict_schema)
    if (!is.null(node$items)) node$items <- .to_openai_strict_schema(node$items)
    node
}

#' Live synthesis backend via ellmer structured output
#'
#' Not exercised by the offline test suite (needs network + a provider API
#' key). Provider and model are config-selected via `chat_fn`/`model`;
#' credentials are picked up by `ellmer` itself from the provider's
#' environment variable (e.g. `GEMINI_API_KEY`, `GITHUB_PAT`) and are never
#' handled here. `schema_transform` adapts the one canonical schema to a
#' provider's structured-output dialect (e.g. the internal
#' `.to_openai_strict_schema()` used by [resolve_backend()] for GitHub
#' Models) without forking the schema itself.
#'
#' @param chat_fn An `ellmer::chat_*()` constructor, e.g.
#'   `ellmer::chat_google_gemini` or `ellmer::chat_github`.
#' @param model Model id passed to `chat_fn`.
#' @param temperature Sampling temperature.
#' @param credentials Optional credentials passed to `chat_fn`; `NULL` uses
#'   the provider's default environment-variable lookup.
#' @param schema_transform A `function(schema_list) -> schema_list` applied
#'   to the parsed canonical schema before it's sent to the provider.
#' @return A backend function; see [mock_backend()] for the contract.
#' @examples
#' \dontrun{
#' backend <- ellmer_backend(ellmer::chat_google_gemini, model = 'gemini-3.5-flash')
#' }
#' @export
ellmer_backend <- function(chat_fn = ellmer::chat_google_gemini, model = 'gemini-3.5-flash',
                            temperature = 0, credentials = NULL, schema_transform = identity){
    function(system_prompt, user_prompt, schema_json, packet_hash = NA_character_){
        chat <- chat_fn(
            system_prompt = system_prompt,
            params = ellmer::params(temperature = temperature),
            model = model,
            credentials = credentials,
            echo = 'none'
        )
        schema <- schema_transform(jsonlite::fromJSON(schema_json, simplifyVector = FALSE))
        type_spec <- ellmer::type_from_schema(text = jsonlite::toJSON(schema, auto_unbox = TRUE, null = 'null'))
        parsed <- .with_rate_limit_retry(function() chat$chat_structured(user_prompt, type = type_spec))
        tokens <- tryCatch(as.list(chat$get_tokens()), error = function(e) list())
        list(
            content = parsed,
            meta = list(model = chat$get_model(), model_version = chat$get_model(), ellmer_call = list(tokens = tokens))
        )
    }
}

# provider + model as a single config knob: 'github' (gpt-4o-mini, the
# generous ~150/day tier) is the default dev provider, 'gemini' is kept for
# occasional quality cross-checks, 'mock' is the offline/CI backend. All
# three satisfy the same backend contract above, so callers (scripts, tests)
# never branch on provider.
.default_models <- list(github = 'gpt-4o-mini', gemini = 'gemini-3.5-flash')

#' Resolve a synthesis backend from a provider name
#'
#' Provider + model as a single config knob. `'mock'` is the offline/CI
#' backend ([mock_backend()]); `'github'` and `'gemini'` are both built on
#' [ellmer_backend()] with the matching `ellmer::chat_*()` constructor and a
#' default model id, so callers never branch on provider.
#'
#' @param provider One of `'github'` (default), `'gemini'`, `'mock'`.
#' @param model Optional model id override; defaults to a per-provider default.
#' @param temperature Sampling temperature (ignored by the mock backend).
#' @return A backend function; see [mock_backend()] for the contract.
#' @examples
#' backend <- resolve_backend(provider = 'mock')
#' \dontrun{
#' backend <- resolve_backend(provider = 'gemini')
#' }
#' @export
resolve_backend <- function(provider = 'github', model = NULL, temperature = 0){
    provider <- match.arg(provider, c('github', 'gemini', 'mock'))
    if (provider == 'mock') return(mock_backend())
    chat_fn <- switch(provider, github = ellmer::chat_github, gemini = ellmer::chat_google_gemini)
    schema_transform <- if (provider == 'github') .to_openai_strict_schema else identity
    ellmer_backend(chat_fn = chat_fn, model = model %||% .default_models[[provider]],
                    temperature = temperature, schema_transform = schema_transform)
}

#' Cache a synthesis backend's raw model calls on disk
#'
#' Wraps any backend with a cache keyed on `packet_hash` + `provider` +
#' `model` + `prompt_template_version`: on a hit, the inner backend (the live
#' API call) is skipped entirely. Deliberately caches only the raw model
#' call, not the full synthesized interpretation -- faithfulness, confidence
#' fusion, and rendering are cheap and deterministic and still re-run every
#' time on top of the cached content, so only an actual
#' packet/provider/model/prompt change ever costs an API call.
#'
#' @param backend The backend function to wrap; see [mock_backend()] for the contract.
#' @param provider Provider name, used only as a cache-key component.
#' @param model Model id, used only as a cache-key component.
#' @param prompt_template_version Prompt template version, used only as a cache-key component.
#' @param cache_dir Directory to store cached `.rds` responses in.
#' @param force_refresh If `TRUE`, bypass the cache and always call `backend`.
#' @return A backend function; see [mock_backend()] for the contract.
#' @examples
#' backend <- cached_backend(mock_backend(), provider = 'mock', model = 'mock',
#'     prompt_template_version = '0.2', cache_dir = tempfile())
#' @export
cached_backend <- function(backend, provider, model, prompt_template_version,
                            cache_dir = 'output/cache', force_refresh = FALSE){
    function(system_prompt, user_prompt, schema_json, packet_hash = NA_character_){
        key <- digest::digest(list(packet_hash, provider, model, prompt_template_version), algo = 'sha256')
        cache_path <- file.path(cache_dir, paste0(key, '.rds'))
        if (!force_refresh && file.exists(cache_path)) return(readRDS(cache_path))

        result <- backend(system_prompt, user_prompt, schema_json, packet_hash = packet_hash)
        dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
        saveRDS(result, cache_path)
        result
    }
}

#' Synthesize one evidence packet into an interpretation via a backend
#'
#' The model's role is bounded: it fills the model-facing schema
#' ([model_output_schema_json()]) from the compact prompt built by
#' [build_system_prompt()] / [build_user_prompt()]; it never sees the raw
#' evidence result tables and never runs analysis code. `module_id` is always
#' taken from `packet`, never trusted from the model's output, and
#' `literature` is always forced empty (out of scope for this milestone
#' regardless of what a backend returns).
#'
#' @param packet An evidence packet, as built by [build_evidence_packet()].
#' @param desc A `dataset_description`; see [dataset_description()]. Required
#'   (hard error via [validate_dataset_description()] if missing/empty).
#' @param backend A backend function; see [mock_backend()] for the contract.
#' @param temperature Sampling temperature passed to the backend.
#' @param seed Optional seed, recorded on the interpretation's provenance.
#' @param prompt_template_version Prompt template version to record on the
#'   interpretation's provenance.
#' @param schema_path Path to the interpretation JSON schema; defaults to the
#'   schema shipped with the package.
#' @param user_weights Named list of per-`tool_id` weight multipliers passed
#'   to [calculate_fusion_score()] for the EVIDENCE CONFIDENCE MATRIX injected
#'   into the user prompt (see [build_user_prompt()]). Default `list()`.
#' @param fusion An optional pre-computed [calculate_fusion_score()] result to
#'   inject into the user prompt; `NULL` (default) computes it from
#'   `packet$fragments` and `user_weights`. [synthesize_module()] computes it
#'   once and passes the same object here and to [fuse_confidence()], so the
#'   prompt and the final fused score are guaranteed to agree.
#' @param dataset_context An optional dataset context, as built by
#'   [build_dataset_context()] / [run_dataset_context()], threaded into
#'   [build_user_prompt()]. `NULL` (default) omits the DATASET CONTEXT block.
#' @return An `interpretation` object (not yet faithfulness-checked or
#'   confidence-fused; see [synthesize_module()] for the full pipeline).
#' @examples
#' ms <- llegir_example_moduleset()
#' packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
#' desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
#' synthesize_interpretation(packet, desc, mock_backend())
#' @export
synthesize_interpretation <- function(packet, desc, backend, temperature = 0, seed = NA_real_,
                                       prompt_template_version = PROMPT_TEMPLATE_VERSION,
                                       schema_path = system.file('schemas', 'interpretation.schema.json', package = 'llegir'),
                                       user_weights = list(), fusion = NULL, dataset_context = NULL){
    validate_dataset_description(desc)
    fusion <- fusion %||% calculate_fusion_score(packet$fragments, user_weights = user_weights)

    system_prompt <- build_system_prompt()
    user_prompt <- build_user_prompt(packet, desc, fusion = fusion, dataset_context = dataset_context)
    schema_json <- model_output_schema_json(schema_path)

    result <- backend(system_prompt, user_prompt, schema_json, packet_hash = packet$packet_hash)
    raw <- result$content

    model_score <- raw$confidence$score
    confidence <- list(score = model_score, model_score = model_score, rationale = raw$confidence$rationale)

    provenance <- make_interpretation_provenance(
        model = result$meta$model %||% 'unknown',
        model_version = result$meta$model_version %||% NA_character_,
        prompt_template_version = prompt_template_version,
        temperature = temperature,
        seed = seed,
        input_packet_hash = packet$packet_hash,
        ellmer_call = result$meta$ellmer_call %||% list()
    )

    to_claim <- function(claim){
        list(
            claim = claim$claim,
            fragment_ids = unlist(claim$fragment_ids),
            direction = claim$direction %||% 'na',
            strength = claim$strength %||% NA_real_
        )
    }

    interp <- interpretation(
        module_id = packet$module_id,
        proposed_label = raw$proposed_label,
        one_line_summary = raw$one_line_summary,
        dominant_biology = raw$dominant_biology,
        supporting_claims = lapply(raw$supporting_claims, to_claim),
        confidence = confidence,
        provenance = provenance,
        cell_state = raw$cell_state %||% NA_character_,
        condition_dynamics = raw$condition_dynamics %||% NA_character_,
        metadata_associations = raw$metadata_associations %||% list(),
        literature = list(),
        flags = unlist(raw$flags) %||% list()
    )
    validate_interpretation(interp)
    interp
}
