## prompt assembly: evidence packet -> compact model input (docs/milestone_2.md
## task 1). Compact rendering only (compact_summary + top_findings +
## effect_strength/significance) -- never the raw `result` tables.

#' Prompt template version
#'
#' Bumped whenever the system prompt or the compact packet rendering shape
#' changes; recorded in `interpretation$provenance$prompt_template_version`
#' for reproducibility.
#'
#' @export
PROMPT_TEMPLATE_VERSION <- '0.4'

# net directional sign of the pooled evidence mass, for the matrix's
# CONSTRAINTS block -- mirrors R/confidence.R's .directional_coherence()
# numerator (before abs()), so the reported sign always matches the C_dir
# magnitude computed alongside it
.net_direction_label <- function(matrix){
    sign_map <- c(up = 1, down = -1, mixed = 0, na = 0)
    sigma <- sign_map[matrix$direction]
    mass <- matrix$weight * matrix$e_score
    net <- sum(sigma * mass)
    if (net > 0) 'up' else if (net < 0) 'down' else 'none'
}

# fixed-width EVIDENCE CONFIDENCE MATRIX block (milestone_fused_confidence.md
# S6): the model explains this pre-computed matrix rather than inventing its
# own confidence, and fuse_confidence() re-derives the final score from the
# same `fusion` object regardless of what the model writes here
.render_confidence_matrix <- function(fusion){
    m <- fusion$matrix
    header <- sprintf('%-18s %-18s %6s %9s %11s %7s %s', 'fragment_id', 'type', 'weight', 'magnitude', 'reliability', 'e_score', 'direction')
    rows <- vapply(seq_len(nrow(m)), function(i){
        sprintf(
            '%-18s %-18s %6.2f %9.2f %11.2f %7.2f %s',
            m$fragment_id[i], m$type[i], m$weight[i], m$magnitude[i], m$reliability[i], m$e_score[i], m$direction[i]
        )
    }, character(1))

    paste(
        'EVIDENCE CONFIDENCE MATRIX  (computed deterministically upstream -- treat as ground truth, do not recompute or contradict)',
        '',
        header,
        paste(rows, collapse = '\n'),
        '',
        sprintf('pooled_evidence  E_pool  = %.2f   (weighted power mean, beta = %.2f)', fusion$e_pool, fusion$params$beta),
        sprintf('directional      C_dir   = %.2f  ->  P_agree = %.2f', fusion$c_dir, fusion$p_agree),
        sprintf('empirical        E_evidence      = %.2f', fusion$e_evidence),
        sprintf('model_trust      lambda          = %.2f', fusion$lambda),
        '',
        'CONSTRAINTS:',
        sprintf('- confidence.score must be consistent with E_evidence; it may not exceed E_evidence + 0.10 (E_evidence = %.2f).', fusion$e_evidence),
        sprintf('- Any directional claim must agree with the sign of the directional mass above (coherence %.2f, net "%s").', fusion$c_dir, .net_direction_label(m)),
        sprintf('- If E_evidence < %.2f, set flags to include insufficient_evidence and keep supporting_claims minimal.', 0.35),
        '- Explain what the numbers mean for this module; do not restate or recompute them.',
        sep = '\n'
    )
}

# a fragment's compact block: one header line (id/type/direction/effect/sig),
# the compact_summary, and up to `max_findings` top_findings as compact JSON;
# already-curated fragments (tools only keep the top few) rarely hit the cap,
# it's a backstop for token efficiency, not the primary truncation
.render_fragment_compact <- function(frag, max_findings = 8){
    sig <- if (is.null(frag$significance) || is.na(frag$significance)) 'NA' else format(frag$significance, digits = 4)
    header <- sprintf(
        '[%s] type=%s direction=%s effect_strength=%s significance=%s',
        frag$fragment_id, frag$type, frag$direction, format(frag$effect_strength, digits = 4), sig
    )
    findings <- utils::head(frag$top_findings, max_findings)
    findings_json <- jsonlite::toJSON(findings, auto_unbox = TRUE, na = 'null')
    paste(header, frag$compact_summary, paste0('top_findings: ', findings_json), sep = '\n')
}

#' Render an evidence packet as a compact model-facing text block
#'
#' Renders only the compact, curated fields of each fragment
#' (`compact_summary`, `top_findings`, `effect_strength`/`significance`) --
#' never the raw `result` tables -- so the prompt stays small and the model
#' only sees pre-summarized evidence.
#'
#' @param packet An evidence packet, as built by [build_evidence_packet()].
#' @param max_findings Maximum number of `top_findings` entries rendered per
#'   fragment; a token-efficiency backstop, since tools already curate
#'   `top_findings` to a handful of entries.
#' @return A single character string.
#' @examples
#' ms <- llegir_example_moduleset()
#' packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
#' cat(render_packet_compact(packet))
#' @export
render_packet_compact <- function(packet, max_findings = 8){
    blocks <- vapply(packet$fragments, .render_fragment_compact, character(1), max_findings = max_findings)
    paste0(
        'Module ', packet$module_id, ' evidence packet (', length(packet$fragments), ' fragments):\n\n',
        paste(blocks, collapse = '\n\n')
    )
}

# a dataset_fragment's compact block: mirrors .render_fragment_compact() but
# there's no direction/effect_strength/significance (dataset_fragment drops
# the fusion fields), and caveats are appended when present
.render_dataset_fragment_compact <- function(frag, max_findings = 8){
    header <- sprintf('[%s] type=%s', frag$fragment_id, frag$type)
    findings <- utils::head(frag$top_findings, max_findings)
    findings_json <- jsonlite::toJSON(findings, auto_unbox = TRUE, na = 'null')
    lines <- c(header, frag$compact_summary, paste0('top_findings: ', findings_json))
    if (length(frag$caveats) > 0) {
        lines <- c(lines, paste0('caveats: ', paste(unlist(frag$caveats), collapse = ', ')))
    }
    paste(lines, collapse = '\n')
}

#' Render a dataset context as a compact model-facing text block
#'
#' Renders only the compact, curated fields of each `dataset_fragment`
#' (`compact_summary`, `top_findings`, `caveats`) -- never the raw `result`
#' tables -- mirroring [render_packet_compact()]. Injected once per dataset
#' into every module's prompt via [build_user_prompt()]'s `dataset_context`
#' argument; unlike the per-module evidence packet, this block is global
#' framing and never enters fusion or faithfulness.
#'
#' @param dataset_context A dataset context, as built by [build_dataset_context()].
#' @param max_findings Maximum number of `top_findings` entries rendered per
#'   fragment. Default `8`.
#' @return A single character string.
#' @examples
#' frag <- dataset_fragment(
#'     'composition', 'dataset_composition_tool', 'composition_summary',
#'     data.frame(group = 'A', n = 10), 'One group, 10 cells.', list(),
#'     provenance = make_provenance('dataset_composition_tool', '0.1', list(), list(), NA_character_)
#' )
#' ctx <- build_dataset_context(list(frag))
#' cat(render_dataset_context_compact(ctx))
#' @export
render_dataset_context_compact <- function(dataset_context, max_findings = 8){
    blocks <- vapply(
        dataset_context$dataset_fragments, .render_dataset_fragment_compact,
        character(1), max_findings = max_findings
    )
    paste0(
        'DATASET CONTEXT (', length(dataset_context$dataset_fragments), ' fragments):\n\n',
        paste(blocks, collapse = '\n\n')
    )
}

#' Build the synthesis system prompt
#'
#' A fixed set of rules governing how a backend must fill the model-facing
#' interpretation schema: evidence-only claims, citation requirements,
#' direction consistency, controlled vocabularies, the empty-literature
#' constraint, and how `confidence.score` must relate to the deterministic
#' EVIDENCE CONFIDENCE MATRIX ([calculate_fusion_score()]) injected into the
#' user prompt by [build_user_prompt()].
#'
#' @return A single character string.
#' @examples
#' cat(build_system_prompt())
#' @export
build_system_prompt <- function(){
    paste(
        'You are filling a structured interpretation of one gene co-expression module',
        'from a fixed evidence packet produced by a deterministic analysis pipeline.',
        '',
        'Rules:',
        '- Use only the evidence given below. Do not invent genes, terms, or results, and do not run or imagine any analysis.',
        '- Every entry in supporting_claims must cite the fragment_id(s) it is based on, and its direction must match the direction reported by those fragments.',
        '- A single supporting_claims entry may only cite fragment_ids that all share the same direction. ranked_genes fragments (e.g. top_genes) always report direction na, so never combine one in the same claim as a directional fragment (e.g. geneset_enrichment, direction up/down) -- cite them as separate supporting_claims entries instead, each using the direction its own fragment(s) actually report.',
        '- metadata_associations entries must cite a real fragment_id the same way.',
        paste0('- Each fragment has a type from a controlled vocabulary: ', paste(.fragment_types, collapse = ', '), '.'),
        paste0('- flags must be drawn only from: ', paste(.interpretation_flags, collapse = ', '), '.'),
        '- If the evidence is weak, sparse, or inconsistent, do not invent a confident story: set flags to include insufficient_evidence, keep supporting_claims minimal (or empty), and give a low confidence score.',
        '- The user prompt includes an EVIDENCE CONFIDENCE MATRIX computed deterministically upstream: treat it as ground truth, do not recompute or contradict it.',
        '- confidence.score must be consistent with the matrix\'s E_evidence: it may not exceed E_evidence + 0.10, and must fall below the matrix\'s stated insufficient_evidence threshold when E_evidence does.',
        '- Every quantitative certainty statement in your response must reference E_evidence rather than restating your own separate estimate.',
        '- You may not assert a direction (e.g. in dominant_biology or condition_dynamics) that contradicts the sign of the directional coherence reported in the matrix.',
        '- literature must be left empty; literature grounding is not available in this pipeline.',
        '- A DATASET CONTEXT block, when present, is global framing (composition, variance structure, and similar dataset-wide context) for confounder awareness -- it is not a per-module fragment, so never cite it in supporting_claims or metadata_associations.',
        sep = '\n'
    )
}

#' Build the synthesis user prompt for one module
#'
#' Concatenates the rendered [dataset_description()], the optional
#' [render_dataset_context_compact()] block, the compact evidence packet
#' ([render_packet_compact()]), and the deterministic EVIDENCE CONFIDENCE
#' MATRIX ([calculate_fusion_score()]) that grounds the model's
#' `confidence.score` in the same numbers [fuse_confidence()] later
#' re-derives the final fused score from.
#'
#' @param packet An evidence packet, as built by [build_evidence_packet()].
#' @param desc A `dataset_description`; see [dataset_description()].
#' @param fusion An optional pre-computed [calculate_fusion_score()] result;
#'   `NULL` (default) computes it from `packet$fragments` and `user_weights`.
#'   Pass the same object used later by [fuse_confidence()] so the prompt and
#'   the final score are guaranteed to agree.
#' @param data_level Observation-unit descriptor of the `ModuleSet` the
#'   packet was built from; see [render_dataset_description()]. Default `'cell'`.
#' @param aggregated Whether that `ModuleSet`'s expression/scores are already
#'   aggregated across cells; see [render_dataset_description()]. Default `FALSE`.
#' @param user_weights Named list of per-`tool_id` weight multipliers passed
#'   to [calculate_fusion_score()] when `fusion` is `NULL`. Default `list()`.
#' @param dataset_context An optional dataset context, as built by
#'   [build_dataset_context()] / [run_dataset_context()]. Rendered via
#'   [render_dataset_context_compact()] between the dataset description and
#'   the evidence packet. `NULL` (default) omits the block entirely, matching
#'   prior prompt output exactly.
#' @return A single character string.
#' @examples
#' ms <- llegir_example_moduleset()
#' packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
#' desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
#' cat(build_user_prompt(packet, desc))
#' @export
build_user_prompt <- function(packet, desc, fusion = NULL, data_level = 'cell', aggregated = FALSE,
                               user_weights = list(), dataset_context = NULL){
    fusion <- fusion %||% calculate_fusion_score(packet$fragments, user_weights = user_weights)
    dataset_block <- if (is.null(dataset_context)) NULL else render_dataset_context_compact(dataset_context)
    blocks <- Filter(Negate(is.null), list(
        render_dataset_description(desc, data_level = data_level, aggregated = aggregated),
        dataset_block,
        render_packet_compact(packet),
        .render_confidence_matrix(fusion)
    ))
    paste(blocks, collapse = '\n\n')
}
