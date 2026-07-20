## signature_correlation: does this module's activity co-vary with a
## signature's score? The co-variation sibling of geneset_enrichment (which
## asks whether a module CONTAINS a signature's genes, not whether it
## CO-VARIES with them). Reuses Part 1's .score_gene_sets() (UCell/decoupleR,
## from moduleset_gene_list.R) to score a signature library across cells,
## then correlates each signature with the module's own module_scores().
##
## LEVEL: cell-level Pearson r is reported by default as descriptive
## co-variation only (no p attached). When sample_ids is available, the
## correlation is instead computed at the SAMPLE level (mean score per
## sample, via aggregate_by_sample()) and its p-value is used -- cells within
## a sample are correlated, so a cell-level p on module activity would be
## inflated (the milestone 1.5 lesson; same fix as module_by_metadata_tool).

# a signature library file: named gene sets from a local .gmt
# (fgsea::gmtPathways) or .rds (a named list of character vectors)
.read_signature_library <- function(path){
    if (grepl('\\.rds$', path, ignore.case = TRUE)) {
        readRDS(path)
    } else {
        fgsea::gmtPathways(path)
    }
}

#' Evidence tool: correlate module activity with a signature library
#'
#' The co-variation sibling of [geneset_enrichment_tool()]: overlap asks
#' whether a module *contains* a signature's genes; this tool asks whether
#' the module's *activity* co-varies with the signature's. Scores every
#' signature in `ctx$params$library_files` across cells via
#' [UCell::ScoreSignatures_UCell()] or [decoupleR::run_ulm()] (the same
#' scoring [gene_list_ModuleSet()] uses), then correlates each signature's
#' score with this module's [module_scores()].
#'
#' Reports Pearson *r* as descriptive co-variation at the cell level by
#' default. When `ctx$ms` also has the `sample_ids` capability, the
#' correlation (and its p-value) is instead computed at the sample level
#' (mean score per sample, via [aggregate_by_sample()]) -- cells within a
#' sample aren't independent, so a cell-level p-value would be inflated (the
#' milestone 1.5 lesson); a cell-level *r* is still descriptive and fine to
#' report, but a p-value never is.
#'
#' @param ctx A tool context list: `list(ms, module_id, params)`, as built by
#'   [run_module()]. `ctx$params$library_files` (required) is a named
#'   character vector of local `.gmt` or `.rds` signature-library file paths,
#'   e.g. `c(Hallmark = 'data/h.all.v2026.1.Hs.symbols.gmt')`. An `.rds` file
#'   must contain a named list of character vectors (gene sets); a `.gmt` is
#'   read via [fgsea::gmtPathways()]. `ctx$params$method` is `'UCell'`
#'   (default) or `'decoupleR'`. `ctx$params$sample_col` (default
#'   `'sample'`) is the sample-id column used for the sample-level
#'   correlation.
#' @return An `evidence_fragment` of type `'signature_correlation'`, or
#'   `NULL` if `ctx$ms` lacks the `module_scores` or `expression` capability
#'   (see [capabilities()]) -- a graceful skip, not an error.
#' @examples
#' \dontrun{
#' ms <- llegir_example_moduleset()
#' signature_correlation_tool(list(
#'     ms = ms, module_id = modules(ms)[1],
#'     params = list(library_files = c(Hallmark = 'data/h.all.v2026.1.Hs.symbols.gmt'))
#' ))
#' }
#' @export
signature_correlation_tool <- function(ctx){
    library_files <- ctx$params$library_files
    if (is.null(library_files)) stop('signature_correlation requires params$library_files')
    method <- ctx$params$method %||% 'UCell'
    sample_col <- ctx$params$sample_col %||% 'sample'

    if (!has_capability(ctx$ms, 'module_scores') || !has_capability(ctx$ms, 'expression')) {
        message('signature_correlation: skipped, module set lacks the module_scores/expression capability')
        return(NULL)
    }

    provenance <- make_provenance(
        tool_version = '0.1',
        params = list(library_files = unname(library_files), method = method, sample_col = sample_col),
        pkg_versions = pkg_versions(ctx$ms),
        module_method = ctx$module_method %||% NA_character_
    )

    libs <- lapply(library_files, .read_signature_library)
    all_gene_sets <- do.call(c, unname(libs))
    lib_of <- rep(names(library_files), vapply(libs, length, integer(1)))
    names(lib_of) <- names(all_gene_sets)

    if (length(all_gene_sets) == 0) {
        return(evidence_fragment(
            fragment_id = 'signature_correlation',
            tool_id = 'signature_correlation',
            module_id = ctx$module_id,
            type = 'signature_correlation',
            result = data.frame(),
            compact_summary = 'no signatures found in the supplied library files',
            top_findings = list(),
            effect_strength = 0,
            direction = 'na',
            provenance = provenance
        ))
    }

    expr <- expression(ctx$ms)
    module_score <- module_scores(ctx$ms, module = ctx$module_id)
    sig_scores <- .score_gene_sets(all_gene_sets, expr, method = method)

    sample_level <- has_capability(ctx$ms, 'sample_ids')
    sample_id <- if (sample_level) metadata(ctx$ms)[[sample_col]] else NULL
    if (sample_level && is.null(sample_id)) sample_level <- FALSE
    module_agg <- if (sample_level) aggregate_by_sample(module_score, sample_id) else NULL

    result <- do.call(rbind, lapply(colnames(sig_scores), function(sig){
        sig_score <- sig_scores[[sig]]
        keep <- !is.na(module_score) & !is.na(sig_score)
        r_cell <- suppressWarnings(stats::cor(module_score[keep], sig_score[keep]))
        row <- data.frame(
            signature = sig, library = unname(lib_of[[sig]]), r = r_cell,
            n = sum(keep), level = 'cell', p = NA_real_
        )
        if (sample_level) {
            sig_agg <- aggregate_by_sample(sig_score, sample_id)
            merged <- merge(module_agg, sig_agg, by = 'sample', suffixes = c('_module', '_sig'))
            if (nrow(merged) >= 3) {
                test <- suppressWarnings(stats::cor.test(merged$mean_score_module, merged$mean_score_sig))
                row$r <- unname(test$estimate)
                row$p <- test$p.value
                row$n <- nrow(merged)
                row$level <- 'sample'
            }
        }
        row
    }))

    result$fdr <- stats::p.adjust(result$p, method = 'BH')
    result <- result[order(-abs(result$r)), ]
    rownames(result) <- NULL
    top <- result[1, ]

    top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
        list(signature = result$signature[i], library = result$library[i], r = result$r[i], level = result$level[i])
    })

    compact_summary <- paste0(
        'top |r| signatures: ',
        paste(sprintf(
            '%s (r=%.2f, %s)', utils::head(result$signature, 5), utils::head(result$r, 5), utils::head(result$level, 5)
        ), collapse = '; ')
    )

    evidence_fragment(
        fragment_id = 'signature_correlation',
        tool_id = 'signature_correlation',
        module_id = ctx$module_id,
        type = 'signature_correlation',
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = abs(top$r),
        significance = top$fdr,
        direction = if (top$r > 0) 'up' else 'down',
        provenance = provenance
    )
}
