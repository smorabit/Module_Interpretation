## geneset_enrichment: Enrichr GO/pathway enrichment over hub genes.
## NETWORK-DEPENDENT: calls the public Enrichr API (maayanlab.cloud); this is the
## one tool in the core set that isn't offline/deterministic (docs/CLAUDE.md ground
## rules flag this explicitly). A network failure degrades to a low-evidence
## fragment (compact_summary notes why) instead of aborting the orchestrator run.

suppressPackageStartupMessages(library(enrichR))

geneset_enrichment_tool <- function(ctx){
    n_hubs <- ctx$params$n_hubs %||% 25
    databases <- ctx$params$databases %||% c('GO_Biological_Process_2023')

    gm <- gene_membership(ctx$ms, ctx$module_id)
    genes <- head(gm$gene_name, n_hubs)

    enrich_result <- tryCatch({
        setEnrichrSite('Enrichr')
        do.call(rbind, enrichr(genes, databases = databases))
    }, error = function(e) e)

    provenance <- make_provenance(
        tool_version = '0.1',
        params = list(n_hubs = n_hubs, databases = databases, network_required = TRUE),
        pkg_versions = pkg_versions(ctx$ms)
    )

    if (inherits(enrich_result, 'error') || is.null(enrich_result) || nrow(enrich_result) == 0) {
        note <- if (inherits(enrich_result, 'error')) conditionMessage(enrich_result) else 'no terms returned'
        return(evidence_fragment(
            fragment_id = 'geneset_enrichment',
            tool_id = 'geneset_enrichment',
            module_id = ctx$module_id,
            type = 'geneset_enrichment',
            result = data.frame(),
            compact_summary = paste0('enrichment unavailable: ', note),
            top_findings = list(),
            effect_strength = 0,
            direction = 'na',
            provenance = provenance
        ))
    }

    enrich_result <- enrich_result[order(enrich_result$Adjusted.P.value), ]
    top <- head(enrich_result, 20)

    top_findings <- lapply(seq_len(min(5, nrow(top))), function(i){
        list(term = top$Term[i], adj_p = top$Adjusted.P.value[i], odds_ratio = top$Odds.Ratio[i])
    })

    compact_summary <- paste0('top enriched terms: ', paste(head(top$Term, 5), collapse = '; '))

    # floor to avoid -log10(0) = Inf, which jsonlite can't round-trip
    min_p <- max(min(top$Adjusted.P.value), 1e-300)

    evidence_fragment(
        fragment_id = 'geneset_enrichment',
        tool_id = 'geneset_enrichment',
        module_id = ctx$module_id,
        type = 'geneset_enrichment',
        result = top,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = -log10(min_p),
        significance = min(top$Adjusted.P.value),
        direction = 'na',
        provenance = provenance
    )
}
