## geneset_enrichment: offline GO/pathway enrichment over hub genes via
## GeneOverlap, against local GMT gene-set libraries (fgsea::gmtPathways()).
## No runtime network -- recycled from SERPENTINE's run_geneoverlap.R, adapted
## to a single module's hub genes vs. a background of all genes in the
## ModuleSet. Deterministic and CI-clean by construction.

# one db's pathways flattened against `hub_genes`; mirrors the go.nested.list
# loop from run_geneoverlap.R (outer = input_list, inner = pathways), here
# specialized to a single input set so only the inner loop is needed
.geneoverlap_flatten <- function(gmt_file, db_name, hub_genes, genome_size){
    pathways <- fgsea::gmtPathways(gmt_file)
    gom <- GeneOverlap::newGOM(pathways, list(module = hub_genes), genome.size = genome_size)

    do.call(rbind, lapply(seq_along(gom@go.nested.list[[1]]), function(j){
        cur <- gom@go.nested.list[[1]][[j]]
        data.frame(
            term = names(pathways)[j],
            overlap = paste0(length(cur@intersection), '|', length(cur@listA)),
            genes = paste(cur@intersection, collapse = ','),
            pval = cur@pval,
            odds_ratio = cur@odds.ratio,
            jaccard = cur@Jaccard,
            ngenes = length(cur@intersection),
            db = db_name
        )
    }))
}

#' Evidence tool: gene-set enrichment among a module's hub genes
#'
#' Offline GO/pathway enrichment ([GeneOverlap::newGOM()]) over hub genes
#' against local GMT gene-set libraries ([fgsea::gmtPathways()]) -- no
#' runtime network access, so the tool is deterministic and CI-clean by
#' construction. Touches the `ModuleSet` adapter contract
#' ([gene_membership()], [expression()], [pkg_versions()]) plus
#' `ctx$params$db_files`, a named vector of local GMT file paths.
#'
#' @param ctx A tool context list: `list(ms, module_id, params)`, as built by
#'   [run_module()]. `ctx$params$n_hubs` (default 25) is the number of hub
#'   genes tested. `ctx$params$db_files` (required for a non-empty result) is
#'   a named character vector of local GMT file paths, e.g.
#'   `c(GO_BP = 'path/to/GO_Biological_Process.txt')`.
#' @return An `evidence_fragment` of type `'geneset_enrichment'`, or `NULL` if
#'   `ctx$ms` lacks the `expression` capability (see [capabilities()]) -- a
#'   graceful skip, not an error.
#' @examples
#' \dontrun{
#' ms <- llegir_example_moduleset()
#' geneset_enrichment_tool(list(
#'     ms = ms, module_id = modules(ms)[1],
#'     params = list(n_hubs = 10, db_files = c(GO_BP = 'path/to/gene_sets.gmt'))
#' ))
#' }
#' @export
geneset_enrichment_tool <- function(ctx){
    n_hubs <- ctx$params$n_hubs %||% 25
    db_files <- ctx$params$db_files %||% c(GO_BP = 'data/GO_Biological_Process_2026.txt')

    if (!has_capability(ctx$ms, 'expression')) {
        message('geneset_enrichment: skipped, module set lacks the expression capability')
        return(NULL)
    }

    gm <- gene_membership(ctx$ms, ctx$module_id)
    hub_genes <- utils::head(gm$gene_name, n_hubs)
    genome_size <- nrow(expression(ctx$ms))

    provenance <- make_provenance(
        tool_version = '0.2',
        params = list(n_hubs = n_hubs, db_files = unname(db_files), network_required = FALSE),
        pkg_versions = pkg_versions(ctx$ms),
        module_method = ctx$module_method %||% NA_character_
    )

    overlap_df <- do.call(rbind, lapply(names(db_files), function(db_name){
        .geneoverlap_flatten(db_files[[db_name]], db_name, hub_genes, genome_size)
    }))
    overlap_df <- subset(overlap_df, ngenes > 0)

    if (nrow(overlap_df) == 0) {
        return(evidence_fragment(
            fragment_id = 'geneset_enrichment',
            tool_id = 'geneset_enrichment',
            module_id = ctx$module_id,
            type = 'geneset_enrichment',
            result = data.frame(),
            compact_summary = 'no gene-set overlap found among hub genes',
            top_findings = list(),
            effect_strength = 0,
            direction = 'na',
            provenance = provenance
        ))
    }

    overlap_df$fdr <- stats::p.adjust(overlap_df$pval, method = 'fdr')
    overlap_df <- overlap_df[order(overlap_df$fdr, -overlap_df$odds_ratio), ]
    rownames(overlap_df) <- NULL
    top <- utils::head(overlap_df, 20)

    top_findings <- lapply(seq_len(min(5, nrow(top))), function(i){
        list(term = top$term[i], fdr = top$fdr[i], odds_ratio = top$odds_ratio[i])
    })

    compact_summary <- paste0('top enriched terms: ', paste(utils::head(top$term, 5), collapse = '; '))

    # floor to avoid -log10(0) = Inf, which jsonlite can't round-trip
    min_fdr <- max(min(top$fdr), 1e-300)

    evidence_fragment(
        fragment_id = 'geneset_enrichment',
        tool_id = 'geneset_enrichment',
        module_id = ctx$module_id,
        type = 'geneset_enrichment',
        result = top,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = -log10(min_fdr),
        significance = min(top$fdr),
        direction = 'up',
        provenance = provenance
    )
}
