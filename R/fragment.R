## evidence_fragment contract: constructor, validator, JSON (de)serialization,
## and evidence-packet assembly + hashing. docs/schemas.md,
## inst/schemas/evidence_fragment.schema.json

# controlled vocab for evidence_fragment$type (docs/schemas.md); extend deliberately,
# the (future) synthesis prompt is written against this list
.fragment_types <- c(
    'ranked_genes', 'categorical_association', 'continuous_correlation',
    'geneset_enrichment', 'signature_correlation', 'cross_condition_delta',
    'state_expression'
)

.direction_types <- c('up', 'down', 'mixed', 'na')

#' Construct an evidence fragment
#'
#' One tool's result for one module; every core/custom tool must return one
#' of these. See `vignette('getting-started', package = 'llegir')` and
#' `inst/schemas/evidence_fragment.schema.json` for the full contract.
#'
#' @param fragment_id Unique id within a packet, e.g. `'cluster_dme'` or
#'   `'metadata::diagnosis'`.
#' @param tool_id Which tool produced this fragment.
#' @param module_id The module this fragment describes.
#' @param type One of the controlled vocabulary: `'ranked_genes'`,
#'   `'categorical_association'`, `'continuous_correlation'`,
#'   `'geneset_enrichment'`, `'signature_correlation'`,
#'   `'cross_condition_delta'`, `'state_expression'`.
#' @param result The full tidy result (a data.frame).
#' @param compact_summary Short digest for the model (token-efficient, no raw tables).
#' @param top_findings A list of the few most salient items (genes / terms / groups).
#' @param effect_strength A comparable magnitude, e.g. `max(abs(r))`, top
#'   `log2FC`, or top `-log10(FDR)`.
#' @param significance p / FDR where applicable, else `NA_real_`.
#' @param direction One of `'up'`, `'down'`, `'mixed'`, `'na'`.
#' @param provenance A provenance list, typically built with [make_provenance()].
#' @return An `evidence_fragment` object.
#' @export
evidence_fragment <- function(fragment_id, tool_id, module_id, type, result,
                               compact_summary, top_findings, effect_strength,
                               significance = NA_real_, direction = 'na',
                               provenance = list()){
    type <- match.arg(type, .fragment_types)
    direction <- match.arg(direction, .direction_types)
    frag <- list(
        fragment_id = fragment_id,
        tool_id = tool_id,
        module_id = module_id,
        type = type,
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = effect_strength,
        significance = significance,
        direction = direction,
        provenance = provenance
    )
    structure(frag, class = 'evidence_fragment')
}

#' Validate an evidence fragment
#'
#' Asserts required fields and basic types against
#' `inst/schemas/evidence_fragment.schema.json`.
#'
#' @param frag An `evidence_fragment` object.
#' @return `TRUE`, invisibly, on success. Throws on the first violation.
#' @export
validate_evidence_fragment <- function(frag){
    required <- c(
        'fragment_id', 'tool_id', 'module_id', 'type', 'result',
        'compact_summary', 'top_findings', 'effect_strength', 'provenance'
    )
    missing_fields <- setdiff(required, names(frag))
    if (length(missing_fields) > 0) {
        stop('evidence_fragment missing required fields: ', paste(missing_fields, collapse = ', '))
    }
    if (!inherits(frag, 'evidence_fragment')) stop('object is not class evidence_fragment')
    if (!is.character(frag$fragment_id) || length(frag$fragment_id) != 1) stop('fragment_id must be a single string')
    if (!is.character(frag$tool_id) || length(frag$tool_id) != 1) stop('tool_id must be a single string')
    if (!is.character(frag$module_id) || length(frag$module_id) != 1) stop('module_id must be a single string')
    if (!(frag$type %in% .fragment_types)) stop('invalid type: ', frag$type)
    if (!is.data.frame(frag$result)) stop('result must be a data.frame')
    if (!is.character(frag$compact_summary) || length(frag$compact_summary) != 1) stop('compact_summary must be a single string')
    if (!is.list(frag$top_findings)) stop('top_findings must be a list')
    if (!is.numeric(frag$effect_strength) || length(frag$effect_strength) != 1) stop('effect_strength must be a single number')
    if (length(frag$significance) != 1 || !(is.numeric(frag$significance) || is.na(frag$significance))) {
        stop('significance must be a single number or NA')
    }
    direction <- frag$direction
    if (is.null(direction)) direction <- 'na'
    if (!(direction %in% .direction_types)) stop('invalid direction: ', direction)
    if (!is.list(frag$provenance)) stop('provenance must be a list')
    prov_required <- c('tool_version', 'params', 'input_hashes', 'pkg_versions', 'timestamp')
    prov_missing <- setdiff(prov_required, names(frag$provenance))
    if (length(prov_missing) > 0) {
        stop('provenance missing fields: ', paste(prov_missing, collapse = ', '))
    }
    invisible(TRUE)
}

# strip volatile fields (timestamps) before hashing so identical evidence
# hashes identically across reruns
.fragment_hashable <- function(frag){
    frag$provenance$timestamp <- NULL
    unclass(frag)
}

#' Serialize an evidence fragment to JSON
#'
#' @param frag An `evidence_fragment` object.
#' @param pretty Pretty-print the JSON. Default `TRUE`.
#' @return A JSON string (a `jsonlite::json` scalar).
#' @export
fragment_to_json <- function(frag, pretty = TRUE){
    # `unclass()` drops the S3 tag so jsonlite serializes the fragment as a plain
    # object; `dataframe = 'rows'` keeps `result` as a row-oriented JSON array
    jsonlite::toJSON(unclass(frag), dataframe = 'rows', auto_unbox = TRUE, na = 'null', pretty = pretty)
}

#' Parse an evidence fragment from JSON
#'
#' Inverse of [fragment_to_json()]; rebuilds the `evidence_fragment` class and
#' defaults for fields that may have been dropped by JSON's null handling.
#'
#' @param json_str A JSON string as produced by [fragment_to_json()].
#' @return An `evidence_fragment` object.
#' @export
fragment_from_json <- function(json_str){
    parsed <- jsonlite::fromJSON(json_str, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    do.call(evidence_fragment, list(
        fragment_id = parsed$fragment_id,
        tool_id = parsed$tool_id,
        module_id = parsed$module_id,
        type = parsed$type,
        result = as.data.frame(parsed$result),
        compact_summary = parsed$compact_summary,
        top_findings = parsed$top_findings,
        effect_strength = parsed$effect_strength,
        significance = ifelse(is.null(parsed$significance), NA_real_, parsed$significance),
        direction = ifelse(is.null(parsed$direction), 'na', parsed$direction),
        provenance = parsed$provenance
    ))
}

#' Assemble and hash a module's evidence packet
#'
#' Validates every fragment, then hashes the content (fragments minus
#' timestamps) so the hash is a reproducibility fingerprint, not just a
#' run-to-run-unique id.
#'
#' @param module_id The module this packet describes.
#' @param fragments A list of `evidence_fragment` objects.
#' @param input_hash A content hash identifying the source dataset (e.g. of
#'   the backing `.rds`), for provenance.
#' @param schema_version Schema version tag. Default `'0.1'`.
#' @param skipped A list of `list(tool_id, reason)` entries for tools that
#'   were skipped because a required `ModuleSet` [capabilities()] was unmet
#'   (see [run_module()]), recorded on `provenance$skipped` for an audit
#'   trail of what didn't run and why. Default `list()`.
#' @return A list with `module_id`, `fragments`, `packet_hash`,
#'   `schema_version`, and `provenance`.
#' @export
build_evidence_packet <- function(module_id, fragments, input_hash = NA_character_, schema_version = '0.1', skipped = list()){
    lapply(fragments, validate_evidence_fragment)
    packet_hash <- digest::digest(lapply(fragments, .fragment_hashable), algo = 'sha256')
    list(
        module_id = module_id,
        fragments = fragments,
        packet_hash = packet_hash,
        schema_version = schema_version,
        provenance = list(
            created_at = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z'),
            input_hash = input_hash,
            tool_ids = vapply(fragments, function(f) f$tool_id, character(1)),
            skipped = skipped
        )
    )
}

#' Serialize an evidence packet to JSON
#'
#' @param packet An evidence packet, as returned by [build_evidence_packet()].
#' @param pretty Pretty-print the JSON. Default `TRUE`.
#' @return A JSON string (a `jsonlite::json` scalar).
#' @export
packet_to_json <- function(packet, pretty = TRUE){
    jsonlite::toJSON(
        list(
            module_id = packet$module_id,
            fragments = lapply(packet$fragments, unclass),
            packet_hash = packet$packet_hash,
            schema_version = packet$schema_version,
            provenance = packet$provenance
        ),
        dataframe = 'rows', auto_unbox = TRUE, na = 'null', pretty = pretty
    )
}

#' Write an evidence packet to a JSON file
#'
#' @param packet An evidence packet, as returned by [build_evidence_packet()].
#' @param path Output file path.
#' @return `path`, invisibly.
#' @export
write_evidence_packet <- function(packet, path){
    writeLines(packet_to_json(packet), path)
    invisible(path)
}

#' Read an evidence packet from a JSON file
#'
#' Reconstructs a packet (and each fragment's S3 class) from a JSON file
#' written by [write_evidence_packet()].
#'
#' @param path Path to a packet JSON file.
#' @return An evidence packet, as returned by [build_evidence_packet()].
#' @export
read_evidence_packet <- function(path){
    # jsonlite simplifies the fragments array into a data.frame, so fragments
    # are indexed row-by-row. JSON *array* fields (result, top_findings)
    # become list-columns, so `[[1]]` unwraps the one element for this row;
    # the `provenance` field is a JSON *object*, so jsonlite instead
    # simplifies it into its own nested data.frame keyed by column, and a
    # one-row slice of that is already this fragment's record (as.list(),
    # not `[[1]]`)
    parsed <- jsonlite::fromJSON(path, simplifyDataFrame = TRUE, simplifyVector = TRUE)
    fragments <- lapply(seq_len(nrow(parsed$fragments)), function(i) {
        f <- parsed$fragments[i, ]
        do.call(evidence_fragment, list(
            fragment_id = f$fragment_id[[1]],
            tool_id = f$tool_id[[1]],
            module_id = f$module_id[[1]],
            type = f$type[[1]],
            result = as.data.frame(f$result[[1]]),
            compact_summary = f$compact_summary[[1]],
            top_findings = f$top_findings[[1]],
            effect_strength = f$effect_strength[[1]],
            significance = ifelse(is.null(f$significance[[1]]) || length(f$significance[[1]]) == 0, NA_real_, f$significance[[1]]),
            direction = ifelse(is.null(f$direction[[1]]), 'na', f$direction[[1]]),
            provenance = as.list(f$provenance)
        ))
    })
    list(
        module_id = parsed$module_id,
        fragments = fragments,
        packet_hash = parsed$packet_hash,
        schema_version = parsed$schema_version,
        provenance = parsed$provenance
    )
}

#' Write every fragment's full result table alongside a packet
#'
#' Writes each fragment's full result table to
#' `<tables_dir>/<module_id>/<fragment_id>.tsv`, so a human can audit any DME
#' table / enrichment table / overlap directly.
#'
#' @param packet An evidence packet, as returned by [build_evidence_packet()].
#' @param tables_dir Output directory.
#' @return `tables_dir`, invisibly.
#' @export
write_fragment_tables <- function(packet, tables_dir){
    # keyed by fragment_id rather than tool_id, since one tool can produce
    # several fragments per module (e.g. module_by_metadata run once for
    # 'diagnosis' and once for 'sample') and tool_id alone would collide;
    # "::" in a fragment_id isn't safe in filenames everywhere, so it's
    # swapped for "__"
    module_dir <- file.path(tables_dir, packet$module_id)
    dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
    for (frag in packet$fragments) {
        file_stub <- gsub(':', '_', frag$fragment_id)
        path <- file.path(module_dir, paste0(file_stub, '.tsv'))
        utils::write.table(frag$result, path, sep = '\t', row.names = FALSE, quote = FALSE)
    }
    invisible(tables_dir)
}

#' Build a fragment's provenance record
#'
#' @param tool_version Version string for the tool that produced the fragment.
#' @param params Named list of the parameters the tool was called with.
#' @param input_hashes Named list of content hashes of relevant inputs.
#' @param pkg_versions Named list of backend package versions, typically from
#'   the `ModuleSet`'s own [pkg_versions()].
#' @param source `'computed'` (default, tool-produced) or `'user_supplied'`
#'   (set automatically by [import_fragment()]).
#' @param module_method Optional free-form description of how the modules
#'   themselves were generated, e.g. `'cNMF factors, k=20'`; see
#'   [dataset_description()]. Threaded through from [run_module()] /
#'   [run_orchestrator()]'s `module_method` argument, when given. Default `NA`.
#' @return A provenance list suitable for [evidence_fragment()]'s `provenance` argument.
#' @export
make_provenance <- function(tool_version, params = list(), input_hashes = list(), pkg_versions = list(),
                             source = 'computed', module_method = NA_character_){
    list(
        tool_version = tool_version,
        params = params,
        input_hashes = input_hashes,
        pkg_versions = pkg_versions,
        source = source,
        module_method = module_method,
        timestamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')
    )
}
