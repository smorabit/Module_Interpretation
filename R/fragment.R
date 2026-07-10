## evidence_fragment contract: constructor, validator, JSON (de)serialization,
## and evidence-packet assembly + hashing. docs/schemas.md

library(jsonlite)
library(digest)

# controlled vocab for evidence_fragment$type (docs/schemas.md); extend deliberately,
# the (future) synthesis prompt is written against this list
.fragment_types <- c(
    'ranked_genes', 'categorical_association', 'continuous_correlation',
    'geneset_enrichment', 'signature_correlation', 'cross_condition_delta',
    'state_expression'
)

.direction_types <- c('up', 'down', 'mixed', 'na')

# one tool's result for one module; every core/custom tool must return one of these
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

# asserts required fields + basic types (schemas/evidence_fragment.schema.json);
# throws on the first violation, returns TRUE invisibly on success
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

# `unclass()` drops the S3 tag so jsonlite serializes the fragment as a plain
# object; `dataframe = 'rows'` keeps `result` as a row-oriented JSON array
fragment_to_json <- function(frag, pretty = TRUE){
    jsonlite::toJSON(unclass(frag), dataframe = 'rows', auto_unbox = TRUE, na = 'null', pretty = pretty)
}

# inverse of fragment_to_json(); rebuilds the evidence_fragment class + defaults
# for fields that may have been dropped by JSON's null handling
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

# one module's evidence packet: validates every fragment, then hashes the
# content (fragments minus timestamps) so the hash is a reproducibility
# fingerprint, not just a run-to-run-unique id. `input_hash` identifies the
# source dataset (e.g. a content hash of the .rds); the per-fragment
# provenance already carries each tool's own params/pkg_versions, so the
# packet-level manifest just adds the assembly-time context (which tools ran,
# against which input) rather than duplicating it
build_evidence_packet <- function(module_id, fragments, input_hash = NA_character_, schema_version = '0.1'){
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
            tool_ids = vapply(fragments, function(f) f$tool_id, character(1))
        )
    )
}

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

write_evidence_packet <- function(packet, path){
    writeLines(packet_to_json(packet), path)
    invisible(path)
}

# reconstructs a packet (and each fragment's S3 class) from a JSON file written
# by write_evidence_packet(); indexes fragments row-by-row since jsonlite
# simplifies the fragments array into a data.frame. JSON *array* fields
# (result, top_findings) become list-columns, so `[[1]]` unwraps the one
# element for this row; the `provenance` field is a JSON *object*, so jsonlite
# instead simplifies it into its own nested data.frame keyed by column, and a
# one-row slice of that is already this fragment's record (as.list(), not `[[1]]`)
read_evidence_packet <- function(path){
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

# `pkg_versions` is supplied by the caller (via the adapter's pkg_versions()),
# not looked up here, so this file stays backend-agnostic
make_provenance <- function(tool_version, params = list(), input_hashes = list(), pkg_versions = list()){
    list(
        tool_version = tool_version,
        params = params,
        input_hashes = input_hashes,
        pkg_versions = pkg_versions,
        timestamp = format(Sys.time(), '%Y-%m-%dT%H:%M:%S%z')
    )
}
