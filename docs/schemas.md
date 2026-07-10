# Data Contracts — Schemas

← [Overview](overview.md) · [Implementation guide](implementation_guide.md) · [Milestone 1](milestone_1.md)

*Status: Draft v0.1, 2026-07. These are the two interfaces everything depends on — change them deliberately and bump the version.*

---

Two contracts:

1. **Evidence fragment** — produced by every tool, consumed by the synthesis layer.
2. **Interpretation object** — produced by the synthesis layer, rendered into the paragraph.

Serialized as JSON for portability and hashing; constructed/validated in R via helper functions. R uses snake_case, list-based objects with an S3 class tag, and validators (see [STYLE.md](../STYLE.md)).

---

## 1. Evidence fragment

One fragment = one tool's result for one module.

### Fields

| Field | Type | Required | Notes |
|---|---|---|---|
| `fragment_id` | string | yes | unique within a packet, e.g. `"cluster_dme"` or `"metadata::diagnosis"` |
| `tool_id` | string | yes | which tool produced it |
| `module_id` | string | yes | the module this describes |
| `type` | enum | yes | controlled vocab (below) |
| `result` | table | yes | full tidy result (data.frame) |
| `compact_summary` | string | yes | short digest for the model (token-efficient, no raw tables) |
| `top_findings` | list | yes | the few most salient items (genes / terms / groups) with values |
| `effect_strength` | number | yes | comparable magnitude, e.g. `max(abs(r))`, top `log2FC`, top `-log10(FDR)` |
| `significance` | number | no | p / FDR where applicable |
| `direction` | enum | no | `up` / `down` / `mixed` / `na` — for signed effects |
| `provenance` | object | yes | see below |

### `type` controlled vocabulary

`ranked_genes`, `categorical_association`, `continuous_correlation`, `geneset_enrichment`, `signature_correlation`, `cross_condition_delta`, `state_expression`. Extend deliberately; the synthesis prompt is written against this vocab.

### `provenance` object

`{ tool_version, params (named list), input_hashes (named list), pkg_versions (named list), timestamp }`

### R constructor (sketch)

```r
evidence_fragment <- function(fragment_id, tool_id, module_id, type,
                              result, compact_summary, top_findings,
                              effect_strength, significance = NA_real_,
                              direction = 'na', provenance = list()){
    frag <- list(
        fragment_id = fragment_id,
        tool_id = tool_id,
        module_id = module_id,
        type = match.arg(type, .fragment_types),
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        effect_strength = effect_strength,
        significance = significance,
        direction = match.arg(direction, c('up', 'down', 'mixed', 'na')),
        provenance = provenance
    )
    structure(frag, class = 'evidence_fragment')
}
validate_evidence_fragment <- function(frag){ ... }   # assert required fields, types
```

An **evidence packet** for a module is `list(module_id, fragments = list(<evidence_fragment>, ...), packet_hash)`.

---

## 2. Interpretation object

One per module, filled by the model via structured output.

### Fields

| Field | Type | Required | Notes |
|---|---|---|---|
| `module_id` | string | yes | |
| `proposed_label` | string | yes | short program name, e.g. "Interferon response" |
| `one_line_summary` | string | yes | |
| `dominant_biology` | string | yes | the main program |
| `supporting_claims` | list | yes | each: `{ claim, fragment_ids[], direction, strength }` |
| `cell_state` | string | no | where expressed (from `cluster_dme`) |
| `condition_dynamics` | string | no | if applicable |
| `metadata_associations` | list | no | each: `{ variable, summary, fragment_id }` |
| `literature` | list | no | each: `{ statement, pmids[] }` |
| `confidence` | object | yes | `{ score (0–1), rationale }` |
| `flags` | list | no | subset of the flag vocab (below) |

### `flags` vocabulary

`insufficient_evidence`, `needs_human_review`, `possible_artifact`, `tool_conflict`, `label_low_specificity`.

### Faithfulness invariant

Every `fragment_id` referenced in `supporting_claims` and `metadata_associations` **must exist** in the module's evidence packet, and its `direction` **must match** the fragment's. This is checked programmatically (§4 of the [implementation guide](implementation_guide.md)); a mismatch is a hard error, not a warning.

### Rendering

The boilerplate paragraph is produced by a deterministic template that reads only interpretation-object fields — no additional model calls. Template lives with the code; versioned.

---

## 3. JSON Schema (authoritative, abbreviated)

Keep a real `evidence_fragment.schema.json` and `interpretation.schema.json` in the repo under `schemas/` (move to `inst/schemas/` once it's packaged later). Sketch:

```json
{
  "$id": "evidence_fragment.schema.json",
  "type": "object",
  "required": ["fragment_id","tool_id","module_id","type","result",
               "compact_summary","top_findings","effect_strength","provenance"],
  "properties": {
    "type": {"enum": ["ranked_genes","categorical_association","continuous_correlation",
                       "geneset_enrichment","signature_correlation",
                       "cross_condition_delta","state_expression"]},
    "direction": {"enum": ["up","down","mixed","na"]},
    "effect_strength": {"type": "number"}
  }
}
```

---

## Versioning

Both schemas carry a `schema_version`. Evidence packets and interpretation objects record the version they were produced under. Breaking changes bump the major version and are noted here.

---

*Last updated: 2026-07-10*
