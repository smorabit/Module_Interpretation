# Milestone (dataset tools) — global, dataset-level grounding for synthesis

← [Overview](../overview.md) · [Implementation guide](../implementation_guide.md) · [Schemas](../schemas.md) · [Pseudo-bulk milestone](milestone_pseudobulk.md) · [Fused confidence milestone](milestone_fused_confidence.md) · [Project home](../../README.md)

*Status: planned. Depends on the abstract `ModuleSet` contract (needs `expression()`, `metadata()`, `module_scores()`, `capabilities()`, `data_level`/`aggregated`) and, for the variance-structure tool, [milestone_pseudobulk.md](milestone_pseudobulk.md) (needs `pseudobulk_view()`).*

---

## Goal

Evidence tools today evaluate one module *in a vacuum* (`R/orchestrator.R::run_module()`), which loses global context — a "disease module" is often just a cell type overrepresented in the disease samples, or its hub genes are dataset-wide housekeeping signal. This milestone introduces **dataset-level tools** that extract global, quantitative summaries of the whole experiment and inject them into every module's synthesis prompt as computed grounding.

The key architectural decision: **a dataset summary is not an `evidence_fragment`.** An `evidence_fragment` is contractually *"one tool's result for one module"* — `module_id` is required, and `effect_strength`/`significance`/`direction`/`tier` exist solely to feed the per-module `calculate_fusion_score()` and `enforce_faithfulness()` machinery. A dataset summary has no `module_id` and is descriptive framing, not a signed per-module effect to be fused. It therefore gets a **sibling contract**, the `dataset_fragment`, and threads into the pipeline exactly where `dataset_description` already sits — global context prepended to the prompt, above the per-module evidence packet.

Mental model: **`dataset_context` is a richer, *computed* sibling of `dataset_description`** (`R/dataset_description.R`). One lives in the same prompt slot the other already occupies.

## Scope constraints (from project mission)

- **llegir does not build pseudo-bulk or compute composition from scratch as its primary path.** Consistent with [milestone_pseudobulk.md](milestone_pseudobulk.md) ("the user owns the aggregation"), the **most common real use is the user importing their own compositional / differential-abundance results** (e.g. from `miloR`, `propeller`, `scCODA`, `sccomp`). So the composition surface is *two-pronged*: a lightweight compute-from-metadata tool **and** an import path mirroring the existing `import_fragment()` family (`R/import_fragment.R`).
- **Recycle `sample_code/`.** The variance-structure tool must reuse the principal-component-regression logic already written in `sample_code/pseudobulk_functions.R::PCRegression()` (see [Recycled code](#recycled-code-pointers)), not reinvent it.
- **Zero new adapter surface.** Dataset tools call only the existing `ModuleSet` generics. No new methods are added to the adapter contract — the elegance headline of this milestone.

## Design principles

- **Dataset tools consume the whole matrix; module tools slice it.** The only structural difference from a normal tool is that a dataset tool reads `expression(ms)` / `metadata(ms)` / `module_scores(ms)` *in full* rather than for one module. It returns a **small tidy summary** in `result` — never the raw matrix — exactly the discipline module tools already follow.
- **Framing, not citable evidence.** A `dataset_context` does **not** enter the EVIDENCE CONFIDENCE MATRIX, `calculate_fusion_score()` (`R/confidence.R`), or `enforce_faithfulness()` (`R/faithfulness.R`). Like `dataset_description`, it is context the model reads, not a fragment it cites per-module. This keeps the entire fusion/faithfulness layer untouched.
- **Reuse the tool *shape*, separate the *namespace*.** Dataset tools register through the same `register_tool()` infrastructure (`R/registry.R`) via a new `scope` field, so the register/get/list/capability-skip machinery is reused verbatim — not duplicated — while `run_module()` can never accidentally pull a dataset tool into a per-module loop.
- **Compute once, inject everywhere.** A `dataset_context` is built once per dataset (before the per-module loop) and the same compact rendering is injected into every module's prompt.

## The two contracts side by side

| | `evidence_fragment` (exists) | `dataset_fragment` (new) |
|---|---|---|
| scope | one module | whole dataset — **no `module_id`** |
| `fragment_id`, `tool_id`, `type` | yes | yes — **own** type vocab (below) |
| `result` (tidy df) | yes | yes (small summary only) |
| `compact_summary`, `top_findings` | yes | yes |
| `effect_strength`/`significance`/`direction` | yes (fusion inputs) | **dropped** — descriptive, not fused |
| `caveats` | — | **added** — confounder flags (below) |
| `provenance` | `make_provenance()` | reuse `make_provenance()` verbatim |
| bundled into | evidence packet (per module) | **`dataset_context`** (per dataset) |
| enters fusion/faithfulness? | yes | **no** |

`dataset_fragment$type` controlled vocabulary: `composition_summary`, `baseline_expression`, `variance_structure`, `module_landscape`. Extend deliberately (mirrors `.fragment_types` in `R/fragment.R`).

`dataset_fragment$caveats`: a list of short machine-readable confounder flags surfaced to the model, drawn from a controlled vocab, e.g. `condition_confounded_with_batch`, `cell_state_imbalanced_across_condition`, `hub_genes_are_housekeeping`, `underpowered_contrast`. Start the vocab small; grow per tool.

A **`dataset_context`** object is `list(dataset_fragments = list(<dataset_fragment>, ...), context_hash, schema_version, provenance)` — the once-per-dataset analog of an evidence packet (`build_evidence_packet()` in `R/fragment.R`).

---

## Parts (sequenced; one Claude Code / Sonnet session each)

> **Implementer note (read first):** each part below is self-contained and names the exact files, functions, and existing patterns to mirror. Build strictly on the `ModuleSet` generics in `R/moduleset.R` — never touch `hdWGCNA`/`Seurat` directly. Follow `STYLE.md` (snake_case, single quotes, 4-space indent, `%>%`, Roxygen on exported fns only). Iterate offline on **one** module / the example fixtures; do not call live syntheses (`CLAUDE.md` budget rules).

### Part 1 — The `dataset_fragment` + `dataset_context` contracts *(keystone; first)*

Model this file directly on `R/fragment.R`, which is the reference implementation for the parallel evidence contract.

- **New `R/dataset_fragment.R`** with:
  - `.dataset_fragment_types <- c('composition_summary', 'baseline_expression', 'variance_structure', 'module_landscape')` and `.dataset_caveat_vocab <- c(...)` (start with the four flags above).
  - `dataset_fragment(fragment_id, tool_id, type, result, compact_summary, top_findings, caveats = list(), provenance = list())` constructor — S3 class `dataset_fragment`. **No `module_id`, no `effect_strength`/`significance`/`direction`.**
  - `validate_dataset_fragment(frag)` — mirror `validate_evidence_fragment()`: required fields, types, `type %in% .dataset_fragment_types`, `result` is a data.frame, `caveats` all in vocab, provenance required fields present.
  - `dataset_fragment_to_json()` / `dataset_fragment_from_json()` — mirror `fragment_to_json()`/`fragment_from_json()` (`dataframe = 'rows'`, `auto_unbox = TRUE`, `na = 'null'`).
  - `build_dataset_context(dataset_fragments, input_hash, schema_version = '0.1')` — mirror `build_evidence_packet()`: validate each fragment, hash content minus timestamps via `.fragment_hashable`-style stripping + `digest::digest(..., algo = 'sha256')`, return the `dataset_context` list.
  - `dataset_context_to_json()` / `write_dataset_context()` / `read_dataset_context()` — mirror the packet (de)serializers.
- **Schema stub** `inst/schemas/dataset_fragment.schema.json` already drafted alongside this milestone — flesh out `caveats` enum and keep `schema_version` in lockstep with the R vocab. Update `docs/schemas.md` with a "3. Dataset fragment" section (mirror the evidence-fragment table).
- **Registry `scope`:** extend `register_tool()` (`R/registry.R`) with `scope = 'module'` (default) / `'dataset'`, stored on the `tool_spec`; add `scope` arg to `list_tools()` to filter. `tier` is module-only — document it as ignored when `scope == 'dataset'`. Non-breaking: `run_module()` takes an explicit `tool_config` and never scans the registry, so nothing downstream changes.

Deliverable: `dataset_fragment` / `dataset_context` construct, validate, round-trip through JSON, and hash reproducibly; `register_tool(..., scope = 'dataset')` works and `list_tools('dataset')` filters; `docs/schemas.md` documents the new contract. Offline unit tests in `tests/testthat/test-dataset_fragment.R` (mirror `test-fragment.R`).

### Part 2 — Orchestration + prompt injection *(spine; before any tool)*

Wire the empty contract through the pipeline so a tool built in Part 3+ lights up end-to-end.

- **New `run_dataset_context(ms, dataset_tool_config, input_hash = NA, validate = TRUE)`** — the dataset analog of `run_orchestrator()` (`R/orchestrator.R`), run **once** per dataset. Same shape as `run_module()`: each spec is `list(id, params)` (registry lookup + capability skip via `.tool_spec_requires()` / `has_capability()`) or `list(fn, params)` (direct). `ctx` here is `list(ms = ms, params = spec$params, module_method = module_method)` — **no `module_id`**. Bundle results via `build_dataset_context()`. Reuse the capability-skip + `provenance$skipped` audit pattern from `run_module()`.
- **Prompt injection** in `R/prompt.R`:
  - `render_dataset_context_compact(dataset_context, max_findings = 8)` — mirror `render_packet_compact()` / `.render_fragment_compact()`; render `compact_summary` + `top_findings` + any `caveats` as a `DATASET CONTEXT` block. Never render `result` tables.
  - Add `dataset_context = NULL` param to `build_user_prompt()`. Place the rendered block **after** `render_dataset_description()` and **before** `render_packet_compact()` — global grounding above local evidence. `NULL` ⇒ omit the block entirely (backward compatible).
  - One line in `build_system_prompt()`: instruct the model to treat the DATASET CONTEXT block as global framing / confounder awareness, and to **not** cite it as a per-module fragment.
  - **Bump `PROMPT_TEMPLATE_VERSION`** (`R/prompt.R`).
- **Thread through synthesis** (`R/orchestrator.R`): add `dataset_context = NULL` to `synthesize_module()` and `run_synthesis_orchestrator()`, passed into `build_user_prompt()`. Compute the context once in the batch orchestrator and pass the same object to every module. **Do not touch** `calculate_fusion_score()`, `fuse_confidence()`, or `enforce_faithfulness()`.

Deliverable: `run_dataset_context()` produces a valid `dataset_context`; `build_user_prompt(..., dataset_context = dc)` emits the `DATASET CONTEXT` block in the right position; `synthesize_module()` accepts and threads it; existing prompt tests still pass with `dataset_context = NULL`; new offline test `test-dataset_orchestrator.R`. Verify with the mock backend on one module (`mock_backend()`), no live calls.

### Part 3 — `dataset_composition_tool` (compute path) *(flagship tool)*

The highest-yield grounding: cell-state census + covariate balance, catching compositional confounding.

- **New `R/dataset_tools.R`** (home for all compute dataset tools) with `dataset_composition_tool(ctx)`:
  - Consumes **`metadata(ms)`** only, plus `ctx$params$group_col` (cell-state/cluster column) and `ctx$params$condition_col` (optional). Capability-gated on `'grouping'`; skip gracefully (message + `NULL`) when absent, mirroring `cluster_dme_tool`.
  - **Metrics** → tidy `result`: n cells per `group_col` level; proportion of each group within each `condition_col` level; the group × condition cross-tab; a skew statistic (Shannon entropy of the group distribution, and/or χ² standardized residuals flagging over/under-represented cells); **samples-per-condition** count when `sample_ids` capability is present (is the contrast even powered?).
  - **Caveats**: emit `cell_state_imbalanced_across_condition` when residuals exceed a threshold; `underpowered_contrast` when a condition has < N samples/cells.
  - `compact_summary` self-describes the unit via `ms$data_level` / `ms$aggregated` (e.g. "across 12,431 cells" vs "8 pseudobulk samples") — same pattern the prompt layer already uses.
  - Register in `.onLoad()` (`R/registry.R`) with `scope = 'dataset'`, `type = 'composition_summary'`, `requires = 'grouping'`.

Deliverable: `dataset_composition_tool` emits a valid `composition_summary` `dataset_fragment` from the example moduleset, gates on `grouping`, skips cleanly otherwise, sets caveats correctly on an imbalanced fixture; offline test `test-dataset_composition.R`.

### Part 4 — Composition **import** path (miloR / propeller / sccomp) *(companion to Part 3)*

The common real-world path: the user brings their own differential-abundance result.

- **New importers in `R/import_fragment.R`** (mirror `import_seurat_markers()` / `import_hdwgcna_dme()` / `import_enrichr()`, which normalize third-party columns into a fragment):
  - `import_dataset_fragment(type, result, fragment_id = NULL, tool_id = 'import_dataset_fragment', params = list(), source_file = NULL)` — the generic dataset analog of `import_fragment()`, producing a `dataset_fragment` with `provenance$source = 'user_supplied'`.
  - `import_milo_da(result, column_map = list(), ...)` — normalize a `miloR::DAtesting` neighborhood table (`logFC`, `SpatialFDR`, cell-type annotation) into a `composition_summary` `dataset_fragment`: summarize which cell states shift in abundance and their direction, set caveats. Default `column_map` handles miloR column names; overridable.
  - Add a `propeller`/`sccomp`-shaped default map (proportion-per-cluster-per-condition tables) as a second normalizer, or a `format = ` switch on `import_milo_da` — implementer's call; keep it parallel to how `import_seurat_markers` handles format-specific maps.
- These plug straight into `dataset_tool_config` as `list(fn = <importer-wrapping tool>, params = ...)`, or are constructed directly and passed to `build_dataset_context()`.

Deliverable: `import_milo_da()` (and one other DA format) produce valid `composition_summary` `dataset_fragment`s with `source = 'user_supplied'`; offline test `test-import_dataset_fragment.R` with a small synthetic miloR-shaped table.

### Part 5 — `dataset_variance_structure_tool` (recycle `PCRegression`) *(biological-vs-technical grounding)*

Quantifies how much of the dataset's variance aligns with each metadata covariate (condition vs batch vs depth) — the trust prior for every downstream `cross_condition_delta`.

- **`dataset_variance_structure_tool(ctx)`** in `R/dataset_tools.R`:
  - **Resolves the pseudo-bulk view** via `pseudobulk_view(ms)` (from [milestone_pseudobulk.md](milestone_pseudobulk.md)); skip gracefully when `NULL`. Runs on the pseudo-bulk expression (or `module_scores(pb_view)` — implementer picks the more informative substrate; expression is closer to `PCRegression`'s original use).
  - **Recycle `sample_code/pseudobulk_functions.R::PCRegression()`** — see [Recycled code](#recycled-code-pointers). Port its multi-covariate branch (the `lm(pc_scores ~ covariate)` loop producing `data.frame(component, covariate, R2, adj_R2, pval, fdr)` with BH correction) into a small internal helper `.pc_regression(embedding, meta_df, covariates)`. **Do not** carry over the `SummarizedExperiment` / `metadata(se)[[reduction_name]]` slot plumbing — llegir has no SE metadata slots; pass the PCA embedding matrix + covariate data.frame directly. Compute the embedding with `prcomp(t(X), scale = TRUE)` as `PseudobulkPCA()` does (`sample_code/pseudobulk_functions.R:436`), keeping the top `n_components`.
  - `ctx$params$covariates` names the metadata columns to regress each PC against. `result` is the tidy regression table; `top_findings` = the covariate/PC pairs with highest `adj_R2` at `fdr < 0.05`.
  - **Caveats**: `condition_confounded_with_batch` when a technical covariate (e.g. `batch`, `nCount`) explains a top PC with higher `adj_R2` than the biological condition does.
  - Register with `scope = 'dataset'`, `type = 'variance_structure'`, `requires = 'pseudobulk'` (or the appropriate capability).

Deliverable: `dataset_variance_structure_tool` emits a valid `variance_structure` `dataset_fragment` reusing the ported PC-regression logic, resolves the pseudo-bulk view, skips cleanly without it, sets the confounding caveat on a fixture where a technical covariate dominates; offline test `test-dataset_variance_structure.R`. Reuse/extend the synthetic pseudo-bulk fixture from the pseudo-bulk milestone.

### Part 6 — `dataset_baseline_expression_tool` *(secondary; optional)*

Distinguishes specific biology from dataset-wide housekeeping/ambient signal.

- **`dataset_baseline_expression_tool(ctx)`** in `R/dataset_tools.R`, consuming `expression(ms)` (+ `counts(ms)` when `capabilities()$counts`): dataset-wide mean expression, detection rate, and CV per gene; top-N globally dominant genes; % ribosomal (`^RP[LS]`) / mitochondrial (`^MT-`) mass; `nCount`/depth distribution summary.
- **Caveat**: `hub_genes_are_housekeeping` is not set here (it's per-module) — instead expose the global top-N / ubiquitous-gene list so the synthesis prompt lets the model discount a module whose hubs sit in it. `requires = 'expression'`, `scope = 'dataset'`, `type = 'baseline_expression'`.

Deliverable: valid `baseline_expression` `dataset_fragment` from the example moduleset; offline test. Ship only after Parts 1–5 land.

---

## Recycled code pointers

- **PC regression** — `sample_code/pseudobulk_functions.R:762` `PCRegression()`. The reusable core is the **multi-covariate branch** (`sample_code/pseudobulk_functions.R:900`–`936`): per covariate × PC, `lm(pc_scores ~ covariate)`, extract `summary()$r.squared` / `adj.r.squared` and `anova()$"Pr(>F)"[1]`, `rbind` into `data.frame(component, covariate, R2, adj_R2, pval)`, then `p.adjust(method = 'BH')`. Strip the `SummarizedExperiment` guard clauses and the `metadata(se)` slot writes; keep the regression loop. The single-covariate "level mode" branch (`836`–`898`) is not needed for the tool.
- **PCA embedding** — `sample_code/pseudobulk_functions.R:436` `PseudobulkPCA()`: `prcomp(t(X), rank. = n_components, scale = TRUE)`, then `pca_var <- sdev^2 / sum(sdev^2)`. Port the `prcomp` call + variance calc; drop the SE assay plumbing.
- **Usage reference** — `sample_code/TCGA_predictions_clean.Rmd:191` shows `PCRegression()` called with a covariate vector and the resulting `regression` table (`component, covariate, R2, adj_R2, pval, fdr`) — this table shape *is* the `variance_structure` fragment's `result`.
- **Import pattern** — `R/import_fragment.R` `import_seurat_markers()` / `import_hdwgcna_dme()` show the format-specific `column_map` normalization to copy for `import_milo_da()`.

## Definition of done (whole milestone)

- `dataset_fragment` / `dataset_context` contracts exist, validate, round-trip JSON, hash reproducibly, and are documented in `docs/schemas.md`; `inst/schemas/dataset_fragment.schema.json` is authoritative.
- `register_tool(scope = 'dataset')` + `list_tools('dataset')` work; core dataset tools register in `.onLoad()`.
- `run_dataset_context()` builds a context once per dataset; `build_user_prompt(dataset_context = )` injects a compact `DATASET CONTEXT` block above the per-module packet; `synthesize_module()` / `run_synthesis_orchestrator()` thread it; `PROMPT_TEMPLATE_VERSION` bumped.
- `dataset_composition_tool` (compute) **and** `import_milo_da()` (import) both emit valid `composition_summary` fragments; `dataset_variance_structure_tool` reuses the ported `PCRegression` logic.
- Fusion/faithfulness layers are **unchanged** — the `dataset_context` never enters the confidence matrix or citation checks.
- Every part has an offline, deterministic test; no live LLM or full-pipeline synthesis calls were made during development.
- `NEWS.md` notes the new dataset-tool surface and the `PROMPT_TEMPLATE_VERSION` bump.

---

*Last updated: 2026-07-22*
