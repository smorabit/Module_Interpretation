# CLAUDE.md

Guidance for Claude Code working in this repo.

## What this is

The **Module Interpretation Engine**: loads a gene co-expression module object (hdWGCNA first), gathers a standardized bundle of evidence per module via a toolbox of R functions, and (later) drafts an evidence-backed interpretation paragraph with confidence-gated human review. Deterministic evidence core in R; model-agnostic synthesis layer added later.

Read [`README.md`](README.md) and the design docs in [`docs/`](docs/) before starting:
- [`docs/overview.md`](docs/overview.md) — concept + design principles
- [`docs/implementation_guide.md`](docs/implementation_guide.md) — architecture
- [`docs/schemas.md`](docs/schemas.md) — the two data contracts (build against these)
- **[`docs/milestone_1.md`](docs/milestone_1.md) — the current task. Start here.**

## Current status

Milestone 1: deterministic evidence core, **no LLM**. Build the `ModuleSet` adapter + core tools + evidence-packet serialization + spike-in smoke test, tested on `data/CSF_Myeloid_hdWGCNA.rds`. Do not build the synthesis/LLM layer yet.

## Non-negotiables

- **Core tools depend only on the `ModuleSet` adapter, never on hdWGCNA/Seurat directly.** This is what keeps the engine generalizable. (Only the adapter imports Seurat/hdWGCNA.)
- **Everything a tool returns is an `evidence_fragment`; validate it.** See `docs/schemas.md`.
- **Reproducibility is anchored on the evidence packet:** hash it, log provenance (params, input hashes, package versions).
- Follow [`STYLE.md`](STYLE.md) for all R code (tidyverse, snake_case, 4-space blocks, single quotes, no roxygen).

## Layout (functionality first; formal package later)

**Nail the functionality with plain sourced R functions — do NOT scaffold a formal
R package yet** (no `DESCRIPTION`/`NAMESPACE`/roxygen exports). Package structure and
pkgdown are handled later by the maintainer.

```
R/                  # moduleset_*.R, tool_*.R, fragment.R, orchestrator.R (plain sourced functions)
schemas/            # evidence_fragment.schema.json, interpretation.schema.json
tests/              # testthat test files, run via testthat::test_dir('tests')
scripts/            # run_csf.R — source R/, run the orchestrator end-to-end on the dev object
data/               # CSF_Myeloid_hdWGCNA.rds (dev object; gitignored)
docs/               # design notes
output/             # evidence_packets/ (gitignored)
```

## Environment

**All R runs in the conda env `hdWGCNA`** — `conda activate hdWGCNA` before anything. Everything required is already installed there; **do not install packages**. If something appears missing, stop and flag it rather than installing.

## Dependencies

R (≥ 4.2), provided by the `hdWGCNA` env. Packages: `hdWGCNA`, `Seurat`, `WGCNA` (adapter only); `jsonlite`, `digest`, `dplyr`/`tidyr`, `testthat`. Milestone 2 adds `ellmer` for model-agnostic synthesis (needs an API key, e.g. `ANTHROPIC_API_KEY`) — not required for M1.

## Running

- Load the dev object, run the orchestrator over all modules, write packets to `output/evidence_packets/`.
- `testthat::test_dir('tests/testthat')` must pass, including the spike-in controls.

## Ground rules

- Verify the dev object's structure before assuming (task 0 in milestone 1).
- Prefer offline/deterministic operation; if a tool needs the network (e.g. Enrichr), isolate it and note it.
- Keep the deterministic core useful with no model.
