# Milestone 1 — Deterministic evidence core (no LLM)

← [Overview](overview.md) · [Implementation guide](implementation_guide.md) · [Schemas](schemas.md) · [Project home](../README.md)

*Status: complete. All tasks (0–5) and acceptance criteria below are met; see [run_csf.R](../scripts/run_csf.R) and [tests/testthat/](../tests/testthat/).*

---

## Goal

Build the **deterministic core only**: load the CSF hdWGCNA object through a `ModuleSet` adapter, run a handful of core tools per module, and emit validated **evidence packets** (JSON + tables). **No LLM, no synthesis, no custom tools, no PubMed** in this milestone — those are Milestone 2+. This is fully testable locally with no API key and no model variability.

## Development dataset

`data/CSF_Myeloid_hdWGCNA.rds` — hdWGCNA Seurat object, myeloid cells from CSF across brain diseases.

| Column | Role |
|---|---|
| `diagnosis` | condition (categorical) → `module_by_metadata` |
| `Sample.ID` | sample (categorical) → `module_by_metadata` |
| `lv2_annot` | cluster / state → `cluster_dme` grouping |

## Tasks

**0. Inspect the object first.** Confirm it is a Seurat object with an hdWGCNA experiment; confirm `GetModules()`, `GetMEs()`, `GetHubGenes()` work; confirm `diagnosis`, `Sample.ID`, `lv2_annot` exist in `@meta.data` and their levels. Record the module count and a couple of example modules. Do **not** assume — verify and note anything surprising.

**1. `ModuleSet` adapter.** Implement `hdWGCNA_ModuleSet` against the interface in the [implementation guide](implementation_guide.md#5-moduleset-adapter-design-now-generalize-later): `modules()`, `gene_membership(module)` (genes + kME), `module_scores()` (MEs), `expression()`, `metadata()`. **Core tools must depend only on this adapter, never on hdWGCNA/Seurat directly.**

**2. Contracts.** Implement the `evidence_fragment` constructor + validator and the JSON (de)serialization from [schemas.md](schemas.md). Add the two JSON Schema files.

**3. Core tools** (each returns an `evidence_fragment`; port logic from SERPENTINE's `module_interpretation_dossier.Rmd`):
- `hub_genes` — top genes by kME.
- `cluster_dme` — `FindAllDMEs` across `lv2_annot`; which clusters express the module.
- `module_by_metadata` — ME vs. a declared column: categorical (`diagnosis`, `Sample.ID`) → group means + Kruskal/enrichment; (continuous handled generically for future datasets).
- `geneset_enrichment` — GO/Enrichr over hub genes (use whatever enrichment is available offline; may stub if network-bound and note it).

**4. Orchestrator + packet.** A driver that, per module, runs the configured tools and assembles a validated evidence packet with a `packet_hash` and provenance manifest. Serialize all packets to `output/evidence_packets/`.

**5. Spike-in smoke test** (the eval seed):
- **Positive control:** create a synthetic module from a known gene set (e.g. a cell-cycle or interferon set present in the data) and confirm the evidence packet clearly reflects it (enrichment + expected cluster).
- **Negative control:** a random gene set → the packet should show weak/empty evidence (low `effect_strength`, no significant enrichment).

## Acceptance criteria (done =)

- [x] Runs end-to-end on `CSF_Myeloid_hdWGCNA.rds` and writes a validated evidence packet for **every** module (14/14).
- [x] Every fragment passes `validate_evidence_fragment`; every packet has a hash + provenance manifest.
- [x] Core tools touch only the `ModuleSet` adapter (grep for `GetModules`/`Seurat` outside the adapter → none).
- [x] Spike-in positive control shows strong, correct evidence (pDC marker genes → `pDC` cluster, r > 0.5, FDR < 0.01); negative control shows weak evidence (random gene set scores well below the positive control on both `cluster_dme` and `geneset_enrichment`).
- [x] `testthat` tests cover the adapter, each tool's fragment shape, and the two spike-ins (59/59 passing via `testthat::test_dir('tests/testthat')`).
- [x] Follows [STYLE.md](../STYLE.md).

## Explicitly out of scope for M1

LLM / synthesis layer, interpretation objects, paragraph rendering, confidence/review routing, custom-tool registry, PubMed/literature, additional `ModuleSet` adapters.

## Conventions

- R style: [STYLE.md](../STYLE.md).
- **Functionality first — do NOT scaffold a formal R package yet** (no `DESCRIPTION`/`NAMESPACE`). Plain sourced functions under `R/`, a `scripts/run_csf.R` entry point, `testthat` tests under `tests/`, JSON schemas under `schemas/`. Package + pkgdown come later, handled by the maintainer.
- Keep the deterministic core usable with no model and no network where possible.

---

*Last updated: 2026-07-10*
