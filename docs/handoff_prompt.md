# Claude Code handoff prompt

← [Project home](../README.md) · [Milestone 1](milestone_1.md)

The prompt used to kick off Milestone 1 with Claude Code, logged for reproducibility. Paste the block below into Claude Code from the repo root.

*Logged 2026-07-10.*

---

## Prompt

```
You're starting Milestone 1 of the Module Interpretation Engine — an R tool that
loads a gene co-expression module object (hdWGCNA) and gathers a standardized bundle
of evidence per module. This milestone is the deterministic evidence core only.

First, read these in the repo: CLAUDE.md, docs/milestone_1.md, docs/schemas.md,
docs/implementation_guide.md, and STYLE.md. milestone_1.md is the task.

Environment: activate the conda env `hdWGCNA` before running any R
(`conda activate hdWGCNA`). All required packages (hdWGCNA, Seurat, WGCNA, jsonlite,
digest, dplyr/tidyr, testthat) are already installed there. Do NOT install packages —
if something is missing, stop and tell me.

Scope for this milestone: the deterministic core ONLY. No LLM/synthesis, no custom
tools, no PubMed, and NO formal R package (plain sourced functions under R/, an
entry-point script under scripts/, testthat tests under tests/, JSON schemas under
schemas/). I'll handle packaging + pkgdown myself later.

Start with task 0 and STOP for my confirmation: load data/CSF_Myeloid_hdWGCNA.rds and
inspect it — confirm it's a Seurat object with an hdWGCNA experiment, that
GetModules(), GetMEs(), and GetHubGenes() work, and that `diagnosis`, `Sample.ID`,
and `lv2_annot` exist in the metadata (report their levels and the module count).
Report what you find and confirm the plan before writing any pipeline code.

After I confirm, work through milestone_1 tasks 1–5:
  1. hdWGCNA_ModuleSet adapter — core tools depend ONLY on this, never on
     hdWGCNA/Seurat directly.
  2. evidence_fragment constructor + validator + JSON (de)serialization (schemas.md);
     add the JSON Schema files under schemas/.
  3. core tools, each returning a validated evidence_fragment: hub_genes;
     cluster_dme (FindAllDMEs across lv2_annot); module_by_metadata (diagnosis,
     Sample.ID); geneset_enrichment (GO/Enrichr over hub genes — isolate if it needs
     network, note it).
  4. orchestrator that runs the configured tools per module and writes validated
     evidence packets (with packet_hash + provenance) to output/evidence_packets/.
  5. spike-in smoke test: a positive control (known gene set -> strong, correct
     evidence) and a negative control (random gene set -> weak/empty evidence).

Non-negotiables: only the adapter touches hdWGCNA/Seurat; every tool returns a
validated evidence_fragment; hash packets and log provenance; follow STYLE.md; keep
the core usable with no model and no network where possible.

Check in with me after task 0, and again after the adapter + evidence_fragment
contract are done, before you build all the tools.
```

---

## Notes

- Env: `hdWGCNA` conda env; packages pre-installed; do not install.
- Deliberate check-ins: after task 0, and after the adapter + contract.
- Milestone 2 (synthesis via `ellmer`, confidence/review, eval harness) is out of scope here.

---

*Last updated: 2026-07-10*
