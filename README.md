# Module Interpretation Engine

A tool that loads a gene co-expression module object (starting with hdWGCNA), systematically gathers a standardized bundle of evidence for each module, and produces a short, evidence-backed interpretation paragraph — with confidence-gated human review and a full reproducibility log. Deterministic evidence core in R; model-agnostic synthesis layer.

*Status: design → Milestone 1 (deterministic core). Started 2026-07.*
*Originated as an offshoot of the SERPENTINE project; now a standalone, general tool.*

---

## Start here

- **Building it (Claude Code):** read [`CLAUDE.md`](CLAUDE.md), then the current task in [`docs/milestone_1.md`](docs/milestone_1.md).
- **Concept:** [`docs/overview.md`](docs/overview.md)
- **Architecture:** [`docs/implementation_guide.md`](docs/implementation_guide.md)
- **Data contracts (schemas):** [`docs/schemas.md`](docs/schemas.md)
- **R code style:** [`STYLE.md`](STYLE.md)

---

## Development dataset

`data/CSF_Myeloid_hdWGCNA.rds` — an hdWGCNA Seurat object of **myeloid cells from cerebrospinal fluid (CSF)** across patients with different brain diseases. Used as the test object for the deterministic core.

Metadata columns of interest:

| Column | Role | Type |
|---|---|---|
| `diagnosis` | disease / condition | categorical |
| `Sample.ID` | individual sample | categorical (grouping) |
| `lv2_annot` | cell cluster / state | categorical (grouping for DME) |

Because this dataset has none of SERPENTINE's bespoke tools (CancerSEA, cross-lineage T-cell coordination), it is an ideal test that the **core is genuinely dataset-agnostic**.

---

*Last updated: 2026-07-10*
