# Module Interpretation Engine — Overview

← [Project home](../README.md) · [Implementation guide](implementation_guide.md) · [Schemas](schemas.md) · [Milestone 1](milestone_1.md)

*Status: Concept / design, started 2026-07.*
*Origin: an offshoot of the SERPENTINE project (`module_interpretation_dossier.Rmd` is the manual prototype the core tools port from). Now a standalone, general tool.*

---

## The problem

Interpreting a single gene co-expression module currently requires manually synthesizing many disjoint pieces of evidence: the ranked hub-gene list, which cell cluster/state expresses the module, differential expression across conditions and metadata, pathway/GO enrichment, overlap with external signatures, and any dataset-specific context. With dozens to hundreds of modules per study, this is slow, inconsistent between analysts, and hard to reproduce or standardize for a manuscript.

## The idea

A tool that **loads an hdWGCNA object, systematically gathers a standardized bundle of evidence for each module, and produces a short, evidence-backed interpretation paragraph** — with a human-in-the-loop review step and a full reproducibility log.

Pipeline, at a glance:

```
hdWGCNA object ──▶ [evidence tools] ──▶ evidence packet (per module)
                                              │
                                              ▼
                              [synthesis: structured slots] ──▶ interpretation object
                                              │
                                     confidence + flags
                                              │
                        ┌─────────────────────┴─────────────────────┐
                        ▼                                             ▼
              boilerplate paragraph                        human review queue
```

## Design principles

1. **Deterministic evidence core, bounded model layer.** All quantitative analysis is done by versioned R functions (reproducible, auditable). The model only *interprets* a fixed evidence packet and *drafts* prose — it does not free-write and run arbitrary analysis code in the production path. This reconciles "agentic" with "standardized + reproducible."
2. **Everything flows through two contracts.** A common *evidence fragment* shape (what every tool returns) and a structured *interpretation object* (the slots the model fills). These make tools pluggable and the model swappable. See [schemas](schemas.md).
3. **Toolbox, core + custom.** Dataset-agnostic core tools (hub genes, cluster DME, metadata associations, enrichment) plus **bespoke custom tools** registered per project.
4. **Structured slots → rendered paragraph.** The model emits typed fields; the boilerplate paragraph is rendered deterministically from them. No free-form paragraph generation.
5. **Every claim cites its evidence.** Each statement references the fragment(s) that support it — the anti-hallucination guarantee and the basis for automated faithfulness checks.
6. **Confidence-gated human review.** Modules are flagged for deeper review when confidence is low, when model confidence disagrees with deterministic evidence strength, when tools contradict, or when an artifact pattern (e.g. immediate-early / dissociation) is detected.
7. **Reproducibility anchored on evidence, not prose.** LLM text is not bit-reproducible across model versions; the *evidence packet* is. The provenance manifest logs the evidence hash, model + version, prompt, seeds, package versions, and the full code log.
8. **Model-agnostic and open-source.** A light R orchestrator (candidate: `ellmer`) so the user picks the model. Built for community extension via registered tools and data-source adapters.
9. **Generalizable by design.** hdWGCNA first, but the core tools talk to a thin `ModuleSet` adapter, so other module/factor sources (NMF/cNMF, Hotspot, metaprograms, DE gene lists) can be swapped in later without rewriting the tools.

## Scope & goals

- **Immediate:** run the deterministic core on the CSF myeloid hdWGCNA object (see [project home](../README.md)), producing standardized evidence packets per module.
- **Then:** add the synthesis layer, confidence/review, and evaluation.
- **Longer term:** a general, open-source tool; generalize beyond hdWGCNA to arbitrary modules/programs/factors and data structures (deferred until the hdWGCNA path works).

## Relation to prior work (SERPENTINE project)

- *Tumor Module Interpretation* — the multi-evidence framework this automates.
- `module_interpretation_dossier.Rmd` — manual per-module evidence dossier; the prototype for the core tools.
- The SERPENTINE bespoke analyses (CancerSEA scoring, cross-lineage T-cell coordination) are the motivating examples of *custom tools*; they are not needed for the CSF development dataset.

## Open questions

- R package vs. lighter scripts + config (leaning package for the open-source goal — see [CLAUDE.md](../CLAUDE.md)).
- Whether synthesis gets a bounded live PubMed call or literature is pre-retrieved deterministically.
- Tool name.

---

*Last updated: 2026-07-10*
