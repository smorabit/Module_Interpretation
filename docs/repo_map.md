.
├── _pkgdown.yml
├── CLAUDE_old.md
├── CLAUDE.md
├── data
│   ├── CSF_Myeloid_hdWGCNA.rds
│   ├── GO_Biological_Process_2026.txt
│   └── h.all.v2026.1.Hs.symbols.gmt
├── DESCRIPTION
├── docs
│   ├── 2026-07-10.md
│   ├── 2026-07-13.md
│   ├── 2026-07-21.md
│   ├── custom_tools.md
│   ├── dev_economy.md
│   ├── implementation_guide.md
│   ├── milestones
│   │   ├── milestone_1_5.md
│   │   ├── milestone_1.md
│   │   ├── milestone_2.md
│   │   ├── milestone_abstract_moduleset.md
│   │   ├── milestone_dataset_tools.md
│   │   ├── milestone_extensibility.md
│   │   ├── milestone_fused_confidence.md
│   │   ├── milestone_packaging.md
│   │   ├── milestone_pseudobulk.md
│   │   └── milestone2_verification.md
│   ├── overview.md
│   ├── prompts
│   │   ├── handoff_prompt_dataset_tools.md
│   │   ├── handoff_prompt_dev_economy.md
│   │   ├── handoff_prompt_extensibility_1.md
│   │   ├── handoff_prompt_extensibility_2.md
│   │   ├── handoff_prompt_fused_confidence.md
│   │   ├── handoff_prompt_hdwgcna_equivalence.md
│   │   ├── handoff_prompt_m1_5.md
│   │   ├── handoff_prompt_m2_run.md
│   │   ├── handoff_prompt_m2.md
│   │   ├── handoff_prompt_packaging.md
│   │   └── handoff_prompt.md
│   ├── repo_map.md
│   └── schemas.md
├── inst
│   ├── schemas
│   │   ├── dataset_fragment.schema.json
│   │   ├── evidence_fragment.schema.json
│   │   └── interpretation.schema.json
│   └── templates
│       └── summary_report.Rmd
├── LICENSE
├── LICENSE.md
├── man
│   ├── assert_faithfulness.Rd
│   ├── build_dataset_context.Rd
│   ├── build_evidence_packet.Rd
│   ├── build_review_queue.Rd
│   ├── build_synthesis_manifest.Rd
│   ├── build_system_prompt.Rd
│   ├── build_user_prompt.Rd
│   ├── cached_backend.Rd
│   ├── calculate_fusion_score.Rd
│   ├── capabilities.Rd
│   ├── categorical_group_test.Rd
│   ├── check_faithfulness.Rd
│   ├── cluster_dme_tool.Rd
│   ├── components_ModuleSet.Rd
│   ├── compute_evidence_signals.Rd
│   ├── counts.Rd
│   ├── dataset_context_to_json.Rd
│   ├── dataset_description.Rd
│   ├── dataset_fragment_from_json.Rd
│   ├── dataset_fragment_to_json.Rd
│   ├── dataset_fragment.Rd
│   ├── describe_flags.Rd
│   ├── differential_module_activity_tool.Rd
│   ├── ellmer_backend.Rd
│   ├── enforce_faithfulness.Rd
│   ├── evidence_fragment.Rd
│   ├── expression.Rd
│   ├── fragment_from_json.Rd
│   ├── fragment_to_json.Rd
│   ├── fuse_confidence.Rd
│   ├── gene_list_ModuleSet.Rd
│   ├── gene_membership.Rd
│   ├── geneset_enrichment_tool.Rd
│   ├── get_tool.Rd
│   ├── has_capability.Rd
│   ├── hdWGCNA_ModuleSet.Rd
│   ├── import_enrichr.Rd
│   ├── import_fragment_tool.Rd
│   ├── import_fragment.Rd
│   ├── import_hdwgcna_dme.Rd
│   ├── import_seurat_markers.Rd
│   ├── interpretation_from_json.Rd
│   ├── interpretation_hash.Rd
│   ├── interpretation_to_json.Rd
│   ├── interpretation.Rd
│   ├── is_faithful.Rd
│   ├── list_tools.Rd
│   ├── llegir_example_moduleset.Rd
│   ├── llegir-package.Rd
│   ├── make_interpretation_provenance.Rd
│   ├── make_provenance.Rd
│   ├── metadata.Rd
│   ├── mock_backend.Rd
│   ├── model_output_schema_json.Rd
│   ├── module_scores.Rd
│   ├── modules.Rd
│   ├── needs_review.Rd
│   ├── packet_to_json.Rd
│   ├── pkg_versions.Rd
│   ├── PROMPT_TEMPLATE_VERSION.Rd
│   ├── pseudobulk_de_limma_tool.Rd
│   ├── pseudobulk_ModuleSet.Rd
│   ├── pseudobulk_view.Rd
│   ├── pseudobulk.Rd
│   ├── read_dataset_context.Rd
│   ├── read_evidence_packet.Rd
│   ├── read_interpretation.Rd
│   ├── register_tool.Rd
│   ├── render_dataset_context_compact.Rd
│   ├── render_dataset_description.Rd
│   ├── render_packet_compact.Rd
│   ├── render_paragraph.Rd
│   ├── RENDER_TEMPLATE_VERSION.Rd
│   ├── resolve_backend.Rd
│   ├── run_dataset_context.Rd
│   ├── run_module.Rd
│   ├── run_orchestrator.Rd
│   ├── run_synthesis_orchestrator.Rd
│   ├── signature_correlation_tool.Rd
│   ├── synthesize_interpretation.Rd
│   ├── synthesize_module.Rd
│   ├── synthetic_ModuleSet.Rd
│   ├── top_genes_tool.Rd
│   ├── validate_dataset_description.Rd
│   ├── validate_dataset_fragment.Rd
│   ├── validate_evidence_fragment.Rd
│   ├── validate_interpretation.Rd
│   ├── validate_moduleset.Rd
│   ├── with_pseudobulk.Rd
│   ├── write_dataset_context.Rd
│   ├── write_evidence_packet.Rd
│   ├── write_fragment_tables.Rd
│   ├── write_interpretation_report.Rd
│   ├── write_interpretation.Rd
│   ├── write_review_queue.Rd
│   └── write_synthesis_manifest.Rd
├── NAMESPACE
├── NEWS.md
├── output
│   ├── cache
│   │   ├── 0e687097cd09776cbdb31aa22c29b5862663bb894429165265eebba9f7f73cb0.rds
│   │   ├── 1015b77398b6919441426aafc0d20601a6b5663f48f01d967bcbf1e76c50a4ed.rds
│   │   ├── 1f60bdd93cc605bde43004eae98d49cef353b63b4f1633617f9f79c5a7577b39.rds
│   │   ├── 200d397836b6d8ccaf08ef2834034ac29829806a2870587791c84ca10b98e291.rds
│   │   ├── 208a7413314299e996ea39b57700b9ce1413b53a5d25813b63ae15e5433f31e1.rds
│   │   ├── 2c0dfb8d5c26d59ee35c4665f9003e5a164dc27fbc942371e8f2b03324f48aa4.rds
│   │   ├── 43277a3278fa42dd0ad34bcc846ad47240a67dbf15916b6922322073c3f16d02.rds
│   │   ├── 45cc4e9c146477252d8b2c153481f834ee5b83d8cbe88cef6fde0e6fdea2e6a4.rds
│   │   ├── 4b49ae991414e8d1192eebfc82b1a40c7f20290b7f70329dc1d41a477f99b3bf.rds
│   │   ├── 5e22b2ca1345d6add78fcb8232379c2e6413148865e60ffb31e8321f259294ed.rds
│   │   ├── 669f4d0799c69e511e0e994da1a79e99c9c43d553b9c407c9b3a631d72ba841e.rds
│   │   ├── 7db8711381f73d00d7bff9f590dc9c8ac9e8f6e9a502fcc23368d157fec65474.rds
│   │   ├── 854c4d8757696594dcdec2fa69f72c520f45de0066fc018d172b09ac09f725ab.rds
│   │   ├── 85c364816e6af239b4bf8a7376fdcff0227c3d1abf1209e479155e57d6fc867d.rds
│   │   ├── 86ba3d2a869ba82de6b68947f29e173953952977d3bebd19c1394b298a8a44d3.rds
│   │   ├── 91cce207e7129f7707db4f8157ddd04051a6ca58d802956eeb6319a9d99fc6d3.rds
│   │   ├── 9be6081354a3b91fad47b6b79da6463d5e0ce57715b237824a42ded0abc5184b.rds
│   │   ├── a94a65590db71f1676a6c3b161c4a67834731c9b71d4323b0af807d079a12947.rds
│   │   ├── ae6a4b336bc477cef3c148650605e2a7820d722c846cea173286a461bc82cbdd.rds
│   │   ├── bec484bb4d8b45b23719924a181d73e9c6f72e11b0a725f871ec7e4c59fa5519.rds
│   │   ├── c753f141e2ac0c739a87429baa3976f8c44343e085caabf742a8453ab75cdee4.rds
│   │   ├── c7abf9cbacf2bfb4d621437b048d1e4068e59fd62430819fce1dbe6d323f082a.rds
│   │   ├── ca7237bd8c3d1916c75c6f0ad9940eee6097095172e5280aed0bc4200aed9ce3.rds
│   │   ├── da022940ced7cad00d90d72b2161f3e29f03eb4bd8ca54e2b8149a84ae4b7397.rds
│   │   ├── da4809520ca03d862b8cba2137bb8d117439fe595075a1e56f1f383c9d1c0985.rds
│   │   ├── e1dc871c4796673611038145ae1350aa4ae59ad98a66c8d86b5954708c7f5500.rds
│   │   ├── e42868506259fe8d214767f0e630a8a4cb7aeeaf39408b18e8ee1d535dd3fa32.rds
│   │   ├── e6883e4538b16bf9ff8f2b35d277ed3f2e0f6b2e93bf95e84db7eb7699a8cbf1.rds
│   │   ├── f001d2669b078055995ad0e570443b2ef1b498b3f84f9e384c60aff42dbc99e7.rds
│   │   ├── f9abe0bdacd91306298929d2ab2ec265ca4fd00952af405b55b6b438ac93e441.rds
│   │   └── fc066d152a2d129ba0bb8fb0b962b965aa0768c9643f115732c8ef5bbacb795b.rds
│   ├── dev_report.html
│   ├── evidence_packets
│   │   ├── MM1.json
│   │   ├── MM10.json
│   │   ├── MM11.json
│   │   ├── MM12.json
│   │   ├── MM13.json
│   │   ├── MM14.json
│   │   ├── MM2.json
│   │   ├── MM3.json
│   │   ├── MM4.json
│   │   ├── MM5.json
│   │   ├── MM6.json
│   │   ├── MM7.json
│   │   ├── MM8.json
│   │   └── MM9.json
│   ├── evidence_packets_deg
│   │   ├── MM1.json
│   │   ├── MM2.json
│   │   └── MM3.json
│   ├── interpretations
│   │   ├── manifest.json
│   │   ├── MM1.json
│   │   ├── MM1.md
│   │   ├── MM10.json
│   │   ├── MM10.md
│   │   ├── MM11.json
│   │   ├── MM11.md
│   │   ├── MM12.json
│   │   ├── MM12.md
│   │   ├── MM13.json
│   │   ├── MM13.md
│   │   ├── MM14.json
│   │   ├── MM14.md
│   │   ├── MM2.json
│   │   ├── MM2.md
│   │   ├── MM3.json
│   │   ├── MM3.md
│   │   ├── MM4.json
│   │   ├── MM4.md
│   │   ├── MM5.json
│   │   ├── MM5.md
│   │   ├── MM6.json
│   │   ├── MM6.md
│   │   ├── MM7.json
│   │   ├── MM7.md
│   │   ├── MM8.json
│   │   ├── MM8.md
│   │   ├── MM9.json
│   │   ├── MM9.md
│   │   └── review_queue.tsv
│   ├── interpretations_deg
│   │   ├── manifest.json
│   │   ├── MM1.json
│   │   ├── MM1.md
│   │   ├── MM2.json
│   │   ├── MM2.md
│   │   ├── MM3.json
│   │   ├── MM3.md
│   │   └── review_queue.tsv
│   └── tables
│       ├── MM1
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM10
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM11
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM12
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM13
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM14
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM2
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM3
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM4
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM5
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM6
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM7
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       ├── MM8
│       │   ├── cluster_dme.tsv
│       │   ├── geneset_enrichment.tsv
│       │   ├── hub_genes.tsv
│       │   ├── metadata__diagnosis.tsv
│       │   ├── metadata__sample.tsv
│       │   └── signature_correlation.tsv
│       └── MM9
│           ├── cluster_dme.tsv
│           ├── geneset_enrichment.tsv
│           ├── hub_genes.tsv
│           ├── metadata__diagnosis.tsv
│           ├── metadata__sample.tsv
│           └── signature_correlation.tsv
├── pkgdown_site
│   ├── 2026-07-10.html
│   ├── 2026-07-13.html
│   ├── 404.html
│   ├── articles
│   │   ├── getting-started.html
│   │   └── index.html
│   ├── authors.html
│   ├── CLAUDE_old.html
│   ├── CLAUDE.html
│   ├── deps
│   │   ├── bootstrap-5.3.8
│   │   │   ├── bootstrap.bundle.min.js
│   │   │   ├── bootstrap.bundle.min.js.map
│   │   │   └── bootstrap.min.css
│   │   ├── bootstrap-toc-1.0.1
│   │   │   └── bootstrap-toc.min.js
│   │   ├── clipboard.js-2.0.11
│   │   │   └── clipboard.min.js
│   │   ├── data-deps.txt
│   │   ├── font-awesome-6.5.2
│   │   │   ├── css
│   │   │   │   ├── all.css
│   │   │   │   ├── all.min.css
│   │   │   │   ├── v4-shims.css
│   │   │   │   └── v4-shims.min.css
│   │   │   └── webfonts
│   │   │       ├── fa-brands-400.ttf
│   │   │       ├── fa-brands-400.woff2
│   │   │       ├── fa-regular-400.ttf
│   │   │       ├── fa-regular-400.woff2
│   │   │       ├── fa-solid-900.ttf
│   │   │       ├── fa-solid-900.woff2
│   │   │       ├── fa-v4compatibility.ttf
│   │   │       └── fa-v4compatibility.woff2
│   │   ├── headroom-0.11.0
│   │   │   ├── headroom.min.js
│   │   │   └── jQuery.headroom.min.js
│   │   ├── jquery-3.6.0
│   │   │   ├── jquery-3.6.0.js
│   │   │   ├── jquery-3.6.0.min.js
│   │   │   └── jquery-3.6.0.min.map
│   │   └── search-1.0.0
│   │       ├── autocomplete.jquery.min.js
│   │       ├── fuse.min.js
│   │       └── mark.min.js
│   ├── index.html
│   ├── katex-auto.js
│   ├── LICENSE-text.html
│   ├── LICENSE.html
│   ├── lightswitch.js
│   ├── link.svg
│   ├── news
│   │   └── index.html
│   ├── pkgdown.js
│   ├── pkgdown.yml
│   ├── reference
│   │   ├── aggregate_by_sample.html
│   │   ├── assert_faithfulness.html
│   │   ├── build_evidence_packet.html
│   │   ├── build_review_queue.html
│   │   ├── build_synthesis_manifest.html
│   │   ├── build_system_prompt.html
│   │   ├── build_user_prompt.html
│   │   ├── cached_backend.html
│   │   ├── capabilities.components_ModuleSet.html
│   │   ├── capabilities.hdWGCNA_ModuleSet.html
│   │   ├── capabilities.html
│   │   ├── capabilities.synthetic_ModuleSet.html
│   │   ├── categorical_group_test.html
│   │   ├── check_faithfulness.html
│   │   ├── cluster_dme_tool.html
│   │   ├── components_ModuleSet.html
│   │   ├── compute_evidence_signals.html
│   │   ├── continuous_correlation_test.html
│   │   ├── dataset_description.html
│   │   ├── describe_flags.html
│   │   ├── ellmer_backend.html
│   │   ├── enforce_faithfulness.html
│   │   ├── evidence_fragment.html
│   │   ├── expression.components_ModuleSet.html
│   │   ├── expression.hdWGCNA_ModuleSet.html
│   │   ├── expression.html
│   │   ├── expression.synthetic_ModuleSet.html
│   │   ├── fragment_from_json.html
│   │   ├── fragment_to_json.html
│   │   ├── fuse_confidence.html
│   │   ├── gene_list_ModuleSet.html
│   │   ├── gene_membership.components_ModuleSet.html
│   │   ├── gene_membership.hdWGCNA_ModuleSet.html
│   │   ├── gene_membership.html
│   │   ├── gene_membership.synthetic_ModuleSet.html
│   │   ├── geneset_enrichment_tool.html
│   │   ├── get_tool.html
│   │   ├── has_capability.html
│   │   ├── hdWGCNA_ModuleSet.html
│   │   ├── hub_genes_tool.html
│   │   ├── import_fragment_tool.html
│   │   ├── import_fragment.html
│   │   ├── index.html
│   │   ├── interpretation_from_json.html
│   │   ├── interpretation_hash.html
│   │   ├── interpretation_to_json.html
│   │   ├── interpretation.html
│   │   ├── is_faithful.html
│   │   ├── is_sample_constant.html
│   │   ├── list_tools.html
│   │   ├── llegir_example_moduleset.html
│   │   ├── llegir-package.html
│   │   ├── llegir.html
│   │   ├── make_interpretation_provenance.html
│   │   ├── make_provenance.html
│   │   ├── metadata.components_ModuleSet.html
│   │   ├── metadata.hdWGCNA_ModuleSet.html
│   │   ├── metadata.html
│   │   ├── metadata.synthetic_ModuleSet.html
│   │   ├── mock_backend.html
│   │   ├── model_output_schema_json.html
│   │   ├── module_by_metadata_tool.html
│   │   ├── module_scores.components_ModuleSet.html
│   │   ├── module_scores.hdWGCNA_ModuleSet.html
│   │   ├── module_scores.html
│   │   ├── module_scores.synthetic_ModuleSet.html
│   │   ├── modules.components_ModuleSet.html
│   │   ├── modules.hdWGCNA_ModuleSet.html
│   │   ├── modules.html
│   │   ├── modules.synthetic_ModuleSet.html
│   │   ├── needs_review.html
│   │   ├── packet_to_json.html
│   │   ├── pkg_versions.components_ModuleSet.html
│   │   ├── pkg_versions.gene_list_ModuleSet.html
│   │   ├── pkg_versions.hdWGCNA_ModuleSet.html
│   │   ├── pkg_versions.html
│   │   ├── pkg_versions.synthetic_ModuleSet.html
│   │   ├── PROMPT_TEMPLATE_VERSION.html
│   │   ├── read_evidence_packet.html
│   │   ├── read_interpretation.html
│   │   ├── register_tool.html
│   │   ├── render_dataset_description.html
│   │   ├── render_packet_compact.html
│   │   ├── render_paragraph.html
│   │   ├── RENDER_TEMPLATE_VERSION.html
│   │   ├── resolve_backend.html
│   │   ├── run_module.html
│   │   ├── run_orchestrator.html
│   │   ├── run_synthesis_orchestrator.html
│   │   ├── sentit_example_moduleset.html
│   │   ├── sentit-package.html
│   │   ├── sentit.html
│   │   ├── signature_correlation_tool.html
│   │   ├── synthesize_interpretation.html
│   │   ├── synthesize_module.html
│   │   ├── synthetic_ModuleSet.html
│   │   ├── validate_dataset_description.html
│   │   ├── validate_evidence_fragment.html
│   │   ├── validate_interpretation.html
│   │   ├── write_evidence_packet.html
│   │   ├── write_fragment_tables.html
│   │   ├── write_interpretation.html
│   │   ├── write_review_queue.html
│   │   └── write_synthesis_manifest.html
│   ├── search.json
│   ├── sitemap.xml
│   ├── STYLE.html
│   └── tutorials
├── R
│   ├── confidence.R
│   ├── dataset_description.R
│   ├── dataset_fragment.R
│   ├── example_moduleset.R
│   ├── exporters.R
│   ├── faithfulness.R
│   ├── fragment.R
│   ├── import_fragment.R
│   ├── interpretation.R
│   ├── llegir-package.R
│   ├── moduleset_components.R
│   ├── moduleset_gene_list.R
│   ├── moduleset_hdwgcna.R
│   ├── moduleset_pseudobulk.R
│   ├── moduleset.R
│   ├── orchestrator.R
│   ├── prompt.R
│   ├── registry.R
│   ├── render.R
│   ├── stats_utils.R
│   ├── synthesis.R
│   ├── tool_cluster_dme.R
│   ├── tool_differential_module_activity.R
│   ├── tool_geneset_enrichment.R
│   ├── tool_pseudobulk_de_limma.R
│   ├── tool_signature_correlation.R
│   ├── tool_top_genes.R
│   └── utils.R
├── README.md
├── README.Rmd
├── sample_code
│   ├── pseudobulk_functions.R
│   ├── T-cell_tumor_network_compare.Rmd
│   └── TCGA_predictions_clean.Rmd
├── scripts
│   ├── interactive_test.R
│   ├── interactive_test.Rmd
│   ├── nmf_factor_test.R
│   ├── pseudobulk_functions.R
│   ├── run_csf.R
│   └── run_synthesis_csf.R
├── STYLE.md
├── tests
│   └── testthat
│       ├── _snaps
│       ├── setup.R
│       ├── synthetic_extensibility.R
│       ├── synthetic_moduleset.R
│       ├── synthetic_pseudobulk.R
│       ├── test-confidence.R
│       ├── test-dataset_fragment.R
│       ├── test-dataset_orchestrator.R
│       ├── test-faithfulness.R
│       ├── test-fragment.R
│       ├── test-import_fragment.R
│       ├── test-interpretation.R
│       ├── test-moduleset_adapter.R
│       ├── test-moduleset_components.R
│       ├── test-moduleset_gene_list.R
│       ├── test-moduleset_pseudobulk.R
│       ├── test-prompt.R
│       ├── test-registry.R
│       ├── test-render.R
│       ├── test-spike_in.R
│       ├── test-synthesis.R
│       ├── test-tool_differential_module_activity.R
│       ├── test-tool_pseudobulk_de_limma.R
│       ├── test-tool_signature_correlation.R
│       ├── test-tools.R
│       └── test-validate_moduleset.R
└── vignettes
    └── getting-started.Rmd

52 directories, 528 files
