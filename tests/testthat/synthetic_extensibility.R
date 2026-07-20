## Synthetic ModuleSet fixtures for the extensibility milestone (Part 1): a
## components_ModuleSet and two gene_list_ModuleSets (UCell, decoupleR), all
## built from real GO Biological Process gene sets
## (data/GO_Biological_Process_2026.txt) rather than made-up gene names, per
## docs/milestone_extensibility.md task 6. Guarded by the same
## file-existence pattern as the CSF fixtures (setup.R), since the GO file is
## also a large gitignored dev-only download.

go_data_available <- file.exists('../../data/GO_Biological_Process_2026.txt')

if (go_data_available) {
    .go_pathways <- suppressWarnings(fgsea::gmtPathways('../../data/GO_Biological_Process_2026.txt'))

    # two real, modest-size GO BP terms as the two synthetic "modules" --
    # sizes (6-7 genes) safely clear decoupleR::run_ulm()'s default minsize = 5
    go_module_a <- "'De Novo' AMP Biosynthetic Process (GO:0044208)"
    go_module_b <- '5S Class rRNA Transcription by RNA Polymerase III (GO:0042791)'
    go_gene_sets <- list(module_a = .go_pathways[[go_module_a]], module_b = .go_pathways[[go_module_b]])

    # four unrelated real GO BP terms, used only as decoy pathways for
    # geneset_enrichment_tool and as background (non-module) genes
    go_decoys <- c(
        'Amyloid-beta Formation (GO:0034205)',
        'CRD-mediated mRNA Stabilization (GO:0070934)',
        'Double-strand Break Repair via Alternative Nonhomologous End Joining (GO:0097681)',
        'Negative Regulation of Alcohol Biosynthetic Process (GO:1902931)'
    )

    # a small curated GMT (the two module terms + four decoys) instead of the
    # full ~6000-pathway file: keeps geneset_enrichment_tool tests fast, and
    # avoids GeneOverlap's "union larger than genome size" error, which the
    # tiny synthetic genome below would trip against some real pathway's much
    # larger gene set elsewhere in the full file
    go_test_gmt <- tempfile(fileext = '.gmt')
    writeLines(
        vapply(c(go_module_a, go_module_b, go_decoys), function(term){
            paste(c(term, '', .go_pathways[[term]]), collapse = '\t')
        }, character(1)),
        go_test_gmt
    )
    go_test_db_files <- c(GO_BP = go_test_gmt)

    # background genome: the decoy pathways' own genes (real symbols, never
    # assigned to a module) plus a few pure noise genes, so genome_size is
    # realistically larger than any curated pathway without being huge
    go_background_genes <- setdiff(
        unlist(.go_pathways[go_decoys], use.names = FALSE),
        unlist(go_gene_sets, use.names = FALSE)
    )
    go_noise_genes <- paste0('NOISE', 1:15)

    # same simulation shape as .example_base_moduleset() (R/example_moduleset.R):
    # a per-cell latent factor per module drives its own genes plus
    # gene-specific noise, so genes within a module carry real correlation
    # signal -- just built from real GO gene symbols instead of made-up ones
    .go_synthetic_data <- function(seed = 1, n_cells = 120){
        set.seed(seed)
        diagnosis <- sample(c('case', 'control'), n_cells, replace = TRUE)
        cell_type <- sample(c('type_a', 'type_b'), n_cells, replace = TRUE)
        sample_id <- sample(paste0('sample', 1:6), n_cells, replace = TRUE)

        latent_a <- stats::rnorm(n_cells, mean = ifelse(diagnosis == 'case', 1, 0), sd = 0.5)
        latent_b <- stats::rnorm(n_cells, mean = ifelse(cell_type == 'type_a', 1, 0), sd = 0.5)

        gene_expr <- function(genes, latent){
            m <- t(vapply(seq_along(genes), function(i) latent + stats::rnorm(n_cells, sd = 0.3), numeric(n_cells)))
            rownames(m) <- genes
            m
        }
        background_genes <- c(go_background_genes, go_noise_genes)

        expr <- rbind(
            gene_expr(go_gene_sets$module_a, latent_a),
            gene_expr(go_gene_sets$module_b, latent_b),
            matrix(
                stats::rnorm(length(background_genes) * n_cells),
                nrow = length(background_genes), dimnames = list(background_genes, NULL)
            )
        )
        expr[expr < 0] <- 0
        colnames(expr) <- paste0('cell', seq_len(n_cells))

        meta <- data.frame(diagnosis = diagnosis, cell_type = cell_type, sample = sample_id, row.names = colnames(expr))
        list(expr = expr, meta = meta)
    }

    go_fixture <- .go_synthetic_data()

    # module score = mean z-scored expression across the gene set (same
    # stand-in as synthetic_ModuleSet()); kme = each gene's correlation with
    # that score, so the components fixture carries real per-gene weights,
    # not placeholders
    .go_module_score <- function(expr, genes){
        genes <- intersect(genes, rownames(expr))
        sub <- as.matrix(expr[genes, , drop = FALSE])
        rowMeans(scale(t(sub)))
    }

    go_components_gene_table <- do.call(rbind, lapply(names(go_gene_sets), function(m){
        genes <- intersect(go_gene_sets[[m]], rownames(go_fixture$expr))
        score <- .go_module_score(go_fixture$expr, genes)
        kme <- vapply(genes, function(g) stats::cor(as.numeric(go_fixture$expr[g, ]), score), numeric(1))
        data.frame(module = m, gene_name = genes, weight = unname(kme))
    }))
    go_components_scores <- as.data.frame(lapply(go_gene_sets, function(genes) .go_module_score(go_fixture$expr, genes)))
    rownames(go_components_scores) <- colnames(go_fixture$expr)

    # the generic components_ModuleSet fixture (docs/milestone_extensibility.md task 2/6)
    # counts is a rounded stand-in for raw counts, same dimensions as expression, so this
    # fixture exercises every declared capability (docs/milestone_abstract_moduleset.md Part 2)
    go_components_ms <- components_ModuleSet(
        go_components_gene_table, go_fixture$expr, go_fixture$meta, scores = go_components_scores,
        counts = round(go_fixture$expr), group_col = 'cell_type', sample_col = 'sample'
    )

    # gene-list ModuleSet fixtures (task 4/6): no weights, scores computed on
    # the fly. Two scoring backends, plus a variant with no group_col/
    # sample_col declared, to exercise the capability-based graceful-skip
    # path (task 5) end to end.
    go_gene_list_ms_ucell <- gene_list_ModuleSet(
        go_gene_sets, go_fixture$expr, go_fixture$meta,
        group_col = 'cell_type', sample_col = 'sample', method = 'UCell'
    )
    go_gene_list_ms_decoupler <- gene_list_ModuleSet(
        go_gene_sets, go_fixture$expr, go_fixture$meta,
        group_col = 'cell_type', sample_col = 'sample', method = 'decoupleR'
    )
    go_gene_list_ms_nocap <- gene_list_ModuleSet(go_gene_sets, go_fixture$expr, go_fixture$meta, method = 'UCell')

    # tiny synthetic signature library for signature_correlation_tool tests
    # (docs/milestone_extensibility.md Part 2a): module_a's own gene set as
    # one signature -- expect near-perfect co-variation with module_a's own
    # score, a spike-in-style sanity check -- plus module_b's gene set and a
    # decoy pathway as true negatives for module_a
    go_signature_library_path <- tempfile(fileext = '.rds')
    saveRDS(
        list(
            sig_match_a = go_gene_sets$module_a,
            sig_match_b = go_gene_sets$module_b,
            sig_decoy = .go_pathways[[go_decoys[1]]]
        ),
        go_signature_library_path
    )
    go_test_signature_files <- c(custom = go_signature_library_path)

    # shared tool_config for full-pipeline tests, mirroring csf_tool_config in setup.R
    go_tool_config <- function(db_files = go_test_db_files){
        list(
            list(fn = hub_genes_tool, params = list(n_hubs = 6)),
            list(fn = cluster_dme_tool, params = list(group_by = 'cell_type')),
            list(fn = module_by_metadata_tool, params = list(column = 'diagnosis', column_type = 'categorical')),
            list(fn = geneset_enrichment_tool, params = list(n_hubs = 6, db_files = db_files))
        )
    }
}
