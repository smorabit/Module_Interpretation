## dataset_tools.R: home for dataset-scope tools (docs/milestones/milestone_dataset_tools.md
## Part 3+). These run once per dataset via run_dataset_context(), never per
## module, and touch only the ModuleSet adapter contract (metadata(),
## has_capability(), pkg_versions()) -- never hdWGCNA/Seurat directly.

# prcomp(t(X), rank. = n_components, scale = TRUE) ported from
# sample_code/pseudobulk_functions.R:436 PseudobulkPCA(), dropping the SE
# assay plumbing; drops zero-variance genes first since scale = TRUE errors
# on a constant column (sample_code/TCGA_predictions_clean.Rmd:170-173)
.pseudobulk_pca <- function(X, n_components){
    gene_var <- apply(X, 1, stats::var)
    X <- X[gene_var > 0, , drop = FALSE]
    n_components <- min(n_components, ncol(X) - 1, nrow(X))
    pca <- stats::prcomp(t(X), rank. = n_components, scale. = TRUE)
    list(embedding = pca$x, variance = pca$sdev^2 / sum(pca$sdev^2))
}

# ported from sample_code/pseudobulk_functions.R:900-936 PCRegression()'s
# multi-covariate branch: per covariate x PC, lm(pc_scores ~ covariate),
# summary()$r.squared / adj.r.squared and anova()$"Pr(>F)"[1], BH-corrected
# across the whole table. No SummarizedExperiment metadata-slot writes --
# llegir has no SE metadata slots, so this returns the tidy table directly.
.pc_regression <- function(embedding, meta_df, covariates){
    num_pcs <- ncol(embedding)
    df <- data.frame()
    for (cov_name in covariates) {
        for (i in seq_len(num_pcs)) {
            pc_scores <- embedding[, i]
            formula_str <- paste('pc_scores ~', cov_name)
            tryCatch({
                lm_fit <- stats::lm(stats::as.formula(formula_str), data = cbind(pc_scores = pc_scores, meta_df))
                s <- summary(lm_fit)
                a <- stats::anova(lm_fit)
                cur_df <- data.frame(
                    component = i,
                    covariate = cov_name,
                    R2 = s$r.squared,
                    adj_R2 = s$adj.r.squared,
                    pval = a$`Pr(>F)`[1]
                )
                df <- rbind(df, cur_df)
            }, error = function(e){
                warning(paste('Error fitting model for PC', i, 'and covariate', cov_name, ':', e$message))
            })
        }
    }
    df$fdr <- stats::p.adjust(df$pval, method = 'BH')
    df
}

# shannon entropy (natural log) of a count vector; 0 when every unit falls
# into one group (maximally skewed), ln(k) when spread evenly across k groups
.shannon_entropy <- function(counts){
    p <- counts[counts > 0] / sum(counts)
    -sum(p * log(p))
}

#' Dataset tool: cell-state census and condition covariate balance
#'
#' A core dataset tool. Touches only the `ModuleSet` adapter contract
#' ([metadata()], [has_capability()], [pkg_versions()]). Summarizes how units
#' (cells or samples) distribute across `ctx$params$group_col` and, when
#' `ctx$params$condition_col` is given, whether that distribution is skewed
#' across condition levels -- the compositional-confounding check every
#' per-module synthesis should see as global framing before it interprets a
#' module as biology rather than cell-type imbalance.
#'
#' @param ctx A dataset tool context list: `list(ms, params, module_method)`,
#'   as built by [run_dataset_context()]. `ctx$params$group_col` (required) is
#'   the metadata column naming the cell-state/cluster grouping.
#'   `ctx$params$condition_col` (optional) is the metadata column to check the
#'   grouping for skew against; when omitted, only a group census is
#'   computed. `ctx$params$sample_col` (default `'sample'`) names the
#'   metadata sample-id column, consulted when the `sample_ids` capability
#'   holds. `ctx$params$residual_threshold` (default `2`) is the absolute
#'   chi-square standardized residual above which a group x condition cell is
#'   flagged. `ctx$params$min_samples` (default `3`) and
#'   `ctx$params$min_cells` (default `20`) are the per-condition minimums
#'   below which `'underpowered_contrast'` fires.
#' @return A `dataset_fragment` of type `'composition_summary'`, or `NULL` if
#'   `ctx$ms` lacks the `grouping` capability (see [capabilities()]) -- a
#'   graceful skip, not an error.
#' @examples
#' ms <- llegir_example_moduleset()
#' params <- list(group_col = 'cell_type', condition_col = 'diagnosis')
#' dataset_composition_tool(list(ms = ms, params = params))
#' @export
dataset_composition_tool <- function(ctx){
    group_col <- ctx$params$group_col
    if (is.null(group_col)) stop('dataset_composition requires params$group_col')
    condition_col <- ctx$params$condition_col
    sample_col <- ctx$params$sample_col %||% 'sample'
    residual_threshold <- ctx$params$residual_threshold %||% 2
    min_samples <- ctx$params$min_samples %||% 3
    min_cells <- ctx$params$min_cells %||% 20

    if (!has_capability(ctx$ms, 'grouping')) {
        message('dataset_composition: skipped, module set lacks the grouping capability')
        return(NULL)
    }

    meta <- metadata(ctx$ms)
    groups <- meta[[group_col]]
    if (is.null(groups)) stop('metadata column not found: ', group_col)
    groups <- droplevels(as.factor(groups))

    n_units <- length(groups)
    unit_label <- paste0(ctx$ms$data_level %||% 'cell', 's')
    group_counts <- table(groups)
    group_entropy <- .shannon_entropy(as.numeric(group_counts))
    caveats <- list()

    if (is.null(condition_col)) {
        result <- data.frame(
            group = names(group_counts),
            n = as.integer(group_counts),
            prop = as.numeric(group_counts) / n_units
        )
        result <- result[order(-result$n), ]
        rownames(result) <- NULL

        top_findings <- lapply(seq_len(min(5, nrow(result))), function(i){
            list(group = result$group[i], n = result$n[i], prop = round(result$prop[i], 3))
        })
        top_findings[[length(top_findings) + 1]] <- list(metric = 'shannon_entropy', value = round(group_entropy, 3))

        compact_summary <- paste0(
            'across ', format(n_units, big.mark = ','), ' ', unit_label, ': ',
            group_col, ' spans ', nlevels(groups), ' levels (entropy=', round(group_entropy, 2), ')'
        )
    } else {
        conditions <- meta[[condition_col]]
        if (is.null(conditions)) stop('metadata column not found: ', condition_col)
        conditions <- droplevels(as.factor(conditions))

        cross_tab <- table(groups, conditions)
        cond_totals <- colSums(cross_tab)
        # a chi-square test (and its standardized residuals) is undefined with
        # only one row or one column; fall back to NA residuals rather than error
        chisq_ok <- nrow(cross_tab) > 1 && ncol(cross_tab) > 1
        if (chisq_ok) {
            chisq <- suppressWarnings(stats::chisq.test(cross_tab))
            std_resid <- chisq$stdres
            expected <- chisq$expected
        } else {
            std_resid <- matrix(NA_real_, nrow(cross_tab), ncol(cross_tab), dimnames = dimnames(cross_tab))
            expected <- matrix(NA_real_, nrow(cross_tab), ncol(cross_tab), dimnames = dimnames(cross_tab))
        }

        result <- do.call(rbind, lapply(colnames(cross_tab), function(cnd){
            data.frame(
                group = rownames(cross_tab),
                condition = cnd,
                n = as.integer(cross_tab[, cnd]),
                prop_of_condition = as.numeric(cross_tab[, cnd]) / cond_totals[[cnd]],
                expected = as.numeric(expected[, cnd]),
                std_resid = as.numeric(std_resid[, cnd])
            )
        }))
        rownames(result) <- NULL
        ranked <- result[order(-abs(result$std_resid)), ]

        top_findings <- lapply(seq_len(min(5, nrow(ranked))), function(i){
            list(
                group = ranked$group[i], condition = ranked$condition[i], n = ranked$n[i],
                std_resid = round(ranked$std_resid[i], 2),
                direction = if (is.na(ranked$std_resid[i])) NA_character_ else if (ranked$std_resid[i] > 0) 'over' else 'under'
            )
        })
        top_findings[[length(top_findings) + 1]] <- list(metric = 'shannon_entropy', value = round(group_entropy, 3))

        if (chisq_ok && any(abs(result$std_resid) > residual_threshold, na.rm = TRUE)) {
            caveats[[length(caveats) + 1]] <- 'cell_state_imbalanced_across_condition'
        }

        underpowered <- any(cond_totals < min_cells)
        if (has_capability(ctx$ms, 'sample_ids') && !is.null(meta[[sample_col]])) {
            samples_per_condition <- tapply(meta[[sample_col]], conditions, function(x) length(unique(x)))
            for (cnd in names(samples_per_condition)) {
                top_findings[[length(top_findings) + 1]] <- list(
                    metric = 'samples_per_condition', condition = cnd,
                    n_samples = unname(samples_per_condition[[cnd]])
                )
            }
            underpowered <- underpowered || any(samples_per_condition < min_samples)
        }
        if (underpowered) caveats[[length(caveats) + 1]] <- 'underpowered_contrast'

        top_skew <- ranked[1, ]
        skew_desc <- if (chisq_ok) {
            paste0(
                '; strongest skew vs ', condition_col, ': ', top_skew$group, ' in ', top_skew$condition,
                ' (z=', round(top_skew$std_resid, 2), ')'
            )
        } else ''
        compact_summary <- paste0(
            'across ', format(n_units, big.mark = ','), ' ', unit_label, ': ',
            group_col, ' spans ', nlevels(groups), ' levels (entropy=', round(group_entropy, 2), ')', skew_desc
        )
    }

    dataset_fragment(
        fragment_id = 'composition',
        tool_id = 'dataset_composition',
        type = 'composition_summary',
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        caveats = caveats,
        provenance = make_provenance(
            tool_version = '0.1',
            params = list(group_col = group_col, condition_col = condition_col %||% NA_character_),
            pkg_versions = pkg_versions(ctx$ms)
        )
    )
}

#' Dataset tool: how much dataset variance aligns with each metadata covariate
#'
#' The trust prior for every downstream `cross_condition_delta`: runs PCA on
#' the pseudo-bulk expression matrix, then regresses each principal component
#' against each declared metadata covariate (`lm(pc_scores ~ covariate)`),
#' reporting `R2`/`adj_R2`/`fdr` per covariate x PC pair. Lets the synthesis
#' prompt tell whether a module's cross-condition signal rides on real
#' biology or on a technical axis (batch, sequencing depth) that happens to
#' dominate the same principal component. Ported from
#' `sample_code/pseudobulk_functions.R` `PseudobulkPCA()` / `PCRegression()`
#' (multi-covariate branch); see [.pseudobulk_pca()] / [.pc_regression()].
#'
#' @param ctx A dataset tool context list: `list(ms, params, module_method)`,
#'   as built by [run_dataset_context()]. `ctx$params$covariates` (required)
#'   is a character vector of pseudo-bulk metadata columns to regress each PC
#'   against. `ctx$params$condition_col` (required) names the biological
#'   covariate among `covariates`; every other entry is treated as
#'   `'technical'` for the confounding check unless
#'   `ctx$params$technical_covariates` overrides that. `ctx$params$n_components`
#'   (default `10`) is the number of principal components to compute,
#'   clamped to what the pseudo-bulk matrix can support.
#' @return A `dataset_fragment` of type `'variance_structure'`, or `NULL` if
#'   [pseudobulk_view()] can't resolve a pseudo-bulk view for `ctx$ms` -- a
#'   graceful skip, not an error.
#' @examples
#' \dontrun{
#' pb_ms <- pseudobulk_ModuleSet(pb_counts, list(module_a = c('GENE1', 'GENE2')), pb_meta)
#' dataset_variance_structure_tool(list(
#'     ms = pb_ms, params = list(covariates = c('condition', 'batch'), condition_col = 'condition')
#' ))
#' }
#' @export
dataset_variance_structure_tool <- function(ctx){
    covariates <- ctx$params$covariates
    if (is.null(covariates)) stop('dataset_variance_structure requires params$covariates')
    condition_col <- ctx$params$condition_col
    if (is.null(condition_col)) stop('dataset_variance_structure requires params$condition_col')
    technical_covariates <- ctx$params$technical_covariates %||% setdiff(covariates, condition_col)
    n_components <- ctx$params$n_components %||% 10

    pb_view <- pseudobulk_view(ctx$ms)
    if (is.null(pb_view)) {
        message('dataset_variance_structure: skipped, no pseudo-bulk view resolvable for this ModuleSet')
        return(NULL)
    }

    meta <- metadata(pb_view)
    missing_covs <- setdiff(covariates, colnames(meta))
    if (length(missing_covs) > 0) stop('metadata columns not found: ', paste(missing_covs, collapse = ', '))

    meta_df <- as.data.frame(meta[, covariates, drop = FALSE])
    for (col in covariates) {
        if (length(unique(meta_df[[col]])) <= 1) {
            stop("covariate '", col, "' has zero or one unique value and cannot be used for regression")
        }
        if (is.character(meta_df[[col]])) meta_df[[col]] <- factor(meta_df[[col]])
    }

    pca <- .pseudobulk_pca(expression(pb_view), n_components)
    regression <- .pc_regression(pca$embedding, meta_df, covariates)

    sig <- regression[regression$fdr < 0.05, , drop = FALSE]
    sig <- sig[order(-sig$adj_R2), ]
    top_findings <- lapply(seq_len(min(5, nrow(sig))), function(i){
        list(
            component = sig$component[i], covariate = sig$covariate[i],
            adj_R2 = round(sig$adj_R2[i], 3), fdr = signif(sig$fdr[i], 3)
        )
    })

    caveats <- list()
    if (nrow(sig) > 0) {
        # the caveat compares the technical and biological covariates on the
        # SAME pc -- the one the strongest signal (of any covariate) lands on --
        # since a technical axis dominating a different pc than condition
        # isn't a confound of that condition's signal
        top_component <- sig$component[1]
        comp_rows <- regression[regression$component == top_component, ]
        condition_adj_r2 <- comp_rows$adj_R2[comp_rows$covariate == condition_col]
        condition_adj_r2 <- if (length(condition_adj_r2) == 0) 0 else condition_adj_r2[1]
        technical_rows <- comp_rows[comp_rows$covariate %in% technical_covariates & comp_rows$fdr < 0.05, ]
        if (nrow(technical_rows) > 0 && max(technical_rows$adj_R2) > condition_adj_r2) {
            caveats[[length(caveats) + 1]] <- 'condition_confounded_with_batch'
        }
    }

    n_samples <- nrow(pca$embedding)
    compact_summary <- if (nrow(sig) > 0) {
        top <- sig[1, ]
        paste0(
            'variance structure across ', n_samples, ' pseudo-bulk samples: PC', top$component,
            ' (', round(pca$variance[top$component] * 100, 1), '% var) most aligned with ', top$covariate,
            ' (adj_R2=', round(top$adj_R2, 2), ', FDR=', signif(top$fdr, 2), ')'
        )
    } else {
        paste0(
            'variance structure across ', n_samples,
            ' pseudo-bulk samples: no covariate significantly explains a top PC (FDR<0.05)'
        )
    }

    dataset_fragment(
        fragment_id = 'variance_structure',
        tool_id = 'dataset_variance_structure',
        type = 'variance_structure',
        result = regression,
        compact_summary = compact_summary,
        top_findings = top_findings,
        caveats = caveats,
        provenance = make_provenance(
            tool_version = '0.1',
            params = list(covariates = covariates, condition_col = condition_col, n_components = ncol(pca$embedding)),
            pkg_versions = pkg_versions(ctx$ms)
        )
    )
}

#' Dataset tool: dataset-wide baseline expression and housekeeping signal
#'
#' Distinguishes specific per-module biology from dataset-wide ambient /
#' housekeeping signal: mean expression, detection rate, and CV per gene
#' (`expression(ms)`, full matrix, never returned), the top-N globally
#' dominant genes by mean expression, ribosomal (`^RP[LS]`) / mitochondrial
#' (`^MT-`) mass, and a depth (`nCount`) distribution summary. Touches only
#' the `ModuleSet` adapter contract ([expression()], [counts()],
#' [has_capability()], [pkg_versions()]). Does **not** set the per-module
#' `hub_genes_are_housekeeping` caveat -- that is a judgment about one
#' module's hub genes, made by synthesis after reading the exposed dominant-
#' gene list, not by this tool.
#'
#' @param ctx A dataset tool context list: `list(ms, params, module_method)`,
#'   as built by [run_dataset_context()]. `ctx$params$top_n` (default `15`)
#'   is the number of globally dominant genes (by mean expression) to keep in
#'   `result` and `top_findings` -- this list doubles as the ubiquitous-gene
#'   set synthesis can cross-check a module's hub genes against.
#' @return A `dataset_fragment` of type `'baseline_expression'`, or `NULL` if
#'   `ctx$ms` lacks the `expression` capability (see [capabilities()]) -- a
#'   graceful skip, not an error.
#' @examples
#' ms <- llegir_example_moduleset()
#' dataset_baseline_expression_tool(list(ms = ms, params = list()))
#' @export
dataset_baseline_expression_tool <- function(ctx){
    top_n <- ctx$params$top_n %||% 15

    if (!has_capability(ctx$ms, 'expression')) {
        message('dataset_baseline_expression: skipped, module set lacks the expression capability')
        return(NULL)
    }

    expr <- as.matrix(expression(ctx$ms))
    gene_names <- rownames(expr)

    mean_expr <- rowMeans(expr)
    detection_rate <- rowMeans(expr > 0)
    gene_sd <- apply(expr, 1, stats::sd)
    # CV undefined for a zero-mean gene; leave NA rather than dividing by zero
    cv <- ifelse(mean_expr > 0, gene_sd / mean_expr, NA_real_)

    ranked <- data.frame(
        gene_name = gene_names,
        mean_expr = mean_expr,
        detection_rate = detection_rate,
        cv = cv,
        stringsAsFactors = FALSE
    )
    ranked <- ranked[order(-ranked$mean_expr), ]
    result <- ranked[seq_len(min(top_n, nrow(ranked))), ]
    rownames(result) <- NULL

    top_findings <- lapply(seq_len(nrow(result)), function(i){
        list(
            gene_name = result$gene_name[i],
            mean_expr = round(result$mean_expr[i], 3),
            detection_rate = round(result$detection_rate[i], 3)
        )
    })

    total_mass <- sum(mean_expr)
    pct_ribo <- if (total_mass > 0) 100 * sum(mean_expr[grepl('^RP[LS]', gene_names)]) / total_mass else 0
    pct_mito <- if (total_mass > 0) 100 * sum(mean_expr[grepl('^MT-', gene_names)]) / total_mass else 0
    top_findings[[length(top_findings) + 1]] <- list(metric = 'pct_ribo_mass', value = round(pct_ribo, 2))
    top_findings[[length(top_findings) + 1]] <- list(metric = 'pct_mito_mass', value = round(pct_mito, 2))

    # counts() is a truer sequencing-depth proxy than expression() (which may
    # already be normalized/log-transformed); fall back to expression() when
    # the counts capability isn't declared
    depth_source <- if (has_capability(ctx$ms, 'counts')) counts(ctx$ms) else NULL
    depth_source <- depth_source %||% expr
    depth <- colSums(depth_source)
    depth_summary <- c(min = min(depth), median = stats::median(depth), mean = mean(depth), max = max(depth))
    top_findings[[length(top_findings) + 1]] <- list(
        metric = 'depth_distribution',
        min = round(depth_summary[['min']], 1), median = round(depth_summary[['median']], 1),
        mean = round(depth_summary[['mean']], 1), max = round(depth_summary[['max']], 1)
    )

    unit_label <- paste0(ctx$ms$data_level %||% 'cell', 's')
    compact_summary <- paste0(
        'baseline expression across ', format(ncol(expr), big.mark = ','), ' ', unit_label, ': ',
        'most dominant gene ', result$gene_name[1], ' (mean=', round(result$mean_expr[1], 2), '); ',
        'ribosomal mass=', round(pct_ribo, 1), '%, mitochondrial mass=', round(pct_mito, 1), '%; ',
        'depth median=', round(depth_summary[['median']], 1)
    )

    dataset_fragment(
        fragment_id = 'baseline_expression',
        tool_id = 'dataset_baseline_expression',
        type = 'baseline_expression',
        result = result,
        compact_summary = compact_summary,
        top_findings = top_findings,
        caveats = list(),
        provenance = make_provenance(
            tool_version = '0.1',
            params = list(top_n = top_n),
            pkg_versions = pkg_versions(ctx$ms)
        )
    )
}
