## shared statistics helpers for categorical / continuous module-score associations.
## generic (base R only); no adapter or backend dependency, so any ModuleSet works here.

# rank-biserial correlation from a two-sample Wilcoxon: bounded in [-1, 1] and
# well-defined for signed module scores, unlike a ratio-based log2FC.
# positive = x tends higher than y. R's W statistic counts pairs where x > y,
# so W == n1*n2 (x always bigger) must map to +1, not -1.
.rank_biserial <- function(x, y){
    n1 <- length(x)
    n2 <- length(y)
    w <- suppressWarnings(wilcox.test(x, y, exact = FALSE)$statistic)
    (2 * w) / (n1 * n2) - 1
}

# one-vs-rest test of `scores` against every level of `groups`, plus an omnibus
# Kruskal-Wallis test across all levels. Shared by cluster_dme (grouping = cell
# state) and the categorical branch of module_by_metadata (grouping = a
# metadata column like diagnosis). This reimplements the statistic behind
# hdWGCNA::FindAllDMEs directly on module_scores() + metadata() rather than
# calling FindAllDMEs itself, since that function needs the Seurat object.
# per-cell only for now; a pseudobulk-aggregated variant is a natural extension
# since it only needs an aggregated `scores`/`groups` pair.
categorical_group_test <- function(scores, groups){
    groups <- droplevels(as.factor(groups))
    levels_use <- levels(groups)

    kw <- suppressWarnings(kruskal.test(scores ~ groups))

    per_group <- do.call(rbind, lapply(levels_use, function(g){
        in_group <- groups == g
        x <- scores[in_group]
        y <- scores[!in_group]
        p_value <- suppressWarnings(wilcox.test(x, y, exact = FALSE)$p.value)
        data.frame(
            group = g,
            n = length(x),
            mean_score = mean(x),
            median_score = median(x),
            rank_biserial = .rank_biserial(x, y),
            p_value = p_value
        )
    }))
    per_group$fdr <- p.adjust(per_group$p_value, method = 'BH')
    per_group$direction <- ifelse(per_group$rank_biserial > 0, 'up', 'down')
    # strongest association first, so callers can just take row 1 for top_findings
    per_group <- per_group[order(-abs(per_group$rank_biserial)), ]
    rownames(per_group) <- NULL

    list(table = per_group, omnibus_p = unname(kw$p.value))
}

# Pearson + Spearman correlation of `scores` against a continuous variable;
# used by the continuous branch of module_by_metadata (not exercised by the
# CSF dataset, but part of the generic contract for future datasets)
continuous_correlation_test <- function(scores, x){
    keep <- !is.na(x) & !is.na(scores)
    scores <- scores[keep]
    x <- x[keep]
    pear <- suppressWarnings(cor.test(scores, x, method = 'pearson'))
    spear <- suppressWarnings(cor.test(scores, x, method = 'spearman'))
    data.frame(
        n = length(x),
        pearson_r = unname(pear$estimate),
        pearson_p = pear$p.value,
        spearman_rho = unname(spear$estimate),
        spearman_p = spear$p.value
    )
}
