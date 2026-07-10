## hdWGCNA_ModuleSet: confirms every adapter generic returns the shape core
## tools expect, using the CSF dev object loaded once in setup.R.

test_that('modules() lists module ids and excludes grey', {
    mods <- modules(ms_test)
    expect_true(is.character(mods))
    expect_true(length(mods) > 0)
    expect_false('grey' %in% mods)
})

test_that('gene_membership() returns genes ranked by kME', {
    gm <- gene_membership(ms_test, mod_test)
    expect_true(is.data.frame(gm))
    expect_true(all(c('gene_name', 'module', 'kme') %in% colnames(gm)))
    expect_true(nrow(gm) > 0)
    # ranked strongest first
    expect_equal(gm$kme, sort(gm$kme, decreasing = TRUE))
})

test_that('module_scores() returns one column per module, or a vector for one', {
    all_scores <- module_scores(ms_test)
    expect_true(is.data.frame(all_scores))
    expect_true(mod_test %in% colnames(all_scores))
    expect_equal(nrow(all_scores), ncol(expression(ms_test)))

    one_score <- module_scores(ms_test, module = mod_test)
    expect_true(is.numeric(one_score))
    expect_equal(length(one_score), nrow(all_scores))
})

test_that('expression() and metadata() align on cells', {
    expr <- expression(ms_test)
    meta <- metadata(ms_test)
    expect_equal(ncol(expr), nrow(meta))
})

test_that('metadata() carries the columns core tools depend on', {
    meta <- metadata(ms_test)
    expect_true(all(c('diagnosis', 'sample', 'lv2_annot') %in% colnames(meta)))
})

test_that('pkg_versions() reports the backend packages', {
    versions <- pkg_versions(ms_test)
    expect_true(all(c('hdWGCNA', 'Seurat', 'WGCNA') %in% names(versions)))
})
