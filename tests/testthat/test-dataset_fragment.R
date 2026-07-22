## dataset_fragment / dataset_context contract: construction, validation,
## JSON round-trip, and hash determinism. Sibling of test-fragment.R. No
## dependency on any ModuleSet here.

make_valid_dataset_fragment <- function(){
    dataset_fragment(
        fragment_id = 'composition',
        tool_id = 'dataset_composition_tool',
        type = 'composition_summary',
        result = data.frame(group = c('myeloid', 'lymphoid'), n_cells = c(120, 80)),
        compact_summary = 'across 200 cells: myeloid 60%, lymphoid 40%',
        top_findings = list(list(group = 'myeloid', n_cells = 120)),
        caveats = list('cell_state_imbalanced_across_condition'),
        provenance = make_provenance(tool_version = '0.1', pkg_versions = list(dummy = '1.0'))
    )
}

test_that('dataset_fragment() rejects an invalid type', {
    expect_error(
        dataset_fragment(
            fragment_id = 'x', tool_id = 'x', type = 'not_a_type',
            result = data.frame(), compact_summary = '', top_findings = list(),
            provenance = list()
        )
    )
})

test_that('validate_dataset_fragment() passes a well-formed fragment', {
    expect_true(validate_dataset_fragment(make_valid_dataset_fragment()))
})

test_that('validate_dataset_fragment() catches missing required fields', {
    frag <- make_valid_dataset_fragment()
    frag$compact_summary <- NULL
    expect_error(validate_dataset_fragment(frag), 'missing required fields')
})

test_that('validate_dataset_fragment() catches malformed provenance', {
    frag <- make_valid_dataset_fragment()
    frag$provenance$pkg_versions <- NULL
    expect_error(validate_dataset_fragment(frag), 'provenance missing fields')
})

test_that('validate_dataset_fragment() rejects a caveat outside the controlled vocab', {
    frag <- make_valid_dataset_fragment()
    frag$caveats <- list('not_a_real_caveat')
    expect_error(validate_dataset_fragment(frag), 'invalid caveats')
})

test_that('dataset_fragment JSON round-trip preserves fields and result table', {
    frag <- make_valid_dataset_fragment()
    restored <- dataset_fragment_from_json(dataset_fragment_to_json(frag))
    expect_equal(restored$fragment_id, frag$fragment_id)
    expect_equal(restored$type, frag$type)
    expect_equal(unlist(restored$caveats), unlist(frag$caveats))
    expect_equal(restored$result, frag$result)
    expect_true(validate_dataset_fragment(restored))
})

test_that('build_dataset_context() hashes identically regardless of timestamp', {
    frag_a <- make_valid_dataset_fragment()
    frag_b <- make_valid_dataset_fragment()
    frag_b$provenance$timestamp <- '2099-01-01T00:00:00+0000'
    context_a <- build_dataset_context(list(frag_a), input_hash = 'abc')
    context_b <- build_dataset_context(list(frag_b), input_hash = 'abc')
    # identical dataset evidence must hash identically even though the timestamps differ
    expect_equal(context_a$context_hash, context_b$context_hash)
})

test_that('build_dataset_context() rejects an invalid fragment', {
    bad <- make_valid_dataset_fragment()
    bad$result <- 'not a data.frame'
    expect_error(build_dataset_context(list(bad)))
})

test_that('dataset context JSON round-trip preserves the hash and fragment count', {
    context <- build_dataset_context(list(make_valid_dataset_fragment()), input_hash = 'abc')
    tmp <- tempfile(fileext = '.json')
    on.exit(unlink(tmp))
    write_dataset_context(context, tmp)
    restored <- read_dataset_context(tmp)
    expect_equal(restored$context_hash, context$context_hash)
    expect_equal(length(restored$dataset_fragments), length(context$dataset_fragments))
    expect_true(validate_dataset_fragment(restored$dataset_fragments[[1]]))
})

test_that('register_tool() accepts scope = "dataset" and checks type against the dataset vocab', {
    dummy_dataset_tool <- function(ctx) make_valid_dataset_fragment()
    register_tool(
        'dummy_dataset_tool', dummy_dataset_tool, type = 'composition_summary',
        description = 'x', scope = 'dataset'
    )
    spec <- get_tool('dummy_dataset_tool')
    expect_equal(spec$scope, 'dataset')
    expect_error(
        register_tool('bad_scope_tool', dummy_dataset_tool, type = 'composition_summary', description = 'x', scope = 'module'),
        'invalid type'
    )
    expect_error(
        register_tool('bad_scope_tool', dummy_dataset_tool, type = 'ranked_genes', description = 'x', scope = 'nonsense'),
        'scope must be'
    )
})

test_that('list_tools() filters by scope without disturbing the unfiltered list', {
    dummy_dataset_tool <- function(ctx) make_valid_dataset_fragment()
    register_tool(
        'dummy_dataset_tool_2', dummy_dataset_tool, type = 'composition_summary',
        description = 'x', scope = 'dataset'
    )
    expect_true('dummy_dataset_tool_2' %in% list_tools())
    expect_true('dummy_dataset_tool_2' %in% list_tools('dataset'))
    expect_false('dummy_dataset_tool_2' %in% list_tools('module'))
    expect_true('top_genes' %in% list_tools('module'))
    expect_false('top_genes' %in% list_tools('dataset'))
    expect_error(list_tools('nonsense'), 'scope must be')
})
