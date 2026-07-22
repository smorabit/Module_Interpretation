## run_dataset_context() + DATASET CONTEXT prompt injection + synthesis
## threading (docs/milestones/milestone_dataset_tools.md Part 2). Offline
## only: llegir_example_moduleset() + mock_backend(), no live syntheses, no
## real dataset tool yet (Part 3) -- fragments here are hand-built or come
## from a trivial identity-style stub.

make_stub_dataset_fragment <- function(fragment_id = 'composition'){
    dataset_fragment(
        fragment_id = fragment_id,
        tool_id = 'stub_dataset_tool',
        type = 'composition_summary',
        result = data.frame(group = c('a', 'b'), n = c(5, 3)),
        compact_summary = 'across 8 cells: group a 62%, group b 38%',
        top_findings = list(list(group = 'a', n = 5)),
        caveats = list('cell_state_imbalanced_across_condition'),
        provenance = make_provenance(tool_version = '0.1', pkg_versions = list(dummy = '1.0'))
    )
}

test_that('run_dataset_context() builds a valid dataset_context from a direct fn spec', {
    ms <- llegir_example_moduleset()
    stub_tool <- function(ctx) make_stub_dataset_fragment()
    dc <- run_dataset_context(ms, list(list(fn = stub_tool, params = list())), input_hash = 'abc')
    expect_equal(length(dc$dataset_fragments), 1)
    expect_equal(dc$provenance$input_hash, 'abc')
    expect_equal(dc$provenance$skipped, list())
})

test_that('run_dataset_context() skips a registry tool missing a required capability and records why', {
    ms <- llegir_example_moduleset()
    stub_tool <- function(ctx) make_stub_dataset_fragment()
    register_tool(
        'stub_dataset_tool_missing_cap', stub_tool, type = 'composition_summary',
        description = 'x', requires = 'pseudobulk', scope = 'dataset'
    )
    dc <- run_dataset_context(ms, list(list(id = 'stub_dataset_tool_missing_cap', params = list())))
    expect_equal(length(dc$dataset_fragments), 0)
    expect_equal(length(dc$provenance$skipped), 1)
    expect_equal(dc$provenance$skipped[[1]]$tool_id, 'stub_dataset_tool_missing_cap')
    expect_match(dc$provenance$skipped[[1]]$reason, 'pseudobulk')
})

test_that('run_dataset_context() runs a registry tool whose required capability is met', {
    ms <- llegir_example_moduleset()
    stub_tool <- function(ctx) make_stub_dataset_fragment()
    register_tool(
        'stub_dataset_tool_ok', stub_tool, type = 'composition_summary',
        description = 'x', requires = 'grouping', scope = 'dataset'
    )
    dc <- run_dataset_context(ms, list(list(id = 'stub_dataset_tool_ok', params = list())))
    expect_equal(length(dc$dataset_fragments), 1)
    expect_equal(dc$provenance$skipped, list())
})

test_that('render_dataset_context_compact() renders compact_summary, top_findings, and caveats but never the result table', {
    dc <- build_dataset_context(list(make_stub_dataset_fragment()), input_hash = 'abc')
    txt <- render_dataset_context_compact(dc)
    expect_true(grepl('DATASET CONTEXT', txt))
    expect_true(grepl('composition', txt, fixed = TRUE))
    expect_true(grepl('across 8 cells', txt, fixed = TRUE))
    expect_true(grepl('cell_state_imbalanced_across_condition', txt, fixed = TRUE))
    expect_false(grepl('n_cells', txt, fixed = TRUE))
})

test_that('build_user_prompt() injects the DATASET CONTEXT block after the dataset description and before the evidence packet', {
    ms <- llegir_example_moduleset()
    packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
    desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
    dc <- build_dataset_context(list(make_stub_dataset_fragment()), input_hash = 'abc')

    txt <- build_user_prompt(packet, desc, dataset_context = dc)
    lines <- strsplit(txt, '\n')[[1]]
    idx_desc <- which(grepl('Dataset context', lines))[1]
    idx_dc <- which(grepl('DATASET CONTEXT', lines))[1]
    idx_packet <- which(grepl('evidence packet', lines))[1]
    expect_true(idx_desc < idx_dc)
    expect_true(idx_dc < idx_packet)
})

test_that('build_user_prompt() with dataset_context = NULL omits the block and matches prior output exactly', {
    ms <- llegir_example_moduleset()
    packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
    desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')

    txt_default <- build_user_prompt(packet, desc)
    txt_explicit_null <- build_user_prompt(packet, desc, dataset_context = NULL)
    expect_equal(txt_default, txt_explicit_null)
    expect_false(grepl('DATASET CONTEXT', txt_default))
})

test_that('build_system_prompt() instructs the model to treat DATASET CONTEXT as global framing, not a citable fragment', {
    txt <- build_system_prompt()
    expect_true(grepl('DATASET CONTEXT', txt))
    expect_true(grepl('not a per-module fragment', txt))
})

test_that('synthesize_module() threads dataset_context into the prompt on the mock backend', {
    ms <- llegir_example_moduleset()
    packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
    desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')
    dc <- build_dataset_context(list(make_stub_dataset_fragment()), input_hash = 'abc')

    interp <- synthesize_module(packet, desc, mock_backend(), dataset_context = dc)
    expect_true(validate_interpretation(interp))
})

test_that('synthesize_module() without dataset_context still validates (backward compatible)', {
    ms <- llegir_example_moduleset()
    packet <- run_module(ms, modules(ms)[1], list(list(fn = top_genes_tool, params = list())))
    desc <- dataset_description('human', 'CSF', 'myeloid', 'scRNA-seq')

    interp <- synthesize_module(packet, desc, mock_backend())
    expect_true(validate_interpretation(interp))
})
