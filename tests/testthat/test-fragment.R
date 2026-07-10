## evidence_fragment / evidence_packet contract: construction, validation,
## JSON round-trip, and hash determinism. No dependency on any ModuleSet here.

make_valid_fragment <- function(){
    evidence_fragment(
        fragment_id = 'dummy',
        tool_id = 'dummy_tool',
        module_id = 'MM1',
        type = 'ranked_genes',
        result = data.frame(gene = c('A', 'B'), score = c(0.9, 0.5)),
        compact_summary = 'top genes: A, B',
        top_findings = list(list(gene = 'A', score = 0.9)),
        effect_strength = 0.9,
        significance = 0.01,
        direction = 'up',
        provenance = make_provenance(tool_version = '0.1', pkg_versions = list(dummy = '1.0'))
    )
}

test_that('evidence_fragment() rejects an invalid type or direction', {
    expect_error(
        evidence_fragment(
            fragment_id = 'x', tool_id = 'x', module_id = 'MM1', type = 'not_a_type',
            result = data.frame(), compact_summary = '', top_findings = list(),
            effect_strength = 0, provenance = list()
        )
    )
    expect_error(
        evidence_fragment(
            fragment_id = 'x', tool_id = 'x', module_id = 'MM1', type = 'ranked_genes',
            result = data.frame(), compact_summary = '', top_findings = list(),
            effect_strength = 0, direction = 'sideways', provenance = list()
        )
    )
})

test_that('validate_evidence_fragment() passes a well-formed fragment', {
    expect_true(validate_evidence_fragment(make_valid_fragment()))
})

test_that('validate_evidence_fragment() catches missing required fields', {
    frag <- make_valid_fragment()
    frag$compact_summary <- NULL
    expect_error(validate_evidence_fragment(frag), 'missing required fields')
})

test_that('validate_evidence_fragment() catches malformed provenance', {
    frag <- make_valid_fragment()
    frag$provenance$pkg_versions <- NULL
    expect_error(validate_evidence_fragment(frag), 'provenance missing fields')
})

test_that('fragment JSON round-trip preserves fields and result table', {
    frag <- make_valid_fragment()
    restored <- fragment_from_json(fragment_to_json(frag))
    expect_equal(restored$fragment_id, frag$fragment_id)
    expect_equal(restored$type, frag$type)
    expect_equal(restored$direction, frag$direction)
    expect_equal(restored$result, frag$result)
    expect_true(validate_evidence_fragment(restored))
})

test_that('build_evidence_packet() hashes identically regardless of timestamp', {
    frag_a <- make_valid_fragment()
    frag_b <- make_valid_fragment()
    frag_b$provenance$timestamp <- '2099-01-01T00:00:00+0000'
    packet_a <- build_evidence_packet('MM1', list(frag_a), input_hash = 'abc')
    packet_b <- build_evidence_packet('MM1', list(frag_b), input_hash = 'abc')
    # identical evidence must hash identically even though the timestamps differ
    expect_equal(packet_a$packet_hash, packet_b$packet_hash)
})

test_that('build_evidence_packet() rejects an invalid fragment', {
    bad <- make_valid_fragment()
    bad$effect_strength <- 'not a number'
    expect_error(build_evidence_packet('MM1', list(bad)))
})

test_that('packet JSON round-trip preserves the hash and fragment count', {
    packet <- build_evidence_packet('MM1', list(make_valid_fragment()), input_hash = 'abc')
    tmp <- tempfile(fileext = '.json')
    on.exit(unlink(tmp))
    write_evidence_packet(packet, tmp)
    restored <- read_evidence_packet(tmp)
    expect_equal(restored$packet_hash, packet$packet_hash)
    expect_equal(length(restored$fragments), length(packet$fragments))
    expect_true(validate_evidence_fragment(restored$fragments[[1]]))
})
