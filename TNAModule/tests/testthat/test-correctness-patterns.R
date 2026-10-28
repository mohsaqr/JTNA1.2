pattern_result <- function(data, analysis = "TNA", ...) {
  roles <- list(buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor")
  if (analysis == "GroupTNA") roles$buildModel_variables_long_group <- "group"
  result <- do.call(jtna_run, c(list(analysis, data = data, pattern_show_table = TRUE,
    pattern_len_min = 2, pattern_len_max = 2, pattern_min_count = 1,
    pattern_min_support = 0, pattern_table_show_all = TRUE), roles, list(...)))
  expect_length(jtna_errors(result), 0L)
  result
}

test_that("pattern association uses group denominators and presence, not occurrences", {
  data <- data.frame(actor = rep(seq_len(100), each = 4),
    action = rep(c("A", "B", "C", "D", "A", "C", "B", "D"), 50),
    group = rep(c(rep("large", 90), rep("small", 10)), each = 4))
  patterns <- jtna_table(pattern_result(data, "GroupTNA")$patterns$table)
  row <- patterns[patterns$pattern == "A->B", ]
  expect_equal(c(row$count_large, row$count_small), c(45L, 5L))
  expect_equal(row$chisq, 0)
  expect_equal(row$p_value, 1)

  # Unequal prevalences: 9/10 vs 2/10, with repeated occurrences in each
  # positive sequence, checked against an independent contingency test.
  positive <- c(rep(TRUE, 9), FALSE, rep(TRUE, 2), rep(FALSE, 8))
  data <- data.frame(actor = rep(seq_len(20), each = 4),
    action = unlist(lapply(positive, function(p) if (p) c("A", "B", "A", "B") else c("A", "C", "A", "C"))),
    group = rep(rep(c("first", "second"), each = 10), each = 4))
  patterns <- jtna_table(pattern_result(data, "GroupTNA")$patterns$table)
  row <- patterns[patterns$pattern == "A->B", ]
  reference <- suppressWarnings(chisq.test(matrix(c(9, 1, 2, 8), 2, byrow = TRUE), correct = FALSE))
  expect_equal(row$count, 11L)
  expect_equal(row$frequency, 22L)
  expect_equal(row$chisq, unname(reference$statistic))
  expect_equal(row$p_value, reference$p.value)
})

test_that("association supports singleton groups and universal patterns", {
  data <- data.frame(actor = rep(seq_len(4), each = 3), action = rep(c("A", "B", "C"), 4),
    group = rep(c("one", "many", "many", "many"), each = 3))
  patterns <- jtna_table(pattern_result(data, "GroupTNA")$patterns$table)
  expect_equal(patterns$count_one, c(1L, 1L))
  expect_equal(patterns$count_many, c(3L, 3L))
  expect_equal(patterns$chisq, c(0, 0))
  expect_equal(patterns$p_value, c(1, 1))
})

test_that("literal delimiters produce distinct patterns and correct lift", {
  data <- data.frame(actor = rep(seq_len(20), each = 3),
    action = rep(c("A->B", "C", "X", "A", "B->C", "X"), 10))
  patterns <- jtna_table(pattern_result(data)$patterns$table)
  first <- match('"A->B"->"C"', patterns$pattern)
  second <- match('"A"->"B->C"', patterns$pattern)
  expect_false(anyNA(c(first, second)))
  expect_equal(patterns$frequency[c(first, second)], c(10L, 10L))
  expect_equal(patterns$support[c(first, second)], c(.5, .5))
  expect_equal(patterns$lift[c(first, second)], c(2, 2))
})

test_that("literal wildcard states and gapped wildcard syntax stay distinct", {
  data <- data.frame(actor = rep(seq_len(20), each = 3), action = rep(c("*", "A", "B", "C", "A", "B"), 10))
  patterns <- jtna_table(pattern_result(data)$patterns$table)
  expect_equal(patterns$lift[patterns$pattern == '"*"->"A"'], 1)
  gapped <- jtna_table(pattern_result(data, pattern_type = "gapped", pattern_gap_max = 1)$patterns$table)
  expect_true('"*"->*->"B"' %in% gapped$pattern)
  expect_equal(gapped$lift[gapped$pattern == '"*"->*->"B"'], 1)
})

test_that("state filters match exact labels including regex characters and empty labels", {
  data <- data.frame(actor = rep(seq_len(10), each = 6), action = rep(c("A", "B", "X", "AB", "C", "X"), 10))
  patterns <- jtna_table(pattern_result(data, pattern_starts_with_use = TRUE, pattern_starts_with = "A")$patterns$table)
  expect_identical(patterns$pattern, "A->B")
  data$action <- rep(c("A.", "B", "X", "AB", "C", "X"), 10)
  patterns <- jtna_table(pattern_result(data, pattern_contains_use = TRUE, pattern_contains = "A.")$patterns$table)
  expect_identical(patterns$pattern, "A.->B")
  data$action <- rep(c("A", "", "B", "A", "", "B"), 10)
  patterns <- jtna_table(pattern_result(data, pattern_starts_with_use = TRUE, pattern_starts_with = "")$patterns$table)
  expect_identical(patterns$pattern, '""->"B"')
})

test_that("indices preserve empty labels and original dominant-state tie ordering", {
  sequences <- data.frame(t1 = c("A", "é"), t2 = c("", "ä"), t3 = c("B", "é"),
    t4 = c("A", "ä"), t5 = c("", "é"), t6 = c("B", "ä"))
  prepared <- list(sequence_data = sequences)
  options <- list(indices_omega = 1, indices_favorable = "")
  indices <- .jtna_indices_df(prepared, options)
  expect_equal(indices$valid_n, c(6L, 6L))
  expect_equal(indices$unique_states, c(3L, 2L))
  expect_identical(indices$dominant_state, c("", sort(c("é", "ä"))[1L]))
  expect_equal(indices$integrative_potential[1], (2 + 5) / sum(1:6))
  expect_equal(indices$dominant_max_spell, c(1L, 1L))
})

test_that("ordinary-label patterns retain codyna frequencies, ordering and lift", {
  sequences <- as.data.frame(matrix(c("A", "B", "A", "C", "A", "B", "B", "A", "C", "A", "A", "C"), nrow = 3, byrow = TRUE))
  encoding <- .jtna_encode_states(sequences)
  invisible(lapply(c("ngram", "gapped", "repeated"), function(type) {
    direct <- codyna::discover_patterns(sequences, type = type, len = 2:3, gap = 1:2, min_freq = 1)
    encoded <- codyna::discover_patterns(encoding$data, type = type, len = 2:3, gap = 1:2, min_freq = 1)
    expect_identical(.jtna_decode_patterns(encoded$pattern, encoding), direct$pattern)
    expect_equal(encoded$frequency, direct$frequency)
    expect_equal(encoded$lift, direct$lift)
  }))
})
