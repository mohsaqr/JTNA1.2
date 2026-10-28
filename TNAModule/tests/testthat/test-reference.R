# Layer 2: JTNA's numbers against independent references (base R counting,
# igraph on the weight matrix, index formulas), not against tna or codyna.

skip_if_not_installed("tna")
skip_if_not_installed("igraph")
skip_if_not_installed("pkgload")

tol <- sqrt(.Machine$double.eps)
run_ref <- function(..., data = jtna_small()) {
  jtna_run("TNA", data = data, buildModel_variables_long_action = "action",
           buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time", ...)
}
prepared_of <- function(data = jtna_small()) {
  suppressMessages(tna::prepare_data(data, actor = "actor", time = "time", action = "action"))
}
matrix_of <- function(table) {
  df <- jtna_table(table)
  m <- as.matrix(df[setdiff(names(df), "from")])
  dimnames(m) <- list(df$from, NULL)
  m
}

test_that("the relative transition matrix equals counts divided by row totals", {
  r <- run_ref(buildModel_show_matrix = TRUE)
  seqs <- ref_sequences(prepared_of())
  states <- r$modelCache$state$labels
  expected <- ref_relative(ref_counts(seqs, states))
  expect_equal(unname(matrix_of(r$model$matrix)), unname(expected), tolerance = tol)
  expect_equal(jtna_table(r$model$inits)$prob, ref_inits(seqs, states), tolerance = tol)
})

test_that("the frequency matrix equals the raw transition counts", {
  r <- run_ref(buildModel_show_matrix = TRUE, buildModel_type = "frequency")
  seqs <- ref_sequences(prepared_of())
  expected <- ref_counts(seqs, r$modelCache$state$labels)
  expect_equal(unname(matrix_of(r$model$matrix)), unname(expected * 1), tolerance = tol)
})

test_that("centralities equal strengths and igraph path measures on the weights", {
  r <- run_ref(centrality_show_table = TRUE, centrality_Closeness = TRUE)
  got <- jtna_table(r$centrality$table)
  ref <- ref_centralities(r$modelCache$state$weights)
  expect_identical(got$state, ref$state)
  lapply(c("OutStrength", "InStrength", "Betweenness", "Closeness"), \(m) {
    expect_equal(got[[m]], ref[[m]], tolerance = tol, label = m)
  })
})

test_that("bigram frequencies and counts equal hand counts", {
  r <- run_ref(pattern_show_table = TRUE, pattern_len_min = 2, pattern_len_max = 2,
               pattern_min_count = 1, pattern_min_support = 0, pattern_table_show_all = TRUE)
  got <- jtna_table(r$patterns$table)
  ref <- ref_bigrams(ref_sequences(prepared_of()))
  expect_setequal(got$pattern, ref$pattern)
  expect_equal(nrow(got), nrow(ref))
  ref <- ref[match(got$pattern, ref$pattern), ]
  expect_false(anyNA(ref$pattern))
  expect_equal(got$frequency, ref$frequency)
  expect_equal(got$count, ref$count)
})

test_that("the bigram oracle treats internal missing events as adjacency breaks", {
  seqs <- rbind(c("A", NA, "B", "A"), c("A", "B", "A", "B"),
                 c("A", "B", "A", "B"))
  ref <- ref_bigrams(seqs)
  expect_setequal(ref$pattern, c("A->B", "B->A"))
  ordered <- ref[match(c("A->B", "B->A"), ref$pattern), ]
  expect_identical(ordered$frequency, c(4L, 3L))
  expect_identical(ordered$count, c(2L, 3L))
  direct <- codyna::discover_patterns(as.data.frame(seqs), len = 2, gap = 1,
                                      min_freq = 1, min_support = 0)
  expect_setequal(direct$pattern, ref$pattern)
  ref <- ref[match(direct$pattern, ref$pattern), ]
  expect_equal(direct$frequency, ref$frequency)
  expect_equal(direct$count, ref$count)
  expect_equal(nrow(ref_bigrams(matrix(c("A", NA, "B"), 1))), 0L)
  expect_equal(nrow(ref_bigrams(matrix("A", 1))), 0L)
})

test_that("all literal-label bigrams match independent tuple counts", {
  seqs <- rbind(
    c("A->B", "C", "A->B", "C"),
    c("A", "B->C", "A", "B->C"),
    c("A->B", "C", NA, NA),
    c("", "A", "", "A"),
    c("", "A", NA, "C"),
    c("A", NA, "C", "D"),
    c("*", "%", "*", "%"),
    c('quote"', "slash\\", 'quote"', "slash\\"))
  ref <- ref_bigrams(seqs)
  # Both tuples have the same naive joined label, but different counts.
  collision <- ref[ref$pattern == "A->B->C", ]
  expect_equal(nrow(collision), 2L)
  expect_equal(collision$frequency[match(c("A->B", "A"), collision$from)], c(3L, 2L))
  expect_equal(collision$count[match(c("A->B", "A"), collision$from)], c(2L, 1L))
  expect_false(any(ref$from == "A" & ref$to == "C"))
  expect_equal(ref$frequency[ref$from == "" & ref$to == "A"], 3L)
  # Formatting is performed only after counting; the counts do not depend
  # on the wrapper's tokenizer, encoding, or pattern-discovery machinery.
  displayed <- paste(encodeString(ref$from, quote = '"'),
                      encodeString(ref$to, quote = '"'), sep = "->")
  data <- data.frame(actor = rep(seq_len(nrow(seqs)), each = ncol(seqs)),
                     action = as.vector(t(seqs)))
  r <- jtna_run("TNA", data = data, buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor", buildModel_show_plot = FALSE,
    pattern_show_table = TRUE, pattern_len_min = 2, pattern_len_max = 2,
    pattern_min_count = 1, pattern_min_support = 0, pattern_table_show_all = TRUE)
  got <- jtna_table(r$patterns$table)
  expect_length(jtna_errors(r), 0L)
  expect_setequal(got$pattern, displayed)
  expect_equal(nrow(got), nrow(ref))
  matched <- match(got$pattern, displayed)
  expect_false(anyNA(matched))
  expect_equal(got$frequency, ref$frequency[matched])
  expect_equal(got$count, ref$count[matched])
})

test_that("sequence indices equal their definitions", {
  r <- run_ref(indices_show_table = TRUE, indices_table_show_all = TRUE)
  got <- jtna_table(r$indices$table)
  ref <- ref_indices(ref_sequences(prepared_of()))
  lapply(names(ref), \(col) {
    if (is.numeric(ref[[col]])) {
      expect_equal(as.numeric(got[[col]]), as.numeric(ref[[col]]), tolerance = tol, label = col)
    } else {
      expect_identical(as.character(got[[col]]), ref[[col]], label = col)
    }
  })
})

test_that("dominant max spell is per sequence (codyna 1.0.0 repeats the last row's value)", {
  seqs <- data.frame(T1 = c("a", "b", "a"), T2 = c("a", "b", "b"),
                     T3 = c("a", "a", "b"), T4 = c("b", "a", "b"), stringsAsFactors = FALSE)
  raw <- codyna::sequence_indices(seqs)
  expect_identical(.jtna_dominant_spell(seqs, raw$dominant_state), c(3L, 2L, 3L))
})

test_that("edge betweenness is directed (tna's default) and equals igraph on 1/weight", {
  r <- run_ref(edgeBetweenness_show_table = TRUE, edgeBetweenness_show_plot = TRUE)
  weights <- r$modelCache$state$weights
  dimnames(weights) <- rep(list(r$modelCache$state$labels), 2L)
  g <- igraph::graph_from_adjacency_matrix(weights, mode = "directed", weighted = TRUE, diag = TRUE)
  edges <- igraph::as_data_frame(g, what = "edges")
  edges$value <- igraph::edge_betweenness(g, directed = TRUE, weights = 1 / edges$weight)
  edges <- subset(edges, value > 0)
  got <- jtna_table(r$edgeBetweenness$table)
  expect_equal(nrow(got), nrow(edges))
  expect_equal(got$value, edges$value[match(paste(got$from, got$to), paste(edges$from, edges$to))],
               tolerance = tol)
  # Directed: A->B and B->A get their own values, so the network is not symmetric.
  eb <- r$edgeBetweenness$plot$state$weights
  expect_false(isSymmetric(unname(eb)))
})
