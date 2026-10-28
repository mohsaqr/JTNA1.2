skip_if_not_installed("tna")

test_that("inferential defaults match upstream instead of a copied reference", {
  defaults <- lapply(c("TNA", "GroupTNA", "ClusterTNA"), function(analysis) {
    schema <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml")))
    stats::setNames(lapply(schema$options, function(option) option$default),
                    vapply(schema$options, function(option) option$name, character(1)))
  })
  expect_equal(defaults[[1]]$centrality_stability_iteration,
               eval(formals(getFromNamespace("estimate_cs.tna", "tna"))$iter))
  invisible(lapply(defaults, function(options) {
    expect_false(options$bootstrap_threshold_use)
    expect_equal(options$cliques_threshold,
                 eval(formals(getFromNamespace("cliques.tna", "tna"))$threshold))
  }))
  invisible(lapply(defaults[-1], function(options) {
    expect_equal(options$compare_sequences_min_freq,
                 eval(formals(getFromNamespace("compare_sequences.group_tna", "tna"))$min_freq))
  }))
})

test_that("the displayed zero clique threshold is honored literally", {
  data <- jtna_small(n_actor = 40, per = 30, actions = letters[1:12])
  result <- jtna_run("TNA", data = data,
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
    cliques_show_plot = TRUE, cliques_size = 2, cliques_threshold = 0)
  expected <- tna::cliques(result$modelCache$state, size = 2, threshold = 0)
  expect_equal(result$cliques$plot$state, expected)
  expect_gt(length(expected$weights), 10L)
})

test_that("blank and whitespace labels remain valid states and visible labels", {
  invisible(lapply(c("", " "), function(blank) {
    data <- data.frame(actor = rep(1:2, each = 4), action = rep(c("A", blank), 4))
    result <- jtna_run("TNA", data = data,
      buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
      buildModel_show_matrix = TRUE)
    model <- result$modelCache$state
    expect_setequal(model$labels, c("A", blank))
    expect_equal(unname(model$weights), matrix(c(0, 1, 1, 0), 2))
    expect_identical(result$model$matrix$asDF$from,
                     encodeString(model$labels, quote = '"', na.encode = FALSE))
    expect_false(anyNA(result$model$matrix$asDF$from))
  }))
  labels <- c("", '""', NA_character_, " ", "A")
  shown <- .jtna_display_labels(labels)
  expect_identical(shown, encodeString(labels, quote = '"', na.encode = FALSE))
  expect_equal(length(unique(shown[!is.na(shown)])), 4)
  expect_identical(.jtna_level("", "B"), "")
  expect_identical(.jtna_level(NULL, "B"), "B")
})
