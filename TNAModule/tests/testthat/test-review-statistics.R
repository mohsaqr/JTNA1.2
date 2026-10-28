skip_if_not_installed("tna")
skip_if_not_installed("pkgload")

test_that("requesting only an unavailable mosaic still explains its requirement", {
  data <- jtna_small()
  invisible(lapply(c("TNA", "GroupTNA", "ClusterTNA"), function(analysis) {
    args <- list(
      analysis = analysis, data = data,
      buildModel_variables_long_action = "action",
      buildModel_variables_long_actor = "actor",
      buildModel_show_plot = FALSE, buildModel_show_mosaic = TRUE)
    if (analysis == "GroupTNA") args$buildModel_variables_long_group <- "group"
    if (analysis == "ClusterTNA") args$clustering_run <- TRUE
    result <- do.call(jtna_run, args)
    expect_false(result$model$mosaic$visible, info = analysis)
    expect_true(result$model$visible, info = analysis)
    expect_match(jtna_notices(result$model), "needs a Frequency model", info = analysis)
    expect_length(jtna_errors(result), 0L)
  }))
})

test_that("sequence comparison preserves literal asterisks in action labels", {
  data <- data.frame(
    actor = rep(seq_len(20), each = 6),
    action = rep(c("*", "A*", "B", "*", "A*", "B"), 20),
    group = rep(c("G1", "G2"), each = 60))
  result <- jtna_run(
    "GroupTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group",
    compare_sequences_show_table = TRUE,
    compare_sequences_sub_min = 2, compare_sequences_sub_max = 2,
    compare_sequences_min_freq = 1, random_seed_use = TRUE, random_seed = 7)
  reference <- .jtna_with_seed(7, function() tna::compare_sequences(
    result$modelCache$state, sub = 2L, min_freq = 1L))
  actual <- jtna_table(result$compareSequences$table)
  expect_length(jtna_errors(result), 0L)
  expect_setequal(actual$pattern, c("*->A*", "A*->B", "B->*"))
  expect_equal(actual$length, rep(2L, 3))
  matched <- reference[match(actual$pattern, reference$pattern), ]
  expect_equal(actual$freq_G1, matched$freq_G1)
  expect_equal(actual$freq_G2, matched$freq_G2)
  expect_equal(actual$p_value, matched$p_value)
})

test_that("action labels containing arrows cannot merge distinct sequence patterns", {
  first <- c(rep("A->B", 8), rep("A", 2), rep("A->B", 2), rep("A", 8))
  second <- ifelse(first == "A->B", "C", "B->C")
  data <- data.frame(
    actor = rep(seq_len(20), each = 2),
    action = as.vector(rbind(first, second)),
    group = rep(c("G1", "G2"), each = 20))
  run <- function(data) jtna_run(
    "GroupTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group",
    compare_sequences_show_table = TRUE,
    compare_sequences_sub_min = 2, compare_sequences_sub_max = 2,
    compare_sequences_min_freq = 1, random_seed_use = TRUE, random_seed = 19)
  result <- run(data)
  actual <- jtna_table(result$compareSequences$table)
  expect_length(jtna_errors(result), 0L)
  expect_setequal(actual$pattern, c('"A->B"->"C"', '"A"->"B->C"'))
  expect_equal(actual$length, c(2L, 2L))
  expect_equal(actual$freq_G1[match('"A->B"->"C"', actual$pattern)], 8)
  expect_equal(actual$freq_G2[match('"A->B"->"C"', actual$pattern)], 2)

  # Renaming states is statistically neutral. The direct tna result on
  # safe labels supplies a reference without its delimiter collision.
  safe <- data
  safe$action <- paste0("state", match(safe$action, sort(unique(data$action))))
  model <- run(safe)$modelCache$state
  reference <- .jtna_with_seed(19, function() tna::compare_sequences(
    model, sub = 2L, min_freq = 1L))
  key <- c('"A"->"B->C"' = "state1->state3", '"A->B"->"C"' = "state2->state4")
  matched <- reference[match(unname(key[actual$pattern]), reference$pattern), ]
  expect_equal(actual$freq_G1, matched$freq_G1)
  expect_equal(actual$freq_G2, matched$freq_G2)
  expect_equal(actual$p_value, matched$p_value)
  expect_equal(actual$effect_size, matched$effect_size)
  expect_s3_class(result$compareSequences$plot$state, "tna_sequence_comparison")
  expect_equal(attr(result$compareSequences$plot$state, "freq_cols"), c("freq_G1", "freq_G2"))
})
