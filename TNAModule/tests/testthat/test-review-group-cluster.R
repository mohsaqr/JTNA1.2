skip_if_not_installed("tna")
skip_if_not_installed("pkgload")

test_that("a retained Hamming weight option does not break other distances", {
  data <- jtna_small(n_actor = 12)
  run <- function(weighted) jtna_run("ClusterTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor", clustering_run = TRUE,
    clustering_dissimilarity = "lv", clustering_weighted = weighted)
  ordinary <- run(FALSE)
  retained <- run(TRUE)
  expect_length(jtna_errors(retained), 0L)
  expect_identical(attr(retained$modelCache$state, "jtna_clusters"),
                   attr(ordinary$modelCache$state, "jtna_clusters"))
})

test_that("cluster pair order is numeric and scaled permutation is refused", {
  data <- jtna_small(n_actor = 30)
  run <- function(scaling) jtna_run("ClusterTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor", clustering_run = TRUE,
    clustering_k = 10, buildModel_scaling = scaling,
    permutation_show_text = TRUE, permutation_iter = 10,
    random_seed_use = TRUE, random_seed = 91)
  raw <- run("noScaling")
  scaled <- run("max")
  expect_identical(names(scaled$modelCache$state), paste("Cluster", 1:10))
  expect_match(jtna_notices(scaled$permutation), "globally scaled")
  expect_null(scaled$permutation$plot$state)
  expect_true("Cluster 2 vs. Cluster 10" %in% names(raw$permutation$plot$state))
})

test_that("actors reused across groups retain separate sequences and networks", {
  data <- data.frame(
    actor = rep(c("s1", "s2"), each = 8),
    group = rep(rep(c("before", "after"), each = 4), 2),
    action = rep(c("a", "b", "a", "b", "c", "d", "c", "d"), 2),
    time = rep(1:8, 2) + 1700000000)
  invisible(lapply(c(FALSE, TRUE), function(use_time) {
    r <- jtna_run("GroupTNA", data = data,
      buildModel_variables_long_action = "action",
      buildModel_variables_long_actor = "actor",
      buildModel_variables_long_group = "group",
      buildModel_variables_long_time = if (use_time) "time" else NULL,
      buildModel_type = "frequency", indices_show_table = TRUE)
    expect_length(jtna_errors(r), 0L)
    model <- r$modelCache$state
    expect_setequal(names(model), c("before", "after"))
    expect_equal(unname(model$before$weights["a", "b"]), 4)
    expect_equal(unname(model$after$weights["c", "d"]), 4)
    expect_equal(sum(model$before$weights[c("c", "d"), ]), 0)
    expect_equal(sum(model$after$weights[c("a", "b"), ]), 0)
    indices <- jtna_table(r$indices$table)
    expect_equal(nrow(indices), 4L)
    expect_equal(indices$valid_n, rep(4, 4))
    expect_setequal(paste(indices$actor, indices$group),
                    c("s1 before", "s2 before", "s1 after", "s2 after"))
  }))
})

test_that("missing group events are excluded consistently and reported", {
  data <- jtna_small(n_actor = 4)
  data$group[data$actor == "S04"] <- NA_character_
  r <- jtna_run("GroupTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group", indices_show_table = TRUE,
    buildModel_show_plot = FALSE)
  expect_length(jtna_errors(r), 0L)
  expect_setequal(names(r$modelCache$state), c("A", "B"))
  expect_equal(sum(vapply(r$modelCache$state, \(model) nrow(model$data), integer(1))), 3L)
  expect_match(jtna_notices(r$model), "Excluded 15 events with missing Group values")
  expect_true(r$model$visible)
  indices <- jtna_table(r$indices$table)
  expect_setequal(indices$actor, c("S01", "S02", "S03"))
  expect_false(anyNA(indices$group))
})

test_that("paired permutations match actors despite shuffled input rows", {
  data <- jtna_small(n_actor = 8, per = 8)
  data$group <- "before"
  after <- data
  after$group <- "after"
  after$action <- rev(after$action)
  data <- rbind(data, after)
  # Explicit order keeps each actor's sequence fixed when event rows move.
  data$position <- rep(seq_len(8), 16)
  run <- function(data) jtna_run("GroupTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group",
    buildModel_variables_long_order = "position",
    permutation_paired = TRUE, permutation_show_text = TRUE,
    permutation_iter = 20, random_seed_use = TRUE, random_seed = 13)
  original <- run(data)
  shuffled <- run(data[rev(seq_len(nrow(data))), ])
  expect_length(jtna_errors(original), 0L)
  expect_length(jtna_errors(shuffled), 0L)
  expect_equal(original$permutation$plot$state, shuffled$permutation$plot$state)
  direct <- .jtna_with_seed(13, \() tna::permutation_test(
    original$modelCache$state, paired = TRUE, iter = 20))
  expect_equal(original$permutation$plot$state, direct)
})

test_that("equal group sizes cannot substitute for matching paired actors", {
  r <- jtna_run("GroupTNA", data = jtna_small(n_actor = 8),
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group",
    permutation_paired = TRUE, permutation_show_text = TRUE,
    permutation_iter = 10, indices_show_table = TRUE)
  expect_match(jtna_notices(r$permutation), "require matching actors and session numbers")
  expect_null(r$permutation$plot$state)
  expect_equal(nrow(jtna_table(r$indices$table)), 8L)
})

test_that("paired groups must match sessions within each actor", {
  data <- data.frame(
    actor = rep(c("s1", "s2", "s2", "s3", "s1", "s1", "s2", "s3"), each = 4),
    group = rep(c("before", "after"), each = 16),
    time = rep(c(0, 0, 100, 0, 0, 100, 0, 0), each = 4) + rep(1:4, 8) + 1700000000,
    action = rep(c("a", "b", "a", "b", "b", "c", "c", "a"), 4))
  r <- jtna_run("GroupTNA", data = data,
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor",
    buildModel_variables_long_group = "group",
    buildModel_variables_long_time = "time", buildModel_threshold = 10,
    permutation_paired = TRUE, permutation_show_text = TRUE,
    permutation_iter = 10, indices_show_table = TRUE)
  expect_match(jtna_notices(r$permutation), "require matching actors and session numbers")
  expect_null(r$permutation$plot$state)
  expect_equal(nrow(jtna_table(r$indices$table)), 8L)
})

test_that("clusters do not infer matched pairs from sequence row positions", {
  r <- jtna_run("ClusterTNA", data = jtna_small(n_actor = 8),
    buildModel_variables_long_action = "action",
    buildModel_variables_long_actor = "actor", clustering_run = TRUE,
    permutation_paired = TRUE, permutation_show_text = TRUE,
    permutation_iter = 10, indices_show_table = TRUE)
  expect_match(jtna_notices(r$permutation), "Paired permutation tests are unavailable for clusters")
  expect_null(r$permutation$plot$state)
  expect_equal(nrow(jtna_table(r$indices$table)), 8L)
})
