# Phase 1 (1.9.1): settings that were missing or ignored. Each test checks
# that the setting changes the result AND that JTNA's table equals a direct
# tna call with the same arguments (wrapper equivalence).

skip_if_not_installed("tna")
skip_if_not_installed("pkgload")

tol <- sqrt(.Machine$double.eps)
small_args <- function(...) {
  list(buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor", ...)
}
run_small <- function(analysis, ..., data = jtna_small()) {
  do.call(jtna_run, c(list(analysis, data = data), small_args(...)))
}
group_run <- function(...) run_small("GroupTNA", buildModel_variables_long_group = "group",
                                     random_seed_use = TRUE, ...)
prepared_small <- function(data = jtna_small()) {
  suppressMessages(tna::prepare_data(data, actor = "actor", action = "action"))
}
with_seed <- function(seed, expr) .jtna_with_seed(seed, \() expr)

test_that("sequence comparison applies the chosen correction (was ignored)", {
  p_of <- function(correction) {
    r <- group_run(compare_sequences_show_table = TRUE, compare_sequences_correction = correction,
                   compare_sequences_sub_max = 3, compare_sequences_min_freq = 5)
    jtna_table(r$compareSequences$table)$p_value
  }
  none <- p_of("none")
  bonf <- p_of("bonferroni")
  expect_true(all(bonf >= none - tol))
  expect_true(any(bonf > none + tol))
  # Equivalence with tna for the same correction and seed.
  model <- group_run()$modelCache$state
  direct <- with_seed(1, tna::compare_sequences(model, sub = 2:3, min_freq = 5, adjust = "bonferroni"))
  direct <- as.data.frame(direct[!grepl("*", direct$pattern, fixed = TRUE), ])
  r <- group_run(compare_sequences_show_table = TRUE, compare_sequences_correction = "bonferroni",
                 compare_sequences_sub_max = 3, compare_sequences_min_freq = 5)
  tab <- jtna_table(r$compareSequences$table)
  expect_equal(tab$p_value, direct$p_value[match(tab$pattern, direct$pattern)], tolerance = tol)
})

test_that("the permutation test applies the chosen correction and matches tna", {
  p_of <- function(adjust) {
    r <- group_run(permutation_show_text = TRUE, permutation_iter = 50, permutation_adjust = adjust,
                   permutation_table_show_all = TRUE)
    tab <- jtna_table(r$permutation$table)
    stats::setNames(tab$p_value, tab$edge_name)
  }
  none <- p_of("none")
  bh <- p_of("BH")
  expect_equal(bh[names(none)], stats::p.adjust(none, "BH")[names(none)], tolerance = tol)
  model <- group_run()$modelCache$state
  direct <- with_seed(1, tna::permutation_test(model, iter = 50, adjust = "BH"))
  stats_direct <- direct[[1]]$edges$stats
  expect_equal(bh[trimws(stats_direct$edge_name)], stats_direct$p_value, tolerance = tol,
               ignore_attr = TRUE)
})

test_that("the random seed makes resampling repeat exactly, and changes with the seed", {
  boot <- function(seed) {
    r <- run_small("TNA", bootstrap_show_table = TRUE, bootstrap_iteration = 30,
                   bootstrap_table_show_all = TRUE, random_seed_use = TRUE,
                   random_seed = seed)
    jtna_table(r$bootstrap$table)
  }
  expect_identical(boot(7), boot(7))
  expect_false(identical(boot(7)$p_value, boot(8)$p_value))
  # Equivalence with a direct, identically seeded call.
  model <- run_small("TNA")$modelCache$state
  direct <- with_seed(7, tna::bootstrap(model, iter = 30))
  tab <- boot(7)
  ref <- as.data.frame(direct$summary)
  key <- paste(tab$from, tab$to)
  expect_equal(tab$p_value, ref$p_value[match(key, paste(ref$from, ref$to))], tolerance = tol)
})

test_that("a seeded run leaves the session's random state untouched", {
  data <- jtna_small()  # the data helper sets its own seed: build it first
  set.seed(42)
  before <- .Random.seed
  run_small("TNA", data = data, bootstrap_show_table = TRUE, bootstrap_iteration = 10,
            random_seed_use = TRUE, random_seed = 3)
  expect_identical(.Random.seed, before)
})

test_that("centralities honour 'invert' and match tna", {
  cent <- function(invert) {
    r <- run_small("TNA", centrality_show_table = TRUE, centrality_Closeness = TRUE,
                   centrality_invert = invert)
    jtna_table(r$centrality$table)
  }
  inv <- cent(TRUE)
  raw <- cent(FALSE)
  expect_false(isTRUE(all.equal(inv$Closeness, raw$Closeness)))
  model <- run_small("TNA")$modelCache$state
  direct <- as.data.frame(tna::centralities(model, invert = FALSE,
    measures = c("OutStrength", "InStrength", "Closeness", "Betweenness")))
  expect_equal(raw$Closeness, direct$Closeness, tolerance = tol)
  expect_equal(raw$Betweenness, direct$Betweenness, tolerance = tol)
})

test_that("cliques honour 'sum of weights' and match tna", {
  n_cliques <- function(sum_weights) {
    r <- run_small("TNA", cliques_show_plot = TRUE, cliques_size = 2, cliques_threshold = 0.25,
                   cliques_sum_weights = sum_weights)
    length(r$cliques$plot$state$weights)
  }
  model <- run_small("TNA")$modelCache$state
  expect_equal(n_cliques(TRUE),
               length(tna::cliques(model, size = 2, threshold = 0.25, sum_weights = TRUE)$weights))
  expect_equal(n_cliques(FALSE),
               length(tna::cliques(model, size = 2, threshold = 0.25)$weights))
  expect_gte(n_cliques(TRUE), n_cliques(FALSE))
})

test_that("every clustering distance and method runs, and matches tna", {
  data <- jtna_small(n_actor = 30)
  seqs <- as.data.frame(prepared_small(data)$sequence_data)
  grid <- rbind(
    data.frame(dissimilarity = c("hamming", "osa", "lv", "dl", "lcs", "qgram", "cosine", "jaccard", "jw"),
               method = "pam"),
    data.frame(dissimilarity = "hamming",
               method = c("ward.D2", "ward.D", "complete", "average", "single", "mcquitty",
                          "median", "centroid")))
  invisible(Map(\(dis, met) {
    r <- run_small("ClusterTNA", data = data, clustering_run = TRUE, clustering_k = 3,
                   clustering_dissimilarity = dis, clustering_method = met)
    got <- attr(r$modelCache$state, "jtna_clusters")
    ref <- with_seed(1, tna::cluster_sequences(seqs, k = 3, dissimilarity = dis, method = met))
    expect_identical(got, paste("Cluster", ref$assignments), label = paste(dis, met))
  }, grid$dissimilarity, grid$method))
})

test_that("weighted Hamming clustering uses lambda", {
  data <- jtna_small(n_actor = 30)
  clusters <- function(lambda) {
    r <- run_small("ClusterTNA", data = data, clustering_run = TRUE, clustering_k = 3,
                   clustering_weighted = TRUE, clustering_lambda = lambda)
    attr(r$modelCache$state, "jtna_clusters")
  }
  seqs <- as.data.frame(prepared_small(data)$sequence_data)
  ref <- with_seed(1, tna::cluster_sequences(seqs, k = 3, weighted = TRUE, lambda = 0.5))
  expect_identical(clusters(0.5), paste("Cluster", ref$assignments))
})

test_that("sequence comparison keeps tna's class for plotting and shows the effect size", {
  r <- group_run(compare_sequences_show_table = TRUE, compare_sequences_show_plot = TRUE,
                 compare_sequences_min_freq = 5, compare_sequences_sub_max = 3)
  expect_s3_class(r$compareSequences$plot$state, "tna_sequence_comparison")
  tab <- jtna_table(r$compareSequences$table)
  expect_true(all(c("effect_size", "p_value") %in% names(tab)))
  expect_true(is.numeric(tab$effect_size))
  expect_no_warning({
    path <- tempfile(fileext = ".png"); on.exit(unlink(path), add = TRUE)
    r$compareSequences$plot$saveAs(path)
  }, class = "jtna_plot_failed")
})

test_that("defaults follow tna: no correction, no seed, missing values left out", {
  a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "GroupTNA.a.yaml"))
  defaults <- stats::setNames(lapply(a$options, \(o) o$default), vapply(a$options, \(o) o$name, ""))
  expect_identical(defaults$permutation_adjust,
                   eval(formals(getFromNamespace("permutation_test.group_tna", "tna"))$adjust))
  expect_identical(defaults$compare_sequences_correction,
                   eval(formals(getFromNamespace("compare_sequences.group_tna", "tna"))$adjust))
  expect_false(defaults$random_seed_use)
  include_na <- eval(formals(getFromNamespace("plot_sequences.group_tna", "tna"))$include_na)
  lapply(c("TNA", "GroupTNA", "ClusterTNA"), \(analysis) {
    a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml")))
    option <- Filter(\(o) o$name == "sequences_include_na", a$options)[[1]]
    expect_identical(option$default, include_na, label = analysis)
  })
  expect_null(.jtna_seed(list(random_seed_use = FALSE, random_seed = 5)))
  expect_identical(.jtna_seed(list(random_seed_use = TRUE, random_seed = 5)), 5)
})
