# Layer 4: every section of every analysis, across model types and scalings,
# through the real jamovi classes. No section may fail (except where tna
# itself does not support a scaled grouped model) and every plot must draw.
# Plus a static check that each cache is cleared by the options its
# computation reads.

skip_if_not_installed("tna")
skip_if_not_installed("pkgload")
skip_on_cran()

everything_on <- function(analysis) {
  common <- list(
    buildModel_show_matrix = TRUE, buildModel_show_plot = TRUE, buildModel_show_histo = TRUE,
    buildModel_show_frequencies = TRUE, buildModel_show_mosaic = TRUE,
    centrality_show_table = TRUE, centrality_show_plot = TRUE,
    community_show_table = TRUE, community_show_plot = TRUE, community_methods = "walktrap",
    cliques_show_plot = TRUE, cliques_threshold = 0.05,
    bootstrap_show_table = TRUE, bootstrap_show_plot = TRUE, bootstrap_iteration = 10,
    sequences_show_index = TRUE, sequences_show_distribution = TRUE,
    pattern_show_table = TRUE, pattern_min_count = 1, indices_show_table = TRUE)
  single <- list(
    centrality_stability_show_table = TRUE, centrality_stability_show_plot = TRUE,
    centrality_stability_iteration = 10,
    edgeBetweenness_show_table = TRUE, edgeBetweenness_show_plot = TRUE)
  grouped <- list(
    permutation_show_text = TRUE, permutation_show_plot = TRUE, permutation_iter = 10,
    compare_show_summary = TRUE, compare_show_network = TRUE, compare_show_plot = TRUE,
    compare_show_network_diff_plot = TRUE,
    compare_sequences_show_table = TRUE, compare_sequences_show_plot = TRUE,
    compare_sequences_min_freq = 2, compare_sequences_sub_max = 3)
  roles <- list(buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
                buildModel_variables_long_time = "time")
  switch(analysis,
    TNA = c(roles, common, single),
    GroupTNA = c(roles, common, grouped, buildModel_variables_long_group = "group"),
    ClusterTNA = c(roles, common, grouped, clustering_run = TRUE, clustering_k = 2))
}
run_all <- function(analysis, ...) {
  do.call(jtna_run, c(list(analysis, data = jtna_small(n_actor = 24, per = 20)),
                      utils::modifyList(everything_on(analysis), list(...))))
}

test_that("every section runs and every plot draws, for every model type", {
  grid <- expand.grid(analysis = c("TNA", "GroupTNA", "ClusterTNA"),
                      type = c("relative", "frequency", "attention"), stringsAsFactors = FALSE)
  invisible(Map(\(analysis, type) {
    r <- run_all(analysis, buildModel_type = type)
    label <- paste(analysis, type)
    expect_identical(jtna_errors(r), character(0), label = label)
    images <- jtna_images(r)
    expect_gt(length(images), 10L)
    lapply(images, \(image) {
      path <- tempfile(fileext = ".png")
      on.exit(unlink(path), add = TRUE)
      expect_no_warning(image$saveAs(path), class = "jtna_plot_failed",
                        message = paste(label, image$name))
    })
  }, grid$analysis, grid$type))
})

test_that("every scaling: sections run as tna allows, and every plot draws", {
  # Grouped models are scaled across groups, tna's default. tna refuses
  # bootstrap and permutation tests on them by design, and in tna 1.3.1
  # communities() and compare() fail on them (the scaled weight matrices
  # lose their state names). Every other section must run.
  tna_limited <- c("community", "bootstrap", "compare", "compareSequences")
  grid <- expand.grid(analysis = c("TNA", "GroupTNA", "ClusterTNA"),
                      scaling = c("minmax", "max", "rank"), stringsAsFactors = FALSE)
  invisible(Map(\(analysis, scaling) {
    r <- run_all(analysis, buildModel_scaling = scaling)
    label <- paste(analysis, scaling)
    errors <- jtna_errors(r)
    failed <- sub("\\..*$", "", names(errors) %||% character(0))
    expect_setequal(failed, if (analysis == "TNA") character(0) else tna_limited)
    if (analysis != "TNA") {
      expect_match(errors[["bootstrap.notice_error"]], "not supported for grouped models with globally scaled",
                   label = label)
      expect_match(errors[["compareSequences.notice_error"]],
                   "not supported for grouped models with globally scaled", label = label)
    }
    lapply(jtna_images(r), \(image) {
      path <- tempfile(fileext = ".png")
      on.exit(unlink(path), add = TRUE)
      expect_no_warning(image$saveAs(path), class = "jtna_plot_failed",
                        message = paste(label, image$name))
    })
  }, grid$analysis, grid$scaling))
})

test_that("the transition matrix shows state names when tna's scaling drops them", {
  r <- run_all("GroupTNA", buildModel_scaling = "minmax")
  model <- r$modelCache$state
  expect_null(rownames(model[[1]]$weights))
  table <- jtna_table(r$model$matrices$get(key = names(model)[1]))
  expect_identical(table$from, model[[1]]$labels)
})

test_that("each cache is cleared by every option its computation reads, and only real options", {
  skip_if_not_installed("yaml")
  jamovi_dir <- testthat::test_path("..", "..", "jamovi")
  model_opts <- c("buildModel_variables_long_action", "buildModel_variables_long_actor",
                  "buildModel_variables_long_time", "buildModel_variables_long_order",
                  "buildModel_type", "buildModel_scaling", "buildModel_threshold", "buildModel_lambda")
  needs <- list(
    "bootstrap/plot" = c(paste0("bootstrap_", c("iteration", "level", "method", "range_low",
                                                "range_up", "threshold")), "random_seed_use", "random_seed"),
    "community/plot" = c("community_methods", "community_gamma", "random_seed_use", "random_seed"),
    "cliques/plot" = c("cliques_size", "cliques_threshold", "cliques_sum_weights"),
    "centrality/table" = c("centrality_loops", "centrality_normalize", "centrality_invert"),
    "patterns/table" = c("pattern_type", "pattern_len_min", "pattern_len_max", "pattern_gap_min",
                         "pattern_gap_max", "pattern_min_support", "pattern_min_count"),
    "indices/table" = c("indices_favorable", "indices_omega"))
  grouped_needs <- list(
    "permutation/plot" = c("permutation_iter", "permutation_paired", "permutation_level",
                           "permutation_adjust", "random_seed_use", "random_seed"),
    "compareSequences/plot" = c("compare_sequences_sub_min", "compare_sequences_sub_max",
                                "compare_sequences_min_freq", "compare_sequences_correction",
                                "random_seed_use", "random_seed"))
  single_needs <- list(
    "stability/plot" = c("centrality_stability_iteration", "centrality_stability_threshold",
                         "centrality_stability_certainty", "centrality_invert", "random_seed_use", "random_seed"))
  extra_model <- list(TNA = character(0), GroupTNA = "buildModel_variables_long_group",
                      ClusterTNA = c("clustering_k", "clustering_dissimilarity", "clustering_method",
                                     "clustering_weighted", "clustering_lambda", "random_seed_use", "random_seed"))
  invisible(lapply(names(extra_model), \(analysis) {
    r <- yaml::read_yaml(file.path(jamovi_dir, paste0(analysis, ".r.yaml")))
    a <- yaml::read_yaml(file.path(jamovi_dir, paste0(analysis, ".a.yaml")))
    option_names <- vapply(a$options, \(o) o$name, character(1))
    elements <- list()
    collect <- function(items, prefix = "") {
      lapply(items, \(it) {
        path <- paste0(prefix, it$name)
        if (!is.null(it$clearWith)) elements[[path]] <<- unlist(it$clearWith)
        if (!is.null(it$items)) collect(it$items, paste0(path, "/"))
      })
    }
    collect(r$items)
    model <- c(model_opts, extra_model[[analysis]])
    expect_setequal(elements$modelCache, model)
    all_needs <- c(needs, if (analysis == "TNA") single_needs else grouped_needs)
    lapply(names(all_needs), \(el) {
      expect_true(all(c(model, all_needs[[el]]) %in% elements[[el]]),
                  label = sprintf("%s %s clearWith: missing %s", analysis, el,
                                  toString(setdiff(c(model, all_needs[[el]]), elements[[el]]))))
    })
    stray <- setdiff(unique(unlist(elements)), option_names)
    expect_identical(stray, character(0), label = paste(analysis, "clearWith names that are not options"))
  }))
})
