# One option at a time, every value: starting from an "everything on"
# configuration, each option of each analysis is set to every value its
# .a.yaml allows (each list value, the other boolean, numeric min and max,
# a sample text). No run may end in an unclassed R error or a failed
# section, except the failures listed in `expected_failure`; plot options
# also have their plots drawn. Slow: set JTNA_EXTENSIVE=true to run.

skip_if_not_installed("tna")
skip_if_not_installed("pkgload")
skip_if_not_installed("yaml")
skip_if_not(identical(Sys.getenv("JTNA_EXTENSIVE"), "true"), "set JTNA_EXTENSIVE=true for the option sweep")

sweep_data <- jtna_small(n_actor = 24, per = 16)

base_config <- function(analysis) {
  common <- list(
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
    buildModel_variables_long_time = "time",
    buildModel_show_matrix = TRUE, buildModel_show_plot = TRUE, buildModel_show_histo = TRUE,
    buildModel_show_frequencies = TRUE,
    centrality_show_table = TRUE, centrality_show_plot = TRUE,
    community_show_table = TRUE, community_show_plot = TRUE, community_methods = "walktrap",
    cliques_show_plot = TRUE, cliques_threshold = 0.05,
    bootstrap_show_table = TRUE, bootstrap_show_plot = TRUE, bootstrap_iteration = 10,
    sequences_show_index = TRUE, sequences_show_distribution = TRUE,
    pattern_show_table = TRUE, pattern_min_count = 1, indices_show_table = TRUE)
  switch(analysis,
    TNA = c(common, centrality_stability_show_table = TRUE, centrality_stability_iteration = 10,
            edgeBetweenness_show_table = TRUE, edgeBetweenness_show_plot = TRUE),
    GroupTNA = c(common, buildModel_variables_long_group = "group",
                 permutation_show_text = TRUE, permutation_show_plot = TRUE, permutation_iter = 10,
                 compare_show_summary = TRUE, compare_show_network = TRUE, compare_show_plot = TRUE,
                 compare_show_network_diff_plot = TRUE, compare_sequences_show_table = TRUE,
                 compare_sequences_min_freq = 2, compare_sequences_sub_max = 3),
    ClusterTNA = c(common, clustering_run = TRUE, clustering_k = 2,
                   permutation_show_text = TRUE, permutation_iter = 10,
                   compare_show_summary = TRUE, compare_show_network = TRUE,
                   compare_sequences_show_table = TRUE, compare_sequences_min_freq = 2,
                   compare_sequences_sub_max = 3))
}

# Values to try for one option definition.
option_values <- function(o) {
  heavy <- grepl("iter|iteration", o$name)  # keep resampling small
  switch(o$type,
    List = lapply(o$options, \(x) if (is.list(x)) x$name else x),
    Bool = list(!isTRUE(o$default)),
    Integer = , Number = {
      vals <- list(o$min, if (!heavy) o$max)
      Filter(\(v) !is.null(v) && !identical(v, o$default), vals)
    },
    String = list("Sample text"),
    list())
}

# Failures that are the correct outcome for that setting.
expected_failure <- list(
  permutation_paired = "Paired permutation tests",
  compare_cluster_i = "choose clusters between",
  compare_cluster_j = "choose clusters between",
  cliques_size = "no cliques",
  pattern_len_min = "minimum length is larger",
  pattern_gap_min = "minimum gap is larger",
  compare_sequences_sub_min = "minimum pattern length is larger",
  compare_sequences_min_freq = "No common patterns",
  community_methods = "unconnected graph",
  bootstrap_range_low = "consistency_range",
  bootstrap_range_up = "consistency_range",
  # Grouped models scaled across groups (tna's default): tna refuses
  # bootstrap and permutation tests, and its communities() and compare()
  # fail (tna 1.3.1 drops the state names from the scaled matrices).
  buildModel_scaling = c("globally scaled", "differing number of rows", "replacement has"))

# Data-dependent outcomes that are correct for any setting: small clusters
# can share no pattern often enough to compare.
always_allowed <- "No common patterns"

skip_options <- c("data", "clustering_run", "buildModel_type")  # model types: test-integration-grid.R

sweep_one <- function(analysis, o, value) {
  config <- utils::modifyList(base_config(analysis), stats::setNames(list(value), o$name))
  label <- sprintf("%s %s = %s", analysis, o$name, format(value))
  r <- tryCatch(do.call(jtna_run, c(list(analysis, data = sweep_data), config)),
                jtna_bad_input = function(e) e)
  if (inherits(r, "jtna_bad_input")) {
    expect_true(o$name %in% c("buildModel_threshold", "clustering_k"),
                label = paste(label, "rejected:", conditionMessage(r)))
    return(invisible(1L))
  }
  errors <- unname(jtna_errors(r))
  allowed <- c(expected_failure[[o$name]], always_allowed)
  errors <- errors[!vapply(errors, \(e) any(vapply(allowed, \(a) grepl(a, e, fixed = TRUE), logical(1))),
                           logical(1))]
  expect_identical(errors, character(0), label = label)
  if (grepl("_plot_|^sequences_|^buildModel_show", o$name)) {
    lapply(jtna_images(r), \(image) {
      path <- tempfile(fileext = ".png")
      on.exit(unlink(path), add = TRUE)
      expect_no_warning(image$saveAs(path), class = "jtna_plot_failed",
                        message = paste(label, image$name))
    })
  }
  invisible(1L)
}

invisible(lapply(c("TNA", "GroupTNA", "ClusterTNA"), \(analysis) {
  test_that(sprintf("%s: every option value runs cleanly", analysis), {
    a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml")))
    swept <- Filter(\(o) !(o$name %in% skip_options) && !(o$type %in% c("Data", "Variable", "Level")),
                    a$options)
    runs <- unlist(lapply(swept, \(o) lapply(option_values(o), \(value) sweep_one(analysis, o, value))))
    expect_gt(length(runs), 50L)
  })
}))
