# End-to-end tests: each analysis runs through its real jamovi class
# (R6 + jmvcore) headless, and the results tree is inspected. Each block
# pins one finding of the jamovi module audit of JTNA 1.7.1.

skip_if_not_installed("tna")
skip_if_not_installed("codyna")
skip_if_not_installed("pkgload")

tna_args <- function(...) {
  list(buildModel_variables_long_action = "action",
       buildModel_variables_long_actor = "actor", ...)
}
run_small <- function(analysis, ..., data = jtna_small()) {
  do.call(jtna_run, c(list(analysis, data = data), tna_args(...)))
}
render <- function(image) {
  path <- tempfile(fileext = ".png")
  on.exit(unlink(path), add = TRUE)
  image$saveAs(path)
}

# ---- HIGH ----------------------------------------------------------------

test_that("a community-detection failure is reported on that section only", {
  # Two components that never meet: spinglass cannot work on them.
  set.seed(3)
  dd <- data.frame(
    actor = rep(1:40, each = 10),
    action = c(replicate(20, sample(c("a", "b"), 10, TRUE)),
               replicate(20, sample(c("c", "d"), 10, TRUE))))
  r <- run_small("TNA", data = dd, community_show_table = TRUE,
                 community_methods = "spinglass", pattern_show_table = TRUE,
                 indices_show_table = TRUE)
  notices <- jtna_notices(r$community)
  expect_length(notices, 1L)
  expect_match(notices, "community detection could not be computed")
  # Sections after it still ran.
  expect_gt(nrow(jtna_table(r$patterns$table)), 0L)
  expect_equal(nrow(jtna_table(r$indices$table)), 40L)
})

test_that("frequencies and mosaic plots draw in all three analyses", {
  args <- list(buildModel_type = "frequency", buildModel_show_frequencies = TRUE,
               buildModel_show_mosaic = TRUE)
  runs <- list(
    TNA = do.call(run_small, c(list("TNA"), args)),
    GroupTNA = do.call(run_small, c(list("GroupTNA", buildModel_variables_long_group = "group"), args)),
    ClusterTNA = do.call(run_small, c(list("ClusterTNA", clustering_run = TRUE), args)))
  lapply(runs, \(r) {
    expect_true(r$model$mosaic$visible)
    expect_no_warning(render(r$model$frequencies), class = "jtna_plot_failed")
    expect_no_warning(render(r$model$mosaic), class = "jtna_plot_failed")
  })
})

test_that("mosaic is hidden, with a notice, unless the model is a frequency model", {
  r <- run_small("TNA", buildModel_show_mosaic = TRUE)
  expect_false(r$model$mosaic$visible)
  expect_match(jtna_notices(r$model), "needs a Frequency model")
})

test_that("ClusterTNA builds the model type, scaling and lambda chosen in the panel", {
  freq <- run_small("ClusterTNA", clustering_run = TRUE, buildModel_type = "frequency")$modelCache$state
  rel <- run_small("ClusterTNA", clustering_run = TRUE, buildModel_type = "relative")$modelCache$state
  scaled <- run_small("ClusterTNA", clustering_run = TRUE, buildModel_scaling = "minmax")$modelCache$state
  expect_identical(attr(freq[[1]], "type"), "frequency")
  expect_gt(max(freq[[1]]$weights), 1)
  expect_equal(unname(rowSums(rel[[1]]$weights)), rep(1, nrow(rel[[1]]$weights)))
  # Scaled across clusters (tna's default): the maximum over all clusters is 1.
  expect_equal(max(vapply(scaled, \(m) max(m$weights), numeric(1))), 1)
  lam <- function(l) run_small("ClusterTNA", clustering_run = TRUE, buildModel_type = "attention",
                               buildModel_lambda = l)$modelCache$state[[1]]$weights
  expect_false(isTRUE(all.equal(lam(0.1), lam(5))))
})

test_that("ClusterTNA labels clusters and passes them to patterns and indices", {
  r <- run_small("ClusterTNA", clustering_run = TRUE, clustering_k = 2,
                 pattern_show_table = TRUE, indices_show_table = TRUE)
  expect_identical(names(r$modelCache$state), c("Cluster 1", "Cluster 2"))
  expect_true(all(c("chisq", "count_Cluster 1") %in% names(jtna_table(r$patterns$table))))
  expect_true(all(jtna_table(r$indices$table)$cluster %in% c("Cluster 1", "Cluster 2")))
})

test_that("the transition matrix is a table: one row per from-state, rows sum to 1", {
  r <- run_small("TNA", buildModel_show_matrix = TRUE)
  m <- jtna_table(r$model$matrix)
  states <- sort(unique(jtna_small()$action))
  expect_identical(m$from, states)
  expect_equal(unname(rowSums(m[setdiff(names(m), "from")])), rep(1, length(states)))
  expect_equal(sum(jtna_table(r$model$inits)$prob), 1)
  g <- run_small("GroupTNA", buildModel_variables_long_group = "group", buildModel_show_matrix = TRUE)
  expect_identical(unlist(g$model$matrices$itemKeys), c("A", "B"))
  expect_equal(colSums(jtna_table(g$model$inits)[c("net1", "net2")]), c(net1 = 1, net2 = 1))
})

# ---- MEDIUM ----------------------------------------------------------------

test_that("plot settings reach the plotting call", {
  opts <- list(bootstrap_plot_cut = 0.3, bootstrap_plot_min_value = 0.02,
               bootstrap_plot_edge_label_size = 1.4, bootstrap_plot_node_size = 12,
               bootstrap_plot_node_label_size = 1.1, bootstrap_plot_layout = "spring")
  expect_identical(.jtna_network_args(opts, "bootstrap_plot"), list(
    cut = 0.3, minimum = 0.02, edge.label.cex = 1.4, node_size = 12,
    label.cex = 1.1, layout = "spring"))
})

test_that("TNA passes the favorable state and omega to sequence indices", {
  r <- run_small("TNA", indices_show_table = TRUE, indices_favorable = "plan", indices_omega = 2)
  idx <- jtna_table(r$indices$table)
  expect_true("integrative_potential" %in% names(idx))
  expect_true(r$indices$table$getColumn("integrative_potential")$visible)
})

test_that("Betweenness is a number column, so normalised values keep decimals", {
  r <- run_small("TNA", centrality_show_table = TRUE, centrality_normalize = TRUE)
  expect_identical(r$centrality$table$getColumn("Betweenness")$type, "number")
  g <- run_small("GroupTNA", buildModel_variables_long_group = "group", centrality_show_table = TRUE)
  expect_identical(g$centrality$table$getColumn("Betweenness")$type, "number")
})

test_that("degenerate input is rejected up front with a clear message", {
  expect_error(jtna_run("TNA", data = data.frame(a = rep("x", 10), b = 1:10),
                        buildModel_variables_long_action = "a"),
               class = "jtna_bad_input")
  one_group <- transform(jtna_small(), group = "A")
  expect_error(run_small("GroupTNA", data = one_group, buildModel_variables_long_group = "group"),
               class = "jtna_bad_input")
  expect_error(run_small("ClusterTNA", data = jtna_small(n_actor = 3), clustering_run = TRUE,
                         clustering_k = 5), class = "jtna_bad_input")
})

test_that("no diagnostic notes reach the results", {
  g <- run_small("GroupTNA", buildModel_variables_long_group = "group",
                 permutation_show_text = TRUE, permutation_iter = 10,
                 bootstrap_show_table = TRUE, bootstrap_iteration = 10)
  notes <- c(names(g$permutation$table$notes), names(g$bootstrap$table$notes))
  expect_false(any(c("edge_debug", "bootstrap_debug") %in% notes))
})

test_that("choosing the same group twice reports it and later sections still run", {
  g <- run_small("GroupTNA", buildModel_variables_long_group = "group",
                 compare_show_summary = TRUE, compare_group_i = "A", compare_group_j = "A",
                 compare_show_network_diff_plot = TRUE, indices_show_table = TRUE)
  expect_match(jtna_notices(g$compare), "two different groups")
  expect_true(g$networkDiff$plot$visible)
  expect_gt(nrow(jtna_table(g$indices$table)), 0L)
})

test_that("indices use the model's sessions and are labelled per session", {
  d <- jtna_small()
  shuffled <- d[sample(nrow(d)), ]
  r <- jtna_run("TNA", data = shuffled, buildModel_variables_long_action = "action",
                buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time",
                buildModel_threshold = 300, indices_show_table = TRUE, indices_table_show_all = TRUE)
  idx <- jtna_table(r$indices$table)
  prepared <- tna::prepare_data(shuffled, actor = "actor", time = "time", action = "action",
                                time_threshold = 300)
  # Same sessions as the model (threshold honoured), same labels in the same order.
  expect_equal(nrow(idx), nrow(r$modelCache$state$data))
  expect_identical(idx$actor, as.character(prepared$meta_data$actor))
  expect_identical(idx$session, as.integer(prepared$meta_data$.session_nr))
})

test_that("ISO-8601 time stamps keep their time of day", {
  d <- jtna_data()
  r <- jtna_run("TNA", data = d, buildModel_variables_long_action = "Action",
                buildModel_variables_long_actor = "Actor", buildModel_variables_long_time = "Time_Posix")
  unix <- jtna_run("TNA", data = d, buildModel_variables_long_action = "Action",
                   buildModel_variables_long_actor = "Actor", buildModel_variables_long_time = "Time_Unix")
  # Unix seconds and the ISO text describe the same instants.
  expect_equal(nrow(r$modelCache$state$data), nrow(unix$modelCache$state$data))
  expect_gt(nrow(r$modelCache$state$data), length(unique(d$Actor)))
})

test_that("an empty significant-only bootstrap table gets a note, not junk rows", {
  r <- run_small("TNA", bootstrap_show_table = TRUE, bootstrap_iteration = 10,
                 bootstrap_level = 0.0001, bootstrap_table_significant_only = TRUE)
  expect_equal(nrow(jtna_table(r$bootstrap$table)), 0L)
  expect_match(r$bootstrap$table$notes$empty$note, "No edges were significant")
})

test_that("GroupTNA honours lambda for attention models", {
  lam <- function(l) run_small("GroupTNA", buildModel_variables_long_group = "group",
                               buildModel_type = "attention",
                               buildModel_lambda = l)$modelCache$state[[1]]$weights
  expect_false(isTRUE(all.equal(lam(0.1), lam(5))))
})

test_that("grouped scaling follows tna: across groups, and tna reports what it cannot analyse", {
  g <- run_small("GroupTNA", buildModel_variables_long_group = "group", buildModel_scaling = "minmax",
                 bootstrap_show_table = TRUE, bootstrap_iteration = 10)
  prepared <- .jtna_prepare(jtna_small(), g$options)
  ref <- tna::group_model(prepared, group = factor(.jtna_session_values(prepared, "group")),
                          scaling = "minmax")
  expect_equal(lapply(g$modelCache$state, \(m) m$weights), lapply(ref, \(m) m$weights))
  expect_match(jtna_errors(g), "not supported for grouped models with globally scaled", all = FALSE)
})

# ---- LOW -----------------------------------------------------------------------

test_that("instructions show only until the required variables are set", {
  r <- jtna_run("TNA", data = jtna_small(), buildModel_variables_long_actor = "actor")
  expect_true(r$instructions$visible)
  expect_false(r$model$visible)
  ready <- run_small("TNA")
  expect_false(ready$instructions$visible)
  gate <- run_small("ClusterTNA")
  expect_true(gate$instructions$visible)
})

test_that("every analysis shows the module version in its title, before and after setup", {
  stamp <- sprintf("(JTNA %s)", utils::packageVersion("JTNA"))
  titles <- c(TNA = "Transition Network Analysis", GroupTNA = "Group Transition Network Analysis",
              ClusterTNA = "Cluster Transition Network Analysis")
  invisible(lapply(names(titles), \(analysis) {
    empty <- jtna_run(analysis, data = jtna_small(), buildModel_variables_long_actor = "actor")
    expect_identical(empty$title, paste(titles[[analysis]], stamp), label = paste(analysis, "before setup"))
    extra <- switch(analysis, TNA = list(), GroupTNA = list(buildModel_variables_long_group = "group"),
                    ClusterTNA = list(clustering_run = TRUE))
    ready <- do.call(run_small, c(list(analysis), extra))
    expect_identical(ready$title, paste(titles[[analysis]], stamp), label = paste(analysis, "after setup"))
  }))
  # A second stamp replaces the first instead of repeating it.
  r <- run_small("TNA")
  .jtna_stamp_version(r)
  expect_identical(r$title, paste(titles[["TNA"]], stamp))
})

test_that("a pattern length range with min above max is reported on the section", {
  r <- run_small("TNA", pattern_show_table = TRUE, pattern_len_min = 4, pattern_len_max = 2)
  expect_match(jtna_notices(r$patterns), "minimum length is larger")
})

test_that("index values are passed unrounded to the table", {
  r <- run_small("TNA", indices_show_table = TRUE)
  ent <- jtna_table(r$indices$table)$longitudinal_entropy
  expect_false(isTRUE(all.equal(ent, round(ent, 3))))
})

test_that("pattern and index tables hold every sequence once, in order", {
  r <- run_small("TNA", indices_show_table = TRUE, indices_table_show_all = TRUE)
  idx <- jtna_table(r$indices$table)
  expect_identical(idx$sequence_id, seq_len(nrow(idx)))
  expect_false(anyDuplicated(paste(idx$actor, idx$session)) > 0)
})

test_that("a prepared object without the needed metadata fails with a clear message", {
  # tna 1.2.3 (bundled by a stale jmvtools build) had no `.session_nr` in
  # meta_data; that surfaced as "differing number of rows: 2000, 0".
  prepared <- tna::prepare_data(jtna_small(), actor = "actor", action = "action")
  prepared$meta_data$.session_nr <- NULL
  expect_error(.jtna_sequence_labels(prepared, list(buildModel_variables_long_actor = "actor")),
               "needs tna 1.3.1 or later")
})

# ---- Sequence plots -------------------------------------------------------

test_that("index and distribution plots show together, with every setting applied", {
  settings <- list(
    sequences_show_index = TRUE, sequences_show_distribution = TRUE, sequences_sort = "all",
    sequences_scale = "count", sequences_geom = "area", sequences_include_na = TRUE,
    sequences_show_n = FALSE, sequences_tick = 2, sequences_index_title = "Index",
    sequences_index_ylab = "Sequence", sequences_distribution_title = "Distribution",
    sequences_distribution_ylab = "Events", sequences_xlab = "Step", sequences_legend_title = "Action",
    sequences_border = "grey40", sequences_na_color = "grey90",
    sequences_width = 800, sequences_height = 500)
  runs <- list(
    TNA = do.call(run_small, c(list("TNA"), settings)),
    GroupTNA = do.call(run_small, c(list("GroupTNA", buildModel_variables_long_group = "group",
                                         sequences_ncol = 1), settings)),
    ClusterTNA = do.call(run_small, c(list("ClusterTNA", clustering_run = TRUE,
                                           sequences_ncol = 1), settings)))
  lapply(runs, \(r) {
    lapply(list(r$sequences$index, r$sequences$distribution), \(image) {
      expect_true(image$visible)
      expect_identical(c(image$width, image$height), c(800, 500))
      expect_no_warning(render(image), class = "jtna_plot_failed")
    })
  })
})

test_that("each sequence plot can be shown on its own", {
  r <- run_small("TNA", sequences_show_distribution = TRUE)
  expect_false(r$sequences$index$visible)
  expect_true(r$sequences$distribution$visible)
})

test_that("sorting an index plot groups the sequences by state", {
  # Number of runs of identical states down the first time point: one per
  # state once sorted.
  runs_first <- function(model, sort) {
    p <- tna::plot_sequences(model, sort_by = .jtna_sort_by(model, sort))
    d <- as.data.frame(p$data)
    d <- d[d$time == levels(d$time)[1], ]
    d <- d[order(as.integer(as.character(d$.seq_id))), ]
    length(rle(as.character(d$state))$lengths)
  }
  model <- run_small("TNA")$modelCache$state
  n_states <- length(model$labels)
  expect_null(.jtna_sort_by(model, "none"))
  expect_equal(runs_first(model, "first"), n_states)
  expect_equal(runs_first(model, "all"), n_states)
  expect_gt(runs_first(model, "none"), n_states)
})

test_that("group networks carry their own group's name when the data start with a later group", {
  # tna 1.3.1 swaps the names for a character group vector whose first value
  # is not alphabetically first; JTNA passes a factor.
  d <- jtna_small()
  d$group <- ifelse(d$group == "A", "zeta", "alpha")  # data now start with "zeta"
  expect_identical(d$group[1], "zeta")
  g <- run_small("GroupTNA", data = d, buildModel_variables_long_group = "group")$modelCache$state
  expect_setequal(names(g), c("alpha", "zeta"))
  lapply(names(g), \(grp) {
    alone <- run_small("TNA", data = subset(d, group == grp))$modelCache$state
    expect_equal(unname(g[[grp]]$weights), unname(alone$weights), tolerance = sqrt(.Machine$double.eps),
                 label = grp)
  })
})

test_that("each cluster network is built from the sequences assigned to it", {
  d <- jtna_small(n_actor = 30)
  r <- run_small("ClusterTNA", data = d, clustering_run = TRUE, clustering_k = 3)
  model <- r$modelCache$state
  clusters <- attr(model, "jtna_clusters")
  prepared <- suppressMessages(tna::prepare_data(d, actor = "actor", action = "action"))
  lapply(unique(clusters), \(cl) {
    rows <- which(clusters == cl)
    seqs <- ref_sequences(prepared)[rows, , drop = FALSE]
    expected <- ref_relative(ref_counts(seqs, model[[cl]]$labels))
    expect_equal(unname(model[[cl]]$weights), unname(expected), tolerance = sqrt(.Machine$double.eps),
                 label = cl)
  })
})
