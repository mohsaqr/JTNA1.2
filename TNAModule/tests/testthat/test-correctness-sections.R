# Regression tests for the independent correctness review (1.9.4).
skip_if_not_installed("tna")
skip_if_not_installed("pkgload")

section_run <- function(analysis = "TNA", data = jtna_small(), ...) {
  extra <- switch(analysis, TNA = list(),
    GroupTNA = list(buildModel_variables_long_group = "group"),
    ClusterTNA = list(clustering_run = TRUE, clustering_k = 2))
  do.call(jtna_run, c(list(analysis, data = data,
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor"),
    extra, list(...)))
}

test_that("cosmetic changes restore numerical caches without consuming RNG", {
  original <- section_run("GroupTNA", bootstrap_show_table = TRUE, bootstrap_iteration = 20,
    permutation_show_text = TRUE, permutation_iter = 20,
    community_show_table = TRUE, community_methods = "label_prop")
  cases <- list(
    bootstrap = list(cosmetic = "bootstrap_plot_edge_label_size", numerical = "bootstrap_iteration",
      run = .jtna_run_bootstrap),
    permutation = list(cosmetic = "permutation_plot_layout", numerical = "permutation_iter",
      run = .jtna_run_permutation),
    community = list(cosmetic = "community_plot_layout", numerical = "community_gamma",
      run = .jtna_run_community))
  invisible(lapply(names(cases), function(name) {
    config <- cases[[name]]
    section <- original$get(name)
    expected <- section$cache$state
    expect_false(is.null(expected), info = name)
    # Use the real jmvcore restoration methods, with their serialized state
    # fields. This does not require RProtoBuf or pretend to round-trip an .omv.
    pb <- list(state = serialize(expected, NULL), stale = FALSE, preformatted = "",
               image = list(width = 600, height = 600, path = ""))
    restored <- GroupTNAResults$new(options = original$options)$get(name)
    restored$cache$fromProtoBuf(pb, oChanges = config$cosmetic, vChanges = character())
    restored$plot$fromProtoBuf(pb, oChanges = config$cosmetic, vChanges = character())
    expect_identical(restored$cache$state, expected, info = name)
    expect_null(restored$plot$state)
    rng <- .Random.seed
    config$run(restored, original$options, original$modelCache$state, "group")
    expect_identical(.Random.seed, rng, info = name)
    expect_identical(restored$plot$state, expected, info = name)
    expect_equal(restored$table$asDF, section$table$asDF, info = name)

    invalidated <- GroupTNAResults$new(options = original$options)$get(name)
    invalidated$cache$fromProtoBuf(pb, oChanges = config$numerical, vChanges = character())
    expect_null(invalidated$cache$state)
    invalidated$cache$fromProtoBuf(pb, oChanges = character(), vChanges = "action")
    expect_null(invalidated$cache$state)
  }))
})

test_that("scaled grouped centralities retain exact model labels and values", {
  invisible(lapply(c("GroupTNA", "ClusterTNA"), function(analysis) {
    r <- section_run(analysis, buildModel_scaling = "max", centrality_show_table = TRUE,
                     centrality_show_plot = TRUE)
    model <- r$modelCache$state
    got <- r$centrality$table$asDF
    expect_identical(got$state, rep(model[[1]]$labels, length(model)))
    expect_identical(as.character(r$centrality$table$state$state), got$state)
    reference <- unlist(lapply(model, function(m) {
      w <- m$weights
      diag(w) <- 0
      rowSums(w)
    }), use.names = FALSE)
    expect_equal(got$OutStrength, reference)
    # The fix must not change the model or its scaling contract.
    expect_null(dimnames(model[[1]]$weights))
  }))
})

test_that("a blank group retains its matrix and centrality identity", {
  d <- jtna_small()
  d$group[d$group == "A"] <- ""
  r <- section_run("GroupTNA", data = d, buildModel_show_matrix = TRUE,
                   centrality_show_table = TRUE, compare_show_summary = TRUE,
                   compare_show_network = TRUE, compare_group_i = "", compare_group_j = "B")
  model <- r$modelCache$state
  invisible(lapply(seq_along(model), function(k) {
    got <- r$model$matrices$get(key = names(model)[k])$asDF
    expect_equal(nrow(got), length(model[[k]]$labels))
    expect_equal(unname(as.matrix(got[-1])), unname(model[[k]]$weights))
  }))
  groups <- rep(names(model), vapply(model, function(m) length(m$labels), integer(1)))
  expect_identical(r$centrality$table$asDF$group, .jtna_display_labels(groups))
  expect_identical(r$centrality$table$state$group, groups)
  expect_identical(r$compare$network$getColumn("net_i")$title, '""')
  expect_identical(r$compare$network$getColumn("net_j")$title, '"B"')
  expect_length(jtna_errors(r), 0L)
})

test_that("grouped clique drawing renders every computed clique", {
  r <- section_run("GroupTNA", data = jtna_small(n_actor = 24, per = 100),
                   cliques_show_plot = TRUE, cliques_threshold = 0.01)
  cliques <- r$cliques$plot$state
  counts <- vapply(cliques, function(x) length(x$weights), integer(1))
  expect_true(all(counts > 6L))
  calls <- list()
  testthat::local_mocked_bindings(splot = function(x, ...) {
    calls[[length(calls) + 1L]] <<- list(weights = x, args = list(...))
    invisible(NULL)
  }, .package = "cograph")
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  .jtna_draw_cliques(cliques, r$options)
  expect_length(calls, sum(counts))
  expect_identical(vapply(calls, function(x) x$args$title, character(1)),
                   rep(names(cliques), counts))
})

test_that("threshold bootstrap uses the upstream default unless explicitly overridden", {
  d <- jtna_small(n_actor = 24, per = 18, actions = letters[1:4])
  run <- function(...) section_run(data = d, bootstrap_show_table = TRUE,
    bootstrap_method = "threshold", bootstrap_iteration = 100,
    random_seed_use = TRUE, random_seed = 11, ...)
  r <- run()
  reference <- .jtna_with_seed(11, function() {
    tna::bootstrap(r$modelCache$state, method = "threshold", iter = 100)
  })
  expect_equal(r$bootstrap$plot$state, reference)
  manual <- run(bootstrap_threshold_use = TRUE, bootstrap_threshold = 0.1)
  manual_reference <- .jtna_with_seed(11, function() {
    tna::bootstrap(r$modelCache$state, method = "threshold", iter = 100, threshold = 0.1)
  })
  expect_equal(manual$bootstrap$plot$state, manual_reference)
  expect_false(identical(manual$bootstrap$plot$state$p_values, reference$p_values))
})

test_that("globally scaled permutations report tna's refusal for both grouped analyses", {
  invisible(lapply(c("GroupTNA", "ClusterTNA"), function(analysis) {
    r <- section_run(analysis, buildModel_scaling = "max", permutation_show_text = TRUE,
                     permutation_iter = 10)
    expect_match(jtna_errors(r), "not supported for grouped models with globally scaled")
    expect_null(r$permutation$cache$state)
    expect_null(r$permutation$plot$state)
  }))
})
