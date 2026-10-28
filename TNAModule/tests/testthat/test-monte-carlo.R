# Layer 3: invariants over many seeded random data sets. Set JTNA_MC_N to
# change the number of data sets (default 100).

skip_if_not_installed("tna")
skip_if_not_installed("pkgload")
skip_on_cran()

tol <- sqrt(.Machine$double.eps)
n_sets <- as.integer(Sys.getenv("JTNA_MC_N", "100"))
seeds <- seq_len(n_sets)

tna_of <- function(data, ...) {
  jtna_run("TNA", data = data, buildModel_variables_long_action = "action",
           buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time", ...)
}
weights_of <- function(data, ...) tna_of(data, ...)$modelCache$state$weights

test_that("relative models: rows sum to 1 (or 0), initial probabilities sum to 1, and both equal hand counts", {
  invisible(lapply(seeds, \(s) {
    d <- mc_data(s)
    model <- tna_of(d)$modelCache$state
    w <- model$weights
    rs <- rowSums(w)
    expect_true(all(abs(rs - 1) < 1e-9 | abs(rs) < 1e-12), label = sprintf("row sums, seed %d", s))
    expect_equal(sum(model$inits), 1, tolerance = tol, label = sprintf("inits, seed %d", s))
    seqs <- ref_sequences(suppressMessages(
      tna::prepare_data(d, actor = "actor", time = "time", action = "action")))
    expect_equal(unname(w), unname(ref_relative(ref_counts(seqs, model$labels))), tolerance = tol,
                 label = sprintf("weights vs counts, seed %d", s))
  }))
})

test_that("shuffling the input rows changes nothing", {
  invisible(lapply(seeds[seq_len(min(30L, n_sets))], \(s) {
    d <- mc_data(s)
    set.seed(s + 1e4)
    shuffled <- d[sample(nrow(d)), ]
    expect_equal(weights_of(shuffled), weights_of(d), tolerance = tol, label = sprintf("seed %d", s))
  }))
})

test_that("relabelling states permutes the model the same way", {
  invisible(lapply(seeds[seq_len(min(30L, n_sets))], \(s) {
    d <- mc_data(s)
    old <- sort(unique(d$action))
    new <- stats::setNames(sprintf("z%02d", rev(seq_along(old))), old)
    relabelled <- transform(d, action = unname(new[action]))
    w_old <- weights_of(d)
    w_new <- weights_of(relabelled)
    expect_equal(unname(w_new[new[rownames(w_old)], new[colnames(w_old)]]), unname(w_old),
                 tolerance = tol, label = sprintf("seed %d", s))
  }))
})

test_that("a shorter session gap never gives fewer sessions", {
  invisible(lapply(seeds[seq_len(min(30L, n_sets))], \(s) {
    d <- mc_data(s)
    sessions <- vapply(c(3600, 900, 120, 30), \(gap) {
      nrow(tna_of(d, buildModel_threshold = gap)$modelCache$state$data)
    }, integer(1))
    expect_true(all(diff(sessions) >= 0), label = sprintf("seed %d: %s", s, toString(sessions)))
  }))
})

test_that("scaled weights stay in their range", {
  invisible(lapply(seeds[seq_len(min(30L, n_sets))], \(s) {
    d <- mc_data(s)
    mm <- weights_of(d, buildModel_scaling = "minmax")
    mx <- weights_of(d, buildModel_scaling = "max")
    expect_true(all(mm >= -tol & mm <= 1 + tol), label = sprintf("minmax, seed %d", s))
    expect_equal(max(mx), 1, tolerance = tol, label = sprintf("max, seed %d", s))
  }))
})

test_that("each group's network equals a single model built on that group alone", {
  invisible(lapply(seeds[seq_len(min(30L, n_sets))], \(s) {
    d <- mc_data(s)
    g <- jtna_run("GroupTNA", data = d, buildModel_variables_long_action = "action",
                  buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time",
                  buildModel_variables_long_group = "group")$modelCache$state
    lapply(names(g), \(grp) {
      alone <- subset(d, group == grp)
      if (length(unique(alone$action)) < 2L) return(invisible())
      single <- tna_of(alone)$modelCache$state
      states <- intersect(single$labels, g[[grp]]$labels)
      expect_equal(unname(g[[grp]]$weights[states, states]), unname(single$weights[states, states]),
                   tolerance = tol, label = sprintf("seed %d, group %s", s, grp))
    })
  }))
})
