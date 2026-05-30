# Regression test for the "scaled freq tna doesn't permute" bug.
#
# Bug: GroupTNA ran permutation_test on the displayed (globally scaled) group
# model. tna refuses this — "Permutation test is not supported for grouped
# models with globally scaled edge weights." — so any non-default Scaling broke
# the whole permutation section.
#
# Fix: scaling is a display transform; the permutation test runs on an UNSCALED
# rebuild of the model (same type / lambda). This pins that contract.

skip_if_not_installed("tna")

make_pd <- function(seed = 1, n_actor = 20, per = 15) {
  set.seed(seed)
  df <- data.frame(
    actor  = rep(sprintf("S%02d", seq_len(n_actor)), each = per),
    action = sample(c("plan", "monitor", "evaluate", "read"),
                    n_actor * per, replace = TRUE),
    group  = rep(sample(c("A", "B"), n_actor, replace = TRUE), each = per),
    stringsAsFactors = FALSE
  )
  suppressMessages(tna::prepare_data(df, actor = "actor", action = "action"))
}

group_vec <- function(pd) {
  pd$long_data[!duplicated(pd$long_data$.session_id), ][["group"]]
}

test_that("permuting a globally scaled grouped model is rejected by tna (the bug)", {
  pd <- make_pd(); g <- group_vec(pd)
  for (sc in c("minmax", "max", "rank")) {
    scaled <- suppressMessages(tna::group_model(pd, group = g, type = "frequency", scaling = sc))
    expect_error(tna::permutation_test(scaled, iter = 20), "scaled")
  }
})

test_that("an unscaled rebuild of the same type permutes successfully (the fix)", {
  pd <- make_pd(); g <- group_vec(pd)
  for (ty in c("frequency", "relative")) {
    unscaled <- suppressMessages(tna::group_model(pd, group = g, type = ty, scaling = character(0L)))
    pt <- tna::permutation_test(unscaled, iter = 20)
    expect_s3_class(pt, "group_tna_permutation")
  }
})

test_that("unscaled rebuild matches the structure of the scaled model", {
  pd <- make_pd(); g <- group_vec(pd)
  scaled   <- suppressMessages(tna::group_model(pd, group = g, type = "frequency", scaling = "minmax"))
  unscaled <- suppressMessages(tna::group_model(pd, group = g, type = "frequency", scaling = character(0L)))
  # Same groups, same node set — only the weight scale differs.
  expect_equal(length(scaled), length(unscaled))
  expect_equal(dim(scaled[[1]]$weights), dim(unscaled[[1]]$weights))
})
