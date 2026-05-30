# Regression test for the attention "takes forever" bug.
#
# Bug: build_model(type="attention") scales ~O(L^2) with sequence length.
# Setting Actor to a coarse grouping variable with no Time column collapses the
# data into a few very long sequences; the attention build then takes minutes
# and the jamovi UI hangs (synchronous .run, so a pre-set warning won't render
# until after the build).
#
# Fix: before building an attention model, check the longest sequence length
# (ncol of sequence_data) and, above a limit, show an actionable message and
# skip the build instead of hanging. These tests pin the trigger and the guard
# decision (the .b.R R6 classes need the jamovi runtime, so we test the logic).

skip_if_not_installed("tna")

ATTENTION_SEQ_LIMIT <- 2000  # must match the constant in TNA.b.R / GroupTNA.b.R

# Mirror of the guard decision used in the .b.R files.
attention_would_hang <- function(tna_obj, type, limit = ATTENTION_SEQ_LIMIT) {
  if (!identical(type, "attention")) return(FALSE)
  if (!inherits(tna_obj, "tna_data") || is.null(tna_obj$sequence_data)) return(FALSE)
  ncol(tna_obj$sequence_data) > limit
}

make_pd <- function(n_groups, total_rows, n_codes = 12) {
  per <- total_rows %/% n_groups
  df <- data.frame(
    Group = rep(sprintf("G%d", seq_len(n_groups)), each = per),
    Code  = sample(paste0("c", seq_len(n_codes)), n_groups * per, replace = TRUE),
    stringsAsFactors = FALSE
  )
  suppressMessages(tna::prepare_data(df, actor = "Group", action = "Code"))
}

test_that("coarse Actor + no Time produces very long sequences", {
  set.seed(1)
  pd <- make_pd(n_groups = 2, total_rows = 10000)   # 2 sequences of ~5000
  expect_gt(ncol(pd$sequence_data), ATTENTION_SEQ_LIMIT)
})

test_that("guard blocks attention on long sequences but allows relative/frequency", {
  set.seed(1)
  pd <- make_pd(n_groups = 2, total_rows = 10000)
  expect_true(attention_would_hang(pd, "attention"))
  expect_false(attention_would_hang(pd, "relative"))
  expect_false(attention_would_hang(pd, "frequency"))
})

test_that("guard allows attention on reasonably short sequences", {
  set.seed(1)
  pd <- make_pd(n_groups = 50, total_rows = 5000)   # 50 sequences of ~100
  expect_lt(ncol(pd$sequence_data), ATTENTION_SEQ_LIMIT)
  expect_false(attention_would_hang(pd, "attention"))
  # And confirm such a build is actually fast (sanity, not a hang).
  t <- system.time(tna::build_model(pd, type = "attention", params = list(lambda = 1)))["elapsed"]
  expect_lt(t, 5)
})
