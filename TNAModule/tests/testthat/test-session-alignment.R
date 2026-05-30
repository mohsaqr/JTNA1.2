# Regression tests for the per-session vs per-actor alignment bug.
#
# Bug: GroupTNA/ClusterTNA derived the group outcome and the actor/group
# columns by mapping over unique actors and guarding with
# `length(unique(actor)) == nrow(seq_data)`. A time threshold splits each
# actor into multiple sessions, so seq_data/indices has one row per SESSION,
# the guard fails, and the group breakdown (codyna `count_*`, chisq, p_value)
# plus the actor/group index columns silently disappear.
#
# Fix: derive these per session from `long_data[!duplicated(.session_id), ]`.
# These tests pin that contract against tna + codyna directly (the .b.R R6
# classes need the jamovi runtime, so we test the alignment logic itself).

skip_if_not_installed("tna")
skip_if_not_installed("codyna")

# Mirror of the fix used in the .b.R files.
session_attr <- function(tna_obj, col, n_expect) {
  if (!inherits(tna_obj, "tna_data") || is.null(tna_obj$long_data)) return(NULL)
  ld <- tna_obj$long_data
  if (!all(c(".session_id", col) %in% names(ld))) return(NULL)
  sess <- ld[!duplicated(ld$.session_id), , drop = FALSE]
  if (nrow(sess) != n_expect) return(NULL)
  as.character(sess[[col]])
}

make_data <- function(seed = 1, n_actor = 30, per = 20) {
  set.seed(seed)
  gaps <- unlist(lapply(seq_len(n_actor), function(a)
    cumsum(sample(c(60, 120, 300, 3600), per, replace = TRUE,
                  prob = c(.5, .3, .15, .05)))))
  data.frame(
    actor  = rep(sprintf("S%02d", seq_len(n_actor)), each = per),
    time   = as.POSIXct("2026-01-01 09:00:00", tz = "UTC") + gaps,
    action = sample(c("plan", "monitor", "evaluate", "read", "write", "discuss"),
                    n_actor * per, replace = TRUE),
    group  = rep(sample(c("AI", "HUMAN", "HYBRID"), n_actor, replace = TRUE),
                 each = per),
    stringsAsFactors = FALSE
  )
}

test_that("a time threshold splits actors into more sessions (the trigger)", {
  df <- make_data()
  pd <- suppressMessages(tna::prepare_data(
    df, actor = "actor", time = "time", action = "action", time_threshold = 900))
  n_sessions <- nrow(pd$sequence_data)
  n_actors   <- length(unique(df$actor))
  expect_gt(n_sessions, n_actors)               # sessions > actors
  # The OLD guard would fail here, dropping the group breakdown:
  expect_false(n_actors == n_sessions)
})

test_that("per-session outcome aligns to seq_data and restores group columns", {
  df <- make_data()
  pd <- suppressMessages(tna::prepare_data(
    df, actor = "actor", time = "time", action = "action", time_threshold = 900))
  seq_data <- as.data.frame(lapply(as.data.frame(pd$sequence_data),
                                   as.character), stringsAsFactors = FALSE)

  outcome <- session_attr(pd, "group", nrow(seq_data))
  expect_false(is.null(outcome))                       # fix produces a vector
  expect_equal(length(outcome), nrow(seq_data))        # aligned to sessions
  expect_setequal(unique(outcome), c("AI", "HUMAN", "HYBRID"))

  p_no  <- codyna::discover_patterns(seq_data, type = "ngram", len = 2:3, min_freq = 3)
  p_yes <- codyna::discover_patterns(seq_data, type = "ngram", len = 2:3, min_freq = 3,
                                     outcome = outcome)
  # Without outcome there are no group/test columns; with it, they appear.
  expect_false(any(c("chisq", "p_value") %in% names(p_no)))
  expect_true(all(c("chisq", "p_value") %in% names(p_yes)))
  expect_true(any(grepl("^count_", names(p_yes))))
})

test_that("sequence_indices actor/group align per session, not per actor", {
  df <- make_data()
  pd <- suppressMessages(tna::prepare_data(
    df, actor = "actor", time = "time", action = "action", time_threshold = 900))
  seq_data <- as.data.frame(lapply(as.data.frame(pd$sequence_data),
                                   as.character), stringsAsFactors = FALSE)
  idx <- codyna::sequence_indices(seq_data)

  actor_vec <- session_attr(pd, "actor", nrow(idx))
  group_vec <- session_attr(pd, "group", nrow(idx))
  expect_false(is.null(actor_vec))
  expect_false(is.null(group_vec))
  expect_equal(length(actor_vec), nrow(idx))
  expect_equal(length(group_vec), nrow(idx))
  # The OLD code returned NA here because length(unique(actor)) != nrow(idx).
  expect_false(anyNA(actor_vec))
})

test_that("with no time column, sessions == actors and alignment still holds", {
  df <- make_data()
  df$time <- NULL
  pd <- suppressMessages(tna::prepare_data(df, actor = "actor", action = "action"))
  seq_data <- as.data.frame(lapply(as.data.frame(pd$sequence_data),
                                   as.character), stringsAsFactors = FALSE)
  outcome <- session_attr(pd, "group", nrow(seq_data))
  expect_false(is.null(outcome))
  expect_equal(length(outcome), nrow(seq_data))
})

test_that("time_threshold changes session count (ClusterTNA prep must pass it)", {
  set.seed(1)
  n <- 10; per <- 20
  gaps <- unlist(lapply(seq_len(n), function(a)
    cumsum(sample(c(60, 600, 1800), per, replace = TRUE))))
  df <- data.frame(
    User = rep(sprintf("U%02d", seq_len(n)), each = per),
    Time = as.POSIXct("2026-01-01 09:00:00", tz = "UTC") + gaps,
    Code = sample(c("a", "b", "c"), n * per, replace = TRUE),
    stringsAsFactors = FALSE)
  s_default <- nrow(suppressMessages(tna::prepare_data(
    df, actor = "User", time = "Time", action = "Code", time_threshold = 900))$sequence_data)
  s_tight   <- nrow(suppressMessages(tna::prepare_data(
    df, actor = "User", time = "Time", action = "Code", time_threshold = 120))$sequence_data)
  # A tighter threshold must split more sessions; if ClusterTNA ignores the
  # option both would equal the 900s default.
  expect_gt(s_tight, s_default)
})
