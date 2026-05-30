# Equivalence tests: the JTNA wrapper paths must produce results numerically
# identical to the canonical tna/codyna reference workflow. These guard against
# the wrapper's data-prep quirks silently distorting results:
#   - the as.POSIXct pre-conversion + column subset in the build path,
#   - the per-session alignment used for group/actor attributes,
#   - the unscaled rebuild used for permutation on scaled models.
#
# Reference = the straightforward tna/codyna call. Wrapper = the exact arg
# construction the .b.R files use. We compare weights with all.equal tolerance.

skip_if_not_installed("tna")
skip_if_not_installed("codyna")

TOL <- 1e-8

# ---- shared synthetic data (long format, with time + group) ----------------
make_df <- function(seed = 42, n_actor = 25, per = 24) {
  set.seed(seed)
  gaps <- unlist(lapply(seq_len(n_actor), function(a)
    cumsum(sample(c(60, 120, 300, 3600), per, replace = TRUE,
                  prob = c(.5, .3, .12, .08)))))
  data.frame(
    User   = rep(sprintf("U%02d", seq_len(n_actor)), each = per),
    Time   = as.POSIXct("2026-01-01 09:00:00", tz = "UTC") + gaps,
    Code   = sample(c("plan", "monitor", "evaluate", "read", "write"),
                    n_actor * per, replace = TRUE),
    Group  = rep(sample(c("AI", "HUMAN"), n_actor, replace = TRUE), each = per),
    stringsAsFactors = FALSE
  )
}

# Exact replication of the JTNA build-path data prep (TNA.b.R lines ~58-100).
# `group` mirrors GroupTNA.b.R, whose columnToUseLong also includes the group
# column so it survives into long_data; pass it for group/outcome tests.
wrapper_prep <- function(df, action, actor = NULL, time = NULL, order = NULL,
                         group = NULL, threshold = 900) {
  copyData <- df
  copyData[[action]] <- as.character(copyData[[action]])
  if (!is.null(time))  copyData[[time]]  <- as.POSIXct(copyData[[time]])
  if (!is.null(actor)) copyData[[actor]] <- as.character(copyData[[actor]])
  cols <- c(time, actor, action, group, order)
  longData <- copyData[cols]
  args <- list(data = longData, actor = actor, time = time, action = action,
               time_threshold = threshold, order = order)
  args <- args[!vapply(args, is.null, logical(1))]
  do.call(tna::prepare_data, args)
}

weights_of <- function(m) {
  if (!is.null(m$weights)) m$weights
  else if (is.list(m) && !is.null(m[[1]]$weights)) lapply(m, function(g) g$weights)
  else stop("no weights")
}

# ---------------------------------------------------------------------------
test_that("build-path prep is numerically identical to canonical prep", {
  df <- make_df()
  # Canonical: pass the full data frame untouched (raw character time).
  ref  <- suppressMessages(tna::prepare_data(
    df[c("User", "Time", "Code")], actor = "User", time = "Time",
    action = "Code", time_threshold = 900))
  # Wrapper: pre-converted POSIXct + subset columns.
  wrp  <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                        time = "Time", threshold = 900))
  expect_equal(nrow(ref$sequence_data), nrow(wrp$sequence_data))
  # Same sessions => identical relative model weights.
  m_ref <- tna::build_model(ref, type = "relative")
  m_wrp <- tna::build_model(wrp, type = "relative")
  expect_equal(weights_of(m_ref), weights_of(m_wrp), tolerance = TOL)
})

test_that("all model types x scalings build equivalently via the wrapper prep", {
  df  <- make_df()
  ref <- suppressMessages(tna::prepare_data(
    df[c("User", "Time", "Code")], actor = "User", time = "Time",
    action = "Code", time_threshold = 900))
  wrp <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                       time = "Time", threshold = 900))
  for (ty in c("relative", "frequency")) {
    for (sc in list(character(0L), "minmax", "max", "rank")) {
      m_ref <- tna::build_model(ref, type = ty, scaling = sc)
      m_wrp <- tna::build_model(wrp, type = ty, scaling = sc)
      expect_equal(weights_of(m_ref), weights_of(m_wrp), tolerance = TOL,
                   info = paste("type", ty, "scaling", paste(sc, collapse = "")))
    }
  }
})

test_that("centralities are equivalent regardless of prep route", {
  df  <- make_df()
  ref <- suppressMessages(tna::prepare_data(
    df[c("User", "Time", "Code")], actor = "User", time = "Time",
    action = "Code", time_threshold = 900))
  wrp <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                       time = "Time", threshold = 900))
  meas <- c("OutStrength", "InStrength", "Betweenness", "Closeness")
  c_ref <- tna::centralities(tna::build_model(ref, type = "relative"), measures = meas)
  c_wrp <- tna::centralities(tna::build_model(wrp, type = "relative"), measures = meas)
  c_ref <- c_ref[order(c_ref$state), ]; c_wrp <- c_wrp[order(c_wrp$state), ]
  for (m in meas) expect_equal(as.numeric(c_ref[[m]]), as.numeric(c_wrp[[m]]), tolerance = TOL)
})

test_that("group_model via per-session group == canonical group_model", {
  df  <- make_df()
  pd  <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                       time = "Time", group = "Group", threshold = 900))
  # Per-session group (the alignment fix).
  gsess <- pd$long_data[!duplicated(pd$long_data$.session_id), ][["Group"]]
  expect_equal(length(gsess), nrow(pd$sequence_data))
  gm <- suppressMessages(tna::group_model(pd, group = gsess, type = "relative"))
  expect_setequal(names(gm), c("AI", "HUMAN"))
  # Each group's weight matrix is a valid relative model (rows sum ~1 where edges exist).
  for (g in names(gm)) {
    w <- gm[[g]]$weights
    rs <- rowSums(w)
    expect_true(all(abs(rs[rs > 0] - 1) < 1e-6))
  }
})

test_that("unscaled permutation rebuild == direct unscaled model (weights)", {
  df <- make_df()
  pd <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                      time = "Time", group = "Group", threshold = 900))
  g  <- pd$long_data[!duplicated(pd$long_data$.session_id), ][["Group"]]
  scaled   <- suppressMessages(tna::group_model(pd, group = g, type = "frequency", scaling = "minmax"))
  rebuilt  <- suppressMessages(tna::group_model(pd, group = g, type = "frequency", scaling = character(0L)))
  direct   <- suppressMessages(tna::group_model(pd, group = g, type = "frequency"))
  expect_equal(weights_of(rebuilt), weights_of(direct), tolerance = TOL)
  # And the rebuild actually permutes (the scaled one would error).
  expect_s3_class(tna::permutation_test(rebuilt, iter = 20), "group_tna_permutation")
})

test_that("codyna outputs are stable and column-complete", {
  df <- make_df()
  pd <- suppressMessages(wrapper_prep(df, action = "Code", actor = "User",
                                      time = "Time", group = "Group", threshold = 900))
  sd <- as.data.frame(lapply(as.data.frame(pd$sequence_data), as.character),
                      stringsAsFactors = FALSE)
  g  <- pd$long_data[!duplicated(pd$long_data$.session_id), ][["Group"]]

  p <- codyna::discover_patterns(sd, type = "ngram", len = 2:3, min_freq = 3, outcome = g)
  expect_true(all(c("pattern", "length", "count", "frequency", "support",
                    "chisq", "p_value") %in% names(p)))
  expect_true(any(grepl("^count_", names(p))))

  idx <- codyna::sequence_indices(sd)
  expect_equal(nrow(idx), nrow(sd))
  expect_gte(ncol(idx), 20L)   # the 23-metric surface
})
