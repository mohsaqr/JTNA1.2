# Regression test for the Order-column scrambling bug.
#
# Bug: TNA.b.R / GroupTNA.b.R forced the Order column to character before
# prepare_data. prepare_data then sorts events lexicographically ("10" < "2"),
# so any sequence longer than 9 events is scrambled and the transition model is
# wrong.
#
# Fix: keep Order numeric when it is fully numeric-coercible, so prepare_data
# sorts events numerically. These tests pin the conversion contract.

skip_if_not_installed("tna")

# Mirror of the fix used in the .b.R files.
coerce_order <- function(x) {
  num <- suppressWarnings(as.numeric(as.character(x)))
  if (!anyNA(num)) num else x
}

first_seq <- function(df, order_col) {
  pd <- suppressMessages(tna::prepare_data(
    df, actor = "User", action = "Code", order = order_col))
  as.character(unlist(pd$sequence_data[1, ]))[seq_len(12)]
}

test_that("numeric Order yields the correct event sequence; character scrambles it", {
  set.seed(3)
  df <- data.frame(User = rep("U1", 12), Order = 1:12,
                   Code = letters[1:12], stringsAsFactors = FALSE)
  df <- df[sample(nrow(df)), ]                       # shuffle rows so order matters

  df_num <- df; df_num$Order <- coerce_order(df_num$Order)   # the fix
  df_chr <- df; df_chr$Order <- as.character(df_chr$Order)   # the old behavior

  expect_equal(first_seq(df_num, "Order"), letters[1:12])    # fix => correct
  expect_false(identical(first_seq(df_chr, "Order"), letters[1:12]))  # old => wrong
})

test_that("coerce_order keeps numeric order numeric and leaves non-numeric untouched", {
  expect_type(coerce_order(c("1", "2", "10")), "double")     # numeric strings -> numeric
  expect_equal(coerce_order(c("1", "2", "10")), c(1, 2, 10))
  expect_identical(coerce_order(c("step1", "step2")),        # genuinely categorical -> unchanged
                   c("step1", "step2"))
})

test_that("clustering respects Order: unsorted file clusters correctly only with order", {
  # Two latent groups with opposite ordered patterns; file rows shuffled.
  set.seed(1)
  n <- 12; per <- 10
  mk <- function(a) {
    codes <- if (a <= 6) rep(c("plan", "do", "reflect"), length.out = per)
             else        rep(c("reflect", "do", "plan"), length.out = per)
    data.frame(User = sprintf("U%02d", a), Order = 1:per, Code = codes,
               stringsAsFactors = FALSE)
  }
  df <- do.call(rbind, lapply(1:n, mk))
  df <- df[sample(nrow(df)), ]

  prep <- function(use_order) {
    d <- df
    d$Order <- coerce_order(d$Order)
    args <- list(data = d[c("User", "Order", "Code")], actor = "User", action = "Code")
    if (use_order) args$order <- "Order"
    suppressMessages(do.call(tna::prepare_data, args))
  }
  set.seed(42); c_yes <- suppressMessages(tna::cluster_sequences(prep(TRUE)$sequence_data,  k = 2))
  asg <- function(c) if (!is.null(c$assignments)) c$assignments else c$clustering
  a <- asg(c_yes)
  # With order, the two true groups separate cleanly: actors 1-6 share a label,
  # 7-12 share the other.
  expect_equal(length(unique(a[1:6])), 1L)
  expect_equal(length(unique(a[7:12])), 1L)
  expect_false(a[1] == a[7])
})

test_that("the fix is order-of-magnitude safe (sequences > 9 events)", {
  set.seed(7)
  n <- 20
  df <- data.frame(User = rep("U1", n), Order = seq_len(n),
                   Code = sample(c("a", "b", "c"), n, replace = TRUE),
                   stringsAsFactors = FALSE)
  df <- df[sample(nrow(df)), ]
  df$Order <- coerce_order(df$Order)
  pd <- suppressMessages(tna::prepare_data(df, actor = "User", action = "Code", order = "Order"))
  got <- as.character(unlist(pd$sequence_data[1, ]))[seq_len(n)]
  # Reconstruct the expected order directly from the (numeric-sorted) data.
  expected <- df$Code[order(df$Order)]
  expect_equal(got, expected)
})
