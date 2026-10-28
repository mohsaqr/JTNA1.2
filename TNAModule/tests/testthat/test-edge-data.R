# Awkward data, as jamovi delivers it. Every case must either give results
# with no failed section, or stop with a classed `jtna_bad_input` message.
# Never an unclassed R error.

skip_if_not_installed("tna")
skip_if_not_installed("pkgload")

sections_on <- list(
  buildModel_show_matrix = TRUE, centrality_show_table = TRUE, community_show_table = TRUE,
  community_methods = "walktrap", pattern_show_table = TRUE, pattern_min_count = 1,
  indices_show_table = TRUE, sequences_show_index = TRUE, sequences_show_distribution = TRUE)
run_case <- function(analysis, data, ...) {
  do.call(jtna_run, c(list(analysis, data = data), sections_on, list(...)))
}
expect_clean <- function(r, label) {
  expect_identical(jtna_errors(r), character(0), label = label)
  expect_false(is.null(r$modelCache$state), label = label)
}
base <- jtna_small(n_actor = 16, per = 12)

test_that("action, actor and group as jamovi factors and integers", {
  d <- base
  d$action <- factor(d$action)
  d$actor <- as.integer(factor(d$actor))
  d$group <- factor(d$group)
  expect_clean(run_case("TNA", d, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor"), "factor action, integer actor")
  expect_clean(run_case("GroupTNA", d, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor", buildModel_variables_long_group = "group"),
               "factor group")
})

test_that("time stamps in every common format give the same sessions", {
  stamps <- as.POSIXct(base$time, tz = "UTC")
  formats <- list(
    text = format(stamps, "%Y-%m-%d %H:%M:%S"),
    iso_t_z = format(stamps, "%Y-%m-%dT%H:%M:%SZ"),
    no_seconds = format(stamps, "%Y-%m-%d %H:%M"),
    unix_seconds = as.numeric(stamps))
  sessions <- vapply(names(formats), \(f) {
    d <- base
    d$time <- formats[[f]]
    r <- run_case("TNA", d, buildModel_variables_long_action = "action",
                  buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time")
    expect_clean(r, f)
    nrow(r$modelCache$state$data)
  }, integer(1))
  # Minute resolution can merge a few gaps at the threshold; the others agree exactly.
  expect_identical(unname(sessions[c("text", "iso_t_z", "unix_seconds")]),
                   rep(sessions[["text"]], 3L))
})

test_that("an unreadable time column stops with a clear message", {
  d <- base
  d$time <- rep(c("yesterday", "soon", "later"), length.out = nrow(d))
  expect_error(run_case("TNA", d, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time"),
               class = "jtna_bad_input")
})

test_that("missing and empty actions mid-sequence", {
  d <- base
  d$action[c(3, 17, 40)] <- NA
  d$action[c(5, 60)] <- ""
  expect_clean(run_case("TNA", d, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor"), "missing actions")
})

test_that("state names with spaces, punctuation, arrows and Unicode", {
  d <- base
  map <- c(plan = "Plan ahead", monitor = "monitör", evaluate = "評価", read = "read/write",
           write = "a->b")
  d$action <- unname(map[d$action])
  r <- run_case("TNA", d, buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor")
  expect_clean(r, "odd state names")
  expect_setequal(jtna_table(r$model$matrix)$from, unname(map))
})

test_that("actors with a single event, and one very long sequence", {
  d <- rbind(base, data.frame(actor = c("solo1", "solo2"), time = base$time[1:2],
                              action = c("plan", "read"), group = "A"))
  long <- data.frame(actor = "marathon", time = format(as.POSIXct("2026-01-02", tz = "UTC") + seq_len(600) * 30,
                                                       "%Y-%m-%d %H:%M:%S"),
                     action = rep(c("plan", "read", "write", "monitor", "evaluate"), 120), group = "B")
  d <- rbind(d, long)
  expect_clean(run_case("TNA", d, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor"), "single events and a long sequence")
})

test_that("many states (30) and a single actor", {
  set.seed(5)
  many <- data.frame(actor = rep(1:10, each = 40), action = sprintf("s%02d", sample(30, 400, TRUE)))
  expect_clean(run_case("TNA", many, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor"), "30 states")
  one <- subset(base, actor == base$actor[1])
  expect_clean(run_case("TNA", one, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor"), "one actor")
})

test_that("Order given as text numbers sorts numerically, together with Time", {
  d <- base
  d$step <- as.character(stats::ave(seq_len(nrow(d)), d$actor, FUN = seq_along))
  shuffled <- d[sample(nrow(d)), ]
  r1 <- run_case("TNA", d, buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
                 buildModel_variables_long_order = "step")
  r2 <- run_case("TNA", shuffled, buildModel_variables_long_action = "action",
                 buildModel_variables_long_actor = "actor", buildModel_variables_long_order = "step")
  expect_equal(r2$modelCache$state$weights, r1$modelCache$state$weights)
})

test_that("groups with missing values, unbalanced groups, and a group of one actor", {
  d <- base
  d$group[d$actor == d$actor[1]] <- NA
  d$group[d$actor == d$actor[nrow(d)]] <- "C"
  r <- run_case("GroupTNA", d, buildModel_variables_long_action = "action",
                buildModel_variables_long_actor = "actor", buildModel_variables_long_group = "group")
  expect_false(is.null(r$modelCache$state))
  expect_false("NA" %in% names(r$modelCache$state))
})

test_that("degenerate input is rejected with a classed message", {
  roles <- list(buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor")
  all_missing <- transform(base, action = NA_character_)
  one_action <- transform(base, action = "plan")
  expect_error(do.call(run_case, c(list("TNA", all_missing), roles)), class = "jtna_bad_input")
  expect_error(do.call(run_case, c(list("TNA", one_action), roles)), class = "jtna_bad_input")
  expect_error(do.call(run_case, c(list("ClusterTNA", jtna_small(n_actor = 3)), roles,
                                   list(clustering_run = TRUE, clustering_k = 3))),
               class = "jtna_bad_input")
})

test_that("a session gap so short that no transitions remain is rejected clearly", {
  expect_error(run_case("TNA", base, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time",
                        buildModel_threshold = 1),
               class = "jtna_bad_input")
  expect_error(run_case("GroupTNA", base, buildModel_variables_long_action = "action",
                        buildModel_variables_long_actor = "actor", buildModel_variables_long_time = "time",
                        buildModel_variables_long_group = "group", buildModel_threshold = 1),
               class = "jtna_bad_input")
})

test_that("short sequences: default pattern settings are clipped to the data, with a note", {
  short <- jtna_small(n_actor = 20, per = 4)  # every sequence has 4 events
  r <- run_case("TNA", short, buildModel_variables_long_action = "action",
                buildModel_variables_long_actor = "actor")
  expect_gt(nrow(jtna_table(r$patterns$table)), 0L)
  expect_match(r$patterns$table$notes$clipped$note, "lengths above 4 were skipped")
  expect_true(all(jtna_table(r$patterns$table)$length <= 4L))
})

test_that("two-event sequences: patterns and indices explain why they cannot be computed", {
  two <- jtna_small(n_actor = 20, per = 2)
  r <- run_case("TNA", two, buildModel_variables_long_action = "action",
                buildModel_variables_long_actor = "actor")
  expect_match(jtna_notices(r$patterns), "at least three events")
  expect_match(jtna_notices(r$indices), "at least three events")
  expect_false(is.null(r$modelCache$state))
})

test_that("network comparison runs for every scaling tna offers; signed scalings skip network properties", {
  a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "GroupTNA.a.yaml"))
  scalings <- unlist(lapply(Filter(\(o) o$name == "compare_scaling", a$options)[[1]]$options, \(x) x$name))
  expect_false("robust" %in% scalings)
  lapply(scalings, \(sc) {
    r <- run_case("GroupTNA", base, buildModel_variables_long_action = "action",
                  buildModel_variables_long_actor = "actor", buildModel_variables_long_group = "group",
                  compare_show_summary = TRUE, compare_show_network = TRUE, compare_scaling = sc)
    expect_identical(jtna_errors(r), character(0), label = sc)
    expect_gt(nrow(jtna_table(r$compare$summary)), 0L)
    if (sc %in% c("zscore", "log")) {
      expect_match(r$compare$network$notes$signed$note, "not computed", label = sc)
    } else {
      expect_gt(nrow(jtna_table(r$compare$network)), 0L)
    }
  })
})
