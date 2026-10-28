test_that("missing order entries do not turn numeric positions into lexical order", {
  d <- data.frame(actor = "s1", action = c("a", "b", "c", "d"),
                  position = c("1", "2", "10", NA_character_))
  lapply(list(d$position, factor(d$position), c(1, 2, 10, NA_real_)), function(position) {
    d$position <- position
    opts <- TNAOptions$new(buildModel_variables_long_action = "action",
                           buildModel_variables_long_actor = "actor",
                           buildModel_variables_long_order = "position")
    p <- suppressMessages(.jtna_prepare(d, opts))
    expect_identical(unname(unlist(p$sequence_data)), c("a", "b", "c", "d"))
    expect_equal(tna::build_model(p)$weights["a", "b"], 1)
  })
})

test_that("POSIXct times keep absolute event order across the autumn DST change", {
  instants <- as.POSIXct(c("2026-10-25 00:50:00", "2026-10-25 01:10:00",
                           "2026-10-25 01:20:00", "2026-10-25 01:30:00"), tz = "UTC")
  attr(instants, "tzone") <- "Europe/Helsinki"
  d <- data.frame(actor = "s1", action = c("a", "b", "c", "d"), time = instants)
  opts <- TNAOptions$new(buildModel_variables_long_action = "action",
                         buildModel_variables_long_actor = "actor",
                         buildModel_variables_long_time = "time", buildModel_threshold = 900)
  p <- suppressMessages(.jtna_prepare(d, opts))
  d$time <- as.numeric(instants)
  reference <- suppressMessages(.jtna_prepare(d, opts))
  expect_equal(p$sequence_data, reference$sequence_data)
  expect_equal(p$time_data, reference$time_data)
  expect_equal(p$meta_data$.session_nr, c(1L, 2L))
  expect_identical(p$long_data$action, c("a", "b", "c", "d"))
})

test_that("action column punctuation preserves timed sequences and event labels", {
  d <- data.frame(actor = rep(c("a", "b"), each = 3),
                  time = rep(c(1000, 1060, 1120), 2),
                  action = c("A", "B", "C", "B", "A", "C"))
  reference <- suppressMessages(tna::prepare_data(d, actor = "actor", time = "time",
                                                  action = "action"))
  lapply(c("Action (coded)", "Action+", "Action[1]", "Action?", "A\\B", "Action.name"), function(column) {
    names(d)[names(d) == "action"] <- column
    opts <- TNAOptions$new(buildModel_variables_long_action = column,
                           buildModel_variables_long_actor = "actor",
                           buildModel_variables_long_time = "time")
    p <- suppressMessages(.jtna_prepare(d, opts))
    expect_identical(unname(as.matrix(p$sequence_data)), unname(as.matrix(reference$sequence_data)))
    expect_identical(p$long_data[[column]], reference$long_data$action)
    expect_equal(p$meta_data, reference$meta_data)
    expect_equal(tna::build_model(p)$weights, tna::build_model(reference)$weights)
  })
})

test_that("one column assigned to Time and Order is selected only once", {
  d <- data.frame(actor = rep(c("a", "b"), each = 3),
                  time = rep(c(1000, 1060, 1120), 2),
                  action = c("A", "B", "C", "B", "A", "C"))
  opts <- TNAOptions$new(buildModel_variables_long_action = "action",
                         buildModel_variables_long_actor = "actor",
                         buildModel_variables_long_time = "time",
                         buildModel_variables_long_order = "time")
  p <- suppressMessages(.jtna_prepare(d, opts))
  expect_false(anyDuplicated(names(p$long_data)) > 0L)
  expect_false("time.1" %in% names(p$meta_data))
  expect_identical(as.vector(as.matrix(p$sequence_data)), c("A", "B", "B", "A", "C", "C"))
})

test_that("actors nested in groups retain the existing sequence order", {
  d <- data.frame(actor = rep(c("s1", "s2", "s3"), each = 3),
                  action = rep(c("a", "b", "c"), 3),
                  group = rep(c("B", "A", "B"), each = 3))
  opts <- GroupTNAOptions$new(buildModel_variables_long_action = "action",
                              buildModel_variables_long_actor = "actor",
                              buildModel_variables_long_group = "group")
  p <- suppressMessages(.jtna_prepare(d, opts))
  expect_identical(p$meta_data$actor, c("s1", "s2", "s3"))
  expect_identical(p$meta_data$group, c("B", "A", "B"))
})
