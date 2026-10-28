# Exercise the production classes and preparation helpers, never a copied wrapper.
skip_if_not_installed("tna")
skip_if_not_installed("codyna")

production_run <- function(data, analysis = "TNA", ...) {
  extra <- if (analysis == "GroupTNA") list(buildModel_variables_long_group = "group") else list()
  do.call(jtna_run, c(list(analysis, data = data,
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
    buildModel_variables_long_time = "time"), extra, list(...)))
}

test_that("production preparation agrees with independently supplied time representations", {
  data <- jtna_small(n_actor = 12)
  unix <- as.numeric(as.POSIXct(data$time, tz = "UTC"))
  variants <- list(text = data, posix = transform(data, time = as.POSIXct(unix, origin = "1970-01-01", tz = "UTC")),
                   unix = transform(data, time = unix))
  reference_data <- data
  reference_data$time <- unix
  reference <- tna::prepare_data(reference_data, actor = "actor", action = "action",
                                  time = "time", is_unix_time = TRUE, time_threshold = 300)
  invisible(lapply(variants, function(input) {
    actual <- production_run(input, buildModel_threshold = 300)
    prepared <- .jtna_prepare(input, actual$options)
    expect_equal(as.matrix(prepared$sequence_data), as.matrix(reference$sequence_data))
    expect_equal(prepared$meta_data$.session_nr, reference$meta_data$.session_nr)
    expect_equal(actual$modelCache$state$weights, tna::build_model(reference)$weights)
  }))
})

test_that("production model types and scalings agree with the direct workflow", {
  data <- jtna_small(n_actor = 12)
  prepared <- tna::prepare_data(data, actor = "actor", time = "time", action = "action")
  grid <- expand.grid(type = c("relative", "frequency", "attention"),
                      scaling = c("noScaling", "minmax", "max", "rank"), stringsAsFactors = FALSE)
  invisible(Map(function(type, scaling) {
    actual <- production_run(data, buildModel_type = type, buildModel_scaling = scaling)
    reference <- tna::build_model(prepared, type = type,
      scaling = if (scaling == "noScaling") character() else scaling)
    expect_equal(actual$modelCache$state$weights, reference$weights, info = paste(type, scaling))
    expect_equal(actual$modelCache$state$inits, reference$inits, info = paste(type, scaling))
  }, grid$type, grid$scaling))
})

test_that("production group labels and weights agree with separate group models", {
  data <- jtna_small(n_actor = 12)
  data$group <- ifelse(data$group == "A", "zeta", "alpha")
  actual <- production_run(data, "GroupTNA")$modelCache$state
  expect_identical(names(actual), c("alpha", "zeta"))
  invisible(lapply(names(actual), function(group) {
    input <- data[data$group == group, ]
    prepared <- tna::prepare_data(input, actor = "actor", time = "time", action = "action")
    reference <- tna::build_model(prepared)
    expect_equal(actual[[group]]$weights, reference$weights)
  }))
})
