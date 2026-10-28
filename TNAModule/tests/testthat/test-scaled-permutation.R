# Inference must use the displayed model; preserve tna's global-scaling refusal.
skip_if_not_installed("tna")

test_that("every global scaling is refused without silently rebuilding a different model", {
  data <- jtna_small(n_actor = 12)
  grid <- expand.grid(analysis = c("GroupTNA", "ClusterTNA"),
    type = c("frequency", "relative", "attention"), scaling = c("minmax", "max", "rank"),
    stringsAsFactors = FALSE)
  invisible(Map(function(analysis, type, scaling) {
    extra <- if (analysis == "GroupTNA") list(buildModel_variables_long_group = "group") else
      list(clustering_run = TRUE, clustering_k = 2)
    result <- do.call(jtna_run, c(list(analysis, data = data,
      buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
      buildModel_type = type, buildModel_scaling = scaling,
      permutation_show_text = TRUE, permutation_iter = 10, indices_show_table = TRUE), extra))
    expect_error(tna::permutation_test(result$modelCache$state, iter = 10), "globally scaled")
    expect_match(jtna_notices(result$permutation), "globally scaled")
    expect_null(result$permutation$plot$state)
    expect_null(result$permutation$cache$state)
    expect_gt(nrow(result$indices$table$asDF), 0L)
  }, grid$analysis, grid$type, grid$scaling))
})
