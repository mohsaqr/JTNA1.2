test_that("weighted Hamming controls are unavailable for other distances", {
  ui <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", "ClusterTNA.u.yaml"))
  controls <- list()
  collect <- function(node) {
    if (!is.null(node$name)) controls[[node$name]] <<- node
    invisible(lapply(node$children, collect))
  }
  collect(ui)
  expect_identical(controls$clustering_weighted$enable,
                   "(clustering_dissimilarity:hamming)")
  expect_identical(controls$clustering_lambda$enable,
                   "(clustering_dissimilarity:hamming && clustering_weighted)")
})

test_that("paired-test options explain the required matching", {
  descriptions <- lapply(c("GroupTNA", "ClusterTNA"), function(analysis) {
    a <- yaml::read_yaml(testthat::test_path("..", "..", "jamovi", paste0(analysis, ".a.yaml")))
    Filter(function(option) option$name == "permutation_paired", a$options)[[1]]$description
  })
  expect_match(descriptions[[1]], "actor and session number")
  expect_match(descriptions[[2]], "unavailable for clusters")
})
