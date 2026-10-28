test_that("clustering preserves literal states and is invariant to their renaming", {
  sequences <- data.frame(
    T1 = c("B", "*", "%", "", "B", "*", "*", "*"),
    T2 = c("%", "B", "", "%", "", "*", "%", "B"),
    T3 = c("B", "%", "B", "", "", "B", "", "B"),
    T4 = c("*", "*", "", "B", "B", "B", "%", "B"),
    T5 = c("*", "", "", "B", "B", "*", "", "B"))
  renamed <- sequences
  replacements <- c("B", "star", "percent", "empty")
  renamed[] <- lapply(renamed, \(x) replacements[match(x, c("B", "*", "%", ""))])
  options <- ClusterTNAOptions$new(clustering_k = 2)
  literal <- .jtna_cluster_sequences(sequences, options)
  reference <- tna::cluster_sequences(renamed, k = 2)
  expect_equal(literal$distance, reference$distance)
  expect_identical(outer(literal$assignments, literal$assignments, "=="),
                   outer(reference$assignments, reference$assignments, "=="))
  # An independent Hamming distance verifies that missing-code handling
  # cannot be hidden by comparing two calls with the same wrong setting.
  expected <- sum(sequences[1, ] != sequences[2, ])
  expect_equal(as.matrix(literal$distance)[1, 2], expected)
  expect_equal(literal, tna::cluster_sequences(sequences, k = 2, na_syms = character()))

  long <- function(x) data.frame(actor = rep(seq_len(nrow(x)), each = ncol(x)),
                                 action = as.vector(t(as.matrix(x))))
  run <- function(data) jtna_run("ClusterTNA", data = data,
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
    clustering_run = TRUE, clustering_k = 2, buildModel_show_plot = FALSE)
  actual <- run(long(sequences))$modelCache$state
  renamed_model <- run(long(renamed))$modelCache$state
  expect_setequal(actual[[1]]$labels, c("B", "*", "%", ""))
  expect_identical(attr(actual, "jtna_clusters"), attr(renamed_model, "jtna_clusters"))
})

test_that("clustering treats literal star and percent as distinct events", {
  sequences <- data.frame(T1 = rep("A", 4), T2 = c("*", "%", "B", "B"), T3 = rep("A", 4))
  actual <- .jtna_cluster_sequences(sequences, ClusterTNAOptions$new(clustering_k = 2))
  expect_equal(as.matrix(actual$distance)[1, 2], 1)
})

test_that("ordinary and ragged sequences retain tna clustering results", {
  sequences <- data.frame(T1 = c("A", "B", "A", "B"),
                          T2 = c("B", "A", "B", "A"),
                          T3 = c("A", NA, "B", "A"))
  lapply(c("hamming", "lv", "lcs"), function(distance) {
    options <- ClusterTNAOptions$new(clustering_k = 2, clustering_dissimilarity = distance)
    expect_equal(.jtna_cluster_sequences(sequences, options),
                 tna::cluster_sequences(sequences, k = 2, dissimilarity = distance))
  })
})

test_that("clustering rejects encoder overflow with an actionable input error", {
  make_sequences <- function(n) {
    x <- as.data.frame(matrix(sprintf("s%02d", rep(seq_len(n), 4)), 4, n, byrow = TRUE))
    x[2, 1] <- "s02"
    x
  }
  options <- ClusterTNAOptions$new(clustering_k = 2)
  sixty_two <- make_sequences(62)
  expect_equal(.jtna_cluster_sequences(sixty_two, options),
               tna::cluster_sequences(sixty_two, k = 2))
  expect_error(.jtna_cluster_sequences(make_sequences(64), options),
                "64 distinct actions.*supports 62", class = "jtna_bad_input")
  sixty_two[4, 62] <- NA_character_
  expect_error(.jtna_cluster_sequences(sixty_two, options),
                "missing or padded events", class = "jtna_bad_input")
  sixty <- make_sequences(60)
  sixty[1, 1] <- NA_character_
  expect_equal(.jtna_cluster_sequences(sixty, options),
               tna::cluster_sequences(sixty, k = 2))
  sixty_one <- make_sequences(61)
  sixty_one[4, 61] <- NA_character_
  expect_equal(.jtna_cluster_sequences(sixty_one, options),
               tna::cluster_sequences(sixty_one, k = 2))
  sixty_one[1, 1] <- NA_character_
  expect_error(.jtna_cluster_sequences(sixty_one, options),
                "needs 63 symbols", class = "jtna_bad_input")
  large <- make_sequences(64)
  long <- data.frame(actor = rep(seq_len(nrow(large)), each = ncol(large)),
                     action = as.vector(t(as.matrix(large))))
  expect_error(jtna_run("ClusterTNA", data = long,
    buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
    clustering_run = TRUE, clustering_k = 2), "Reduce the number of action categories",
    class = "jtna_bad_input")
})
