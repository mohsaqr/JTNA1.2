# Layer 1 (complete): every results table of every analysis equals a direct,
# identically seeded tna / codyna call on the same prepared data.

skip_if_not_installed("tna")
skip_if_not_installed("codyna")
skip_if_not_installed("pkgload")

tol <- sqrt(.Machine$double.eps)
seeded <- function(expr) .jtna_with_seed(11, \() expr)
data_eq <- jtna_small(n_actor = 24, per = 18)
roles <- list(buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
              buildModel_variables_long_time = "time")
run_eq <- function(analysis, ...) {
  extra <- switch(analysis,
    TNA = list(),
    GroupTNA = list(buildModel_variables_long_group = "group"),
    ClusterTNA = list(clustering_run = TRUE, clustering_k = 2))
  do.call(jtna_run, c(list(analysis, data = data_eq), roles, extra,
                      list(random_seed_use = TRUE, random_seed = 11), list(...)))
}
prepared_eq <- suppressMessages(tna::prepare_data(data_eq, actor = "actor", time = "time", action = "action"))
sequences_eq <- as.data.frame(lapply(prepared_eq$sequence_data, as.character), stringsAsFactors = FALSE)
key <- function(...) do.call(paste, c(list(...), sep = "|"))

for (analysis in c("TNA", "GroupTNA", "ClusterTNA")) {
  id <- switch(analysis, TNA = NULL, GroupTNA = "group", ClusterTNA = "cluster")

  test_that(sprintf("%s: centralities equal tna::centralities", analysis), {
    r <- run_eq(analysis, centrality_show_table = TRUE, centrality_Closeness = TRUE,
                centrality_Diffusion = TRUE, centrality_Clustering = TRUE, centrality_BetweennessRSP = TRUE,
                centrality_ClosenessIn = TRUE, centrality_ClosenessOut = TRUE)
    got <- jtna_table(r$centrality$table)
    measures <- .jtna_centrality_measures
    ref <- as.data.frame(tna::centralities(r$modelCache$state, measures = measures))
    ref_key <- if (is.null(id)) as.character(ref$state) else key(ref$group, ref$state)
    got_key <- if (is.null(id)) got$state else key(got[[id]], got$state)
    ref <- ref[match(got_key, ref_key), ]
    lapply(measures, \(m) expect_equal(got[[m]], ref[[m]], tolerance = tol, label = paste(analysis, m)))
  })

  test_that(sprintf("%s: communities equal tna::communities", analysis), {
    methods <- c("walktrap", "fast_greedy", "label_prop", "infomap", "edge_betweenness", "leading_eigen")
    lapply(methods, \(m) {
      r <- run_eq(analysis, community_show_table = TRUE, community_methods = m)
      got <- jtna_table(r$community$table)
      direct <- seeded(tna::communities(r$modelCache$state, methods = m))
      networks <- if (is.null(id)) list(direct) else direct
      ref_states <- unlist(lapply(networks, \(network) as.character(network$assignments$state)), use.names = FALSE)
      ref_values <- unlist(lapply(networks, \(network) as.integer(network$assignments[[m]])), use.names = FALSE)
      expect_identical(got$state, ref_states, label = paste(analysis, m, "state identity"))
      if (!is.null(id)) expect_identical(got[[id]],
        rep(names(direct), vapply(networks, \(network) nrow(network$assignments), integer(1))))
      expect_equal(got$community, ref_values, label = paste(analysis, m))
    })
  })

  test_that(sprintf("%s: cliques equal tna::cliques", analysis), {
    r <- run_eq(analysis, cliques_show_plot = TRUE, cliques_size = 3, cliques_threshold = 0.02)
    ref <- tna::cliques(r$modelCache$state, size = 3, threshold = 0.02)
    expect_equal(r$cliques$plot$state, ref, ignore_attr = TRUE)
  })

  test_that(sprintf("%s: bootstrap table equals tna::bootstrap", analysis), {
    lapply(c("stability", "threshold"), \(method) {
      r <- run_eq(analysis, bootstrap_show_table = TRUE, bootstrap_iteration = 40,
                  bootstrap_method = method, bootstrap_table_show_all = TRUE)
      got <- jtna_table(r$bootstrap$table)
      # Omit default arguments, and compare raw upstream fields without using
      # the production table mapper to construct its own expected result.
      direct <- seeded(tna::bootstrap(r$modelCache$state, iter = 40, method = method))
      networks <- if (is.null(id)) list(direct) else direct
      ref <- do.call(rbind, lapply(networks, \(network) as.data.frame(network$summary)))
      expected_key <- key(ref$from, ref$to)
      actual_key <- key(got$from, got$to)
      if (!is.null(id)) {
        groups <- rep(names(direct), vapply(networks, \(network) nrow(network$summary), integer(1)))
        expected_key <- key(groups, expected_key)
        actual_key <- key(got[[id]], actual_key)
      }
      expect_equal(nrow(got), nrow(ref), label = paste(analysis, method))
      expect_setequal(actual_key, expected_key)
      ref <- ref[match(actual_key, expected_key), , drop = FALSE]
      expect_identical(got$significant, ifelse(ref$sig, "Yes", "No"))
      lapply(c("weight", "p_value", "cr_lower", "cr_upper", "ci_lower", "ci_upper"), \(col) {
        expect_equal(got[[col]], ref[[col]], tolerance = tol, label = paste(analysis, method, col))
      })
    })
  })

  test_that(sprintf("%s: pattern discovery equals codyna::discover_patterns", analysis), {
    lapply(c("ngram", "gapped", "repeated"), \(type) {
      r <- run_eq(analysis, pattern_show_table = TRUE, pattern_type = type, pattern_min_count = 1,
                  pattern_table_show_all = TRUE)
      got <- jtna_table(r$patterns$table)
      outcome <- switch(analysis, TNA = NULL,
        GroupTNA = .jtna_session_values(prepared_eq, "group"),
        ClusterTNA = attr(r$modelCache$state, "jtna_clusters"))
      args <- list(data = sequences_eq, type = type, len = 2:5, gap = 1:3, min_support = 0.01,
                   min_freq = 1)
      if (!is.null(outcome)) args$outcome <- outcome
      ref <- as.data.frame(do.call(codyna::discover_patterns, args))
      expect_identical(got$pattern, ref$pattern, label = paste(analysis, type))
      expect_equal(got$frequency, ref$frequency, tolerance = tol, label = paste(analysis, type))
      expect_equal(got$support, ref$support, tolerance = tol, label = paste(analysis, type))
      if (!is.null(outcome)) {
        # Compatibility correction: codyna tests uniform counts, whereas
        # JTNA reports association of presence/absence with group.
        groups <- unique(as.character(outcome))
        sizes <- vapply(groups, function(group) sum(outcome == group), integer(1))
        expected_p <- vapply(seq_len(nrow(ref)), function(i) {
          present <- as.numeric(ref[i, paste0("count_", groups)])
          if (sum(present) == sum(sizes)) return(1)
          suppressWarnings(chisq.test(cbind(present, sizes - present), correct = FALSE))$p.value
        }, numeric(1))
        expect_equal(got$p_value, expected_p, tolerance = tol, label = paste(analysis, type))
      }
    })
  })

  test_that(sprintf("%s: sequence indices equal codyna (corrected dominant spell)", analysis), {
    r <- run_eq(analysis, indices_show_table = TRUE, indices_table_show_all = TRUE,
                indices_favorable = "plan", indices_omega = 2)
    got <- jtna_table(r$indices$table)
    ref <- as.data.frame(codyna::sequence_indices(sequences_eq, favorable = "plan", omega = 2))
    numeric_cols <- setdiff(names(ref)[vapply(ref, is.numeric, logical(1))], "dominant_max_spell")
    lapply(numeric_cols, \(col) expect_equal(as.numeric(got[[col]]), ref[[col]], tolerance = tol,
                                             label = paste(analysis, col)))
    lapply(c("first_state", "last_state", "dominant_state"), \(col) {
      expect_identical(as.character(got[[col]]), ref[[col]], label = paste(analysis, col))
    })
  })
}

test_that("TNA: stability and edge betweenness equal tna", {
  r <- run_eq("TNA", centrality_stability_show_table = TRUE, centrality_stability_iteration = 20,
              edgeBetweenness_show_table = TRUE)
  cs <- seeded(tna::estimate_cs(r$modelCache$state, iter = 20, measures = c("InStrength", "OutStrength",
                                "Betweenness"), progressbar = FALSE))
  got <- jtna_table(r$stability$table)
  expect_equal(got$cs_coefficient, vapply(cs[got$measure], \(m) m$cs_coefficient, numeric(1)),
               tolerance = tol, ignore_attr = TRUE)
  eb <- tna::betweenness_network(r$modelCache$state)  # tna's default: directed
  ref <- .jtna_edge_betweenness_df(eb$weights)
  got <- jtna_table(r$edgeBetweenness$table)
  expect_equal(key(got$from, got$to, got$value), key(ref$from, ref$to, ref$value))
})

for (analysis in c("GroupTNA", "ClusterTNA")) {
  id <- if (analysis == "GroupTNA") "group" else "cluster"

  test_that(sprintf("%s: permutation table equals tna::permutation_test", analysis), {
    lapply(c("none", "holm"), \(adjust) {
      r <- run_eq(analysis, permutation_show_text = TRUE, permutation_iter = 40, permutation_adjust = adjust,
                  permutation_table_show_all = TRUE)
      got <- jtna_table(r$permutation$table)
      direct <- seeded(tna::permutation_test(r$modelCache$state, iter = 40, adjust = adjust))
      ref <- do.call(rbind, lapply(direct, \(pair) as.data.frame(pair$edges$stats)))
      pairs <- rep(names(direct), vapply(direct, \(pair) nrow(pair$edges$stats), integer(1)))
      expected_key <- key(pairs, trimws(as.character(ref$edge_name)))
      actual_key <- key(got[[paste0(id, "_comparison")]], got$edge_name)
      expect_equal(nrow(got), nrow(ref))
      expect_setequal(actual_key, expected_key)
      ref <- ref[match(actual_key, expected_key), , drop = FALSE]
      lapply(c("diff_true", "effect_size", "p_value"), \(col) {
        expect_equal(got[[col]], ref[[col]], tolerance = tol, label = paste(analysis, adjust, col))
      })
    })
  })

  test_that(sprintf("%s: network comparison equals tna::compare", analysis), {
    r <- run_eq(analysis, compare_show_summary = TRUE, compare_show_network = TRUE)
    model <- r$modelCache$state
    ref <- tna::compare(model, i = 1, j = 2, network = TRUE)
    expect_equal(jtna_table(r$compare$summary)$value, as.numeric(ref$summary_metrics$value), tolerance = tol)
    nm <- as.data.frame(ref$network_metrics)
    got <- jtna_table(r$compare$network)
    expect_equal(got$net_i, as.numeric(nm[[2]]), tolerance = tol)
    expect_equal(got$net_j, as.numeric(nm[[3]]), tolerance = tol)
  })

  test_that(sprintf("%s: sequence comparison equals tna::compare_sequences", analysis), {
    r <- run_eq(analysis, compare_sequences_show_table = TRUE, compare_sequences_min_freq = 3,
                compare_sequences_sub_max = 3, compare_sequences_correction = "holm")
    got <- jtna_table(r$compareSequences$table)
    ref <- seeded(tna::compare_sequences(r$modelCache$state, sub = 2:3, min_freq = 3, adjust = "holm"))
    ref <- as.data.frame(ref)
    ref <- ref[match(got$pattern, ref$pattern), ]
    expect_false(anyNA(ref$pattern))
    expect_equal(got$p_value, ref$p_value, tolerance = tol)
    expect_equal(got$effect_size, ref$effect_size, tolerance = tol)
  })
}
