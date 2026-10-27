# This file is a generated template, your changes will not be overwritten

ClusterTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ClusterTNAClass",
  inherit = ClusterTNABase,
  private = list(
    .prep = NULL,

    .sections = c("model", "centrality", "community", "cliques", "bootstrap",
                  "permutation", "compare", "networkDiff", "compareSequences",
                  "sequences", "patterns", "indices"),

    .run = function() {
      vars <- .jtna_vars(self$options)
      ready <- all(c("action", "actor") %in% names(vars)) && isTRUE(self$options$clustering_run)
      self$results$instructions$setVisible(!ready)
      if (!ready) {
        self$results$instructions$setContent(.jtna_instructions("cluster"))
        .jtna_hide_sections(self$results, private$.sections)
        return()
      }

      model <- private$.model()
      opts <- self$options
      res <- self$results
      private$.section("model", "cluster models", \() {
        .jtna_run_matrix(res$model, opts, model, "cluster")
        .jtna_scaling_notice(res$model, opts, "cluster")
        .jtna_check_mosaic(res$model, opts)
      })
      private$.section("centrality", "centrality measures",
        \() .jtna_run_centrality(res$centrality, opts, model, "cluster"))
      private$.section("community", "community detection",
        \() .jtna_run_community(res$community, opts, model, "cluster"))
      private$.section("cliques", "cliques",
        \() .jtna_run_cliques(res$cliques, opts, model))
      private$.section("bootstrap", "bootstrap",
        \() .jtna_run_bootstrap(res$bootstrap, opts, model, "cluster"))
      private$.section("permutation", "permutation test",
        \() .jtna_run_permutation(res$permutation, opts, model, "cluster", private$.unscaledModel))
      private$.section("compare", "network comparison", \() {
        k <- length(model)
        picked <- c(opts$compare_cluster_i, opts$compare_cluster_j)
        if (any(picked > k)) {
          stop(sprintf("choose clusters between 1 and %d.", k), call. = FALSE)
        }
        .jtna_run_compare(res$compare, opts, model, names(model)[picked[1]],
                          names(model)[picked[2]], "cluster")
      })
      private$.section("compareSequences", "sequence comparison",
        \() .jtna_run_compare_sequences(res$compareSequences, opts, model, "cluster"))
      clusters <- attr(model, "jtna_clusters")
      private$.section("sequences", "sequence plots",
        \() .jtna_run_sequences(res$sequences, opts))
      private$.section("patterns", "pattern discovery",
        \() .jtna_run_patterns(res$patterns, opts, private$.prepared, \(p) clusters))
      private$.section("indices", "sequence indices",
        \() .jtna_run_indices(res$indices, opts, private$.prepared,
                              \(p) c(.jtna_sequence_labels(p, opts), list(cluster = clusters))))
    },

    # ---- Data and model --------------------------------------------------

    .section = function(name, label, fun) {
      section <- self$results$get(name)
      if (!section$visible) return(invisible())
      .jtna_section(self$options, section, label, fun)
    },

    # The prepared sequences, computed at most once per run and only when
    # something needs them; never stored in the saved results.
    .prepared = function() {
      if (is.null(private$.prep)) {
        private$.prep <- .jtna_prepare(self$data, self$options)
      }
      private$.prep
    },

    # Cluster the sequences, then build one network per cluster with the
    # model type, scaling and lambda chosen in the panel. The cluster of each
    # sequence is kept on the model for pattern discovery and indices.
    .model = function() {
      cache <- self$results$modelCache
      if (!is.null(cache$state)) return(cache$state)

      prepared <- private$.prepared()
      sequences <- as.data.frame(prepared$sequence_data)
      k <- self$options$clustering_k
      if (k >= nrow(sequences)) {
        .jtna_reject(sprintf(
          "The number of clusters (%d) must be smaller than the number of sequences (%d).",
          k, nrow(sequences)))
      }
      clustering <- tna::cluster_sequences(
        data = sequences,
        k = k,
        dissimilarity = self$options$clustering_dissimilarity,
        method = self$options$clustering_method)
      clusters <- paste("Cluster", clustering$assignments)

      model <- .jtna_group_model(prepared, clusters, self$options)
      attr(model, "jtna_clusters") <- clusters
      cache$setState(model)
      model
    },

    .unscaledModel = function() {
      model <- self$results$modelCache$state
      .jtna_group_model(private$.prepared(), attr(model, "jtna_clusters"), self$options,
                        scaling = character(0L))
    },

    # ---- Plots ------------------------------------------------------------

    .showModelPlot = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The network plot", \() .jtna_draw_model(model, self$options))
    },

    .showModelHistogram = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The histogram", \() .jtna_draw_histogram(model))
    },

    .showModelFrequencies = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The frequencies plot", \() print(tna::plot_frequencies(x = model)))
    },

    .showModelMosaic = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The mosaic plot", \() print(tna::plot_mosaic(x = model, digits = 2)))
    },

    .showCentralityPlot = function(image, ...) {
      cent <- self$results$centrality$table$state
      if (is.null(cent)) return(FALSE)
      .jtna_draw("The centrality plot", \() print(plot(cent)))
    },

    .showCommunityPlot = function(image, ...) {
      coms <- self$results$community$plot$state
      if (is.null(coms)) return(FALSE)
      .jtna_draw("The community plot", \() .jtna_draw_community(coms, self$options))
    },

    .showCliquesPlot = function(image, ...) {
      cliques <- self$results$cliques$plot$state
      if (is.null(cliques)) return(FALSE)
      .jtna_draw("The cliques plot", \() .jtna_draw_cliques(cliques, self$options))
    },

    .showBootstrapPlot = function(image, ...) {
      bs <- self$results$bootstrap$plot$state
      if (is.null(bs)) return(FALSE)
      .jtna_draw("The bootstrap plot", \() .jtna_draw_bootstrap(bs, self$options))
    },

    .showPermutationPlot = function(image, ...) {
      perm <- self$results$permutation$plot$state
      if (is.null(perm)) return(FALSE)
      .jtna_draw("The permutation plot", \() .jtna_draw_permutation(perm, self$options))
    },

    .showComparePlot = function(image, ...) {
      state <- self$results$compare$plot$state
      if (is.null(state)) return(FALSE)
      .jtna_draw("The comparison plot", \() .jtna_draw_compare(state, self$options))
    },

    .showNetworkDiffPlot = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The difference networks", \() .jtna_draw_network_diff(model, self$options))
    },

    .showCompareSequencesPlot = function(image, ...) {
      cs <- self$results$compareSequences$plot$state
      if (is.null(cs)) return(FALSE)
      .jtna_draw("The sequence comparison plot", \() .jtna_draw_compare_sequences(cs))
    },

    .showSequenceIndexPlot = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The sequence index plot", \() .jtna_draw_sequences(model, self$options, "index"))
    },
    .showSequenceDistributionPlot = function(image, ...) {
      model <- self$results$modelCache$state
      if (is.null(model)) return(FALSE)
      .jtna_draw("The distribution plot", \() .jtna_draw_sequences(model, self$options, "distribution"))
    }
  )
)
