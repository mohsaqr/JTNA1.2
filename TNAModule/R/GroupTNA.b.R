# This file is a generated template, your changes will not be overwritten

GroupTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "GroupTNAClass",
  inherit = GroupTNABase,
  private = list(
    .prep = NULL,

    .sections = c("model", "centrality", "community", "cliques", "bootstrap",
                  "permutation", "compare", "networkDiff", "compareSequences",
                  "sequences", "patterns", "indices"),

    .run = function() {
      .jtna_stamp_version(self$results)
      vars <- .jtna_vars(self$options)
      ready <- all(c("action", "actor", "group") %in% names(vars))
      self$results$instructions$setVisible(!ready)
      if (!ready) {
        self$results$instructions$setContent(.jtna_instructions("group"))
        .jtna_hide_sections(self$results, private$.sections)
        return()
      }

      model <- private$.model()
      opts <- self$options
      res <- self$results
      missing_group_events <- sum(is.na(self$data[[opts$buildModel_variables_long_group]]))
      if (missing_group_events > 0L) {
        .jtna_notice(opts, res$model,
          sprintf("Excluded %d events with missing Group values.", missing_group_events),
          name = "missing_group", type = jmvcore::NoticeType$WARNING)
      }
      private$.section("model", "transition models", \() {
        .jtna_run_matrix(res$model, opts, model, "group")
      })
      private$.section("centrality", "centrality measures",
        \() .jtna_run_centrality(res$centrality, opts, model, "group"))
      private$.section("community", "community detection",
        \() .jtna_run_community(res$community, opts, model, "group"))
      private$.section("cliques", "cliques",
        \() .jtna_run_cliques(res$cliques, opts, model))
      private$.section("bootstrap", "bootstrap",
        \() .jtna_run_bootstrap(res$bootstrap, opts, model, "group"))
      private$.section("permutation", "permutation test", \() {
        if (isTRUE(opts$permutation_paired)) {
          # tna pairs rows by position; equal group sizes alone do not
          # establish that these are the same actors and sessions.
          prepared <- private$.prepared()
          keys <- data.frame(
            actor = as.character(.jtna_session_values(prepared, opts$buildModel_variables_long_actor)),
            session = .jtna_session_values(prepared, ".session_nr"),
            stringsAsFactors = FALSE)
          groups <- private$.groups(prepared)
          paired_keys <- lapply(names(model), \(group) {
            key <- keys[groups == group, , drop = FALSE]
            rownames(key) <- NULL
            key
          })
          if (!all(vapply(paired_keys, identical, logical(1), paired_keys[[1]]))) {
            stop(paste("Paired permutation tests require matching actors and session numbers",
                       "in every group. Use an unpaired test or provide matching sequences."),
                 call. = FALSE)
          }
        }
        .jtna_run_permutation(res$permutation, opts, model, "group", private$.unscaledModel)
      })
      private$.section("compare", "network comparison", \() {
        groups <- names(model)
        i <- .jtna_level(opts$compare_group_i, groups[1])
        j <- .jtna_level(opts$compare_group_j, groups[2])
        .jtna_run_compare(res$compare, opts, model, i, j, "group")
      })
      private$.section("compareSequences", "sequence comparison",
        \() .jtna_run_compare_sequences(res$compareSequences, opts, model, "group"))
      private$.section("sequences", "sequence plots",
        \() .jtna_run_sequences(res$sequences, opts))
      private$.section("patterns", "pattern discovery",
        \() .jtna_run_patterns(res$patterns, opts, private$.prepared, private$.groups))
      private$.section("indices", "sequence indices",
        \() .jtna_run_indices(res$indices, opts, private$.prepared,
                              \(p) c(.jtna_sequence_labels(p, opts), list(group = private$.groups(p)))))
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

    # The group of each sequence, from the prepared data's own metadata.
    .groups = function(prepared) {
      as.character(.jtna_session_values(prepared, self$options$buildModel_variables_long_group))
    },

    .build = function(scaling) {
      prepared <- private$.prepared()
      .jtna_group_model(prepared, private$.groups(prepared), self$options, scaling)
    },

    .model = function() {
      cache <- self$results$modelCache
      if (!is.null(cache$state)) return(cache$state)

      notice <- NULL
      if (identical(self$options$buildModel_type, "attention")) {
        # The attention model weights each event by its whole decayed
        # history, so it can be slow on long sequences.
        notice <- .jtna_notice(
          self$options, self$results$model,
          "Building the attention models. This can take a while on long sequences.",
          name = "attention", type = jmvcore::NoticeType$INFO)
        private$.checkpoint()
      }
      model <- private$.build(.jtna_scaling(self$options))
      if (!is.null(notice)) notice$setVisible(FALSE)
      cache$setState(model)
      model
    },

    .unscaledModel = function() private$.build(character(0L)),

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
