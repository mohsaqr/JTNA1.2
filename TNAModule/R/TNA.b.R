# This file is a generated template, your changes will not be overwritten

TNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "TNAClass",
    inherit = TNABase,
    private = list(
        .prep = NULL,

        .sections = c("model", "centrality", "stability", "edgeBetweenness",
                      "community", "cliques", "bootstrap", "sequences",
                      "patterns", "indices"),

        .run = function() {
            ready <- !is.null(self$options$buildModel_variables_long_action)
            self$results$instructions$setVisible(!ready)
            if (!ready) {
                self$results$instructions$setContent(.jtna_instructions("single"))
                .jtna_hide_sections(self$results, private$.sections)
                return()
            }

            model <- private$.model()
            opts <- self$options
            res <- self$results
            private$.section("model", "transition model", \() {
                .jtna_run_matrix(res$model, opts, model)
                .jtna_check_mosaic(res$model, opts)
            })
            private$.section("centrality", "centrality measures",
                \() .jtna_run_centrality(res$centrality, opts, model))
            private$.section("stability", "centrality stability",
                \() private$.runStability(model))
            private$.section("edgeBetweenness", "edge betweenness",
                \() private$.runEdgeBetweenness(model))
            private$.section("community", "community detection",
                \() .jtna_run_community(res$community, opts, model))
            private$.section("cliques", "cliques",
                \() .jtna_run_cliques(res$cliques, opts, model))
            private$.section("bootstrap", "bootstrap",
                \() .jtna_run_bootstrap(res$bootstrap, opts, model))
            private$.section("sequences", "sequence plots",
              \() .jtna_run_sequences(res$sequences, opts))
            private$.section("patterns", "pattern discovery",
                \() .jtna_run_patterns(res$patterns, opts, private$.prepared))
            private$.section("indices", "sequence indices",
                \() .jtna_run_indices(res$indices, opts, private$.prepared,
                                      \(p) .jtna_sequence_labels(p, opts)))
        },

        # ---- Data and model ------------------------------------------------

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

        .model = function() {
            cache <- self$results$modelCache
            if (!is.null(cache$state)) return(cache$state)

            prepared <- private$.prepared()
            notice <- NULL
            if (identical(self$options$buildModel_type, "attention")) {
                # The attention model weights each event by its whole decayed
                # history, so it can be slow on long sequences.
                notice <- .jtna_notice(
                    self$options, self$results$model,
                    "Building the attention model. This can take a while on long sequences.",
                    name = "attention", type = jmvcore::NoticeType$INFO)
                private$.checkpoint()
            }
            model <- tna::build_model(
                x = prepared,
                type = self$options$buildModel_type,
                scaling = .jtna_scaling(self$options),
                params = .jtna_params(self$options))
            if (!is.null(notice)) notice$setVisible(FALSE)
            cache$setState(model)
            model
        },

        # ---- Sections only TNA has -----------------------------------------

        .runStability = function(model) {
            plot <- self$results$stability$plot
            cs <- plot$state
            if (is.null(cs)) {
                cs <- tna::estimate_cs(
                    x = model,
                    loops = self$options$centrality_loops,
                    normalize = self$options$centrality_normalize,
                    measures = .jtna_stability_measures(self$options),
                    iter = self$options$centrality_stability_iteration,
                    threshold = self$options$centrality_stability_threshold,
                    certainty = self$options$centrality_stability_certainty,
                    progressbar = FALSE)
                plot$setState(cs)
            }
            .jtna_fill_table(self$results$stability$table, data.frame(
                measure = names(cs),
                cs_coefficient = vapply(cs, \(m) if (is.null(m$cs_coefficient)) NA_real_ else m$cs_coefficient, numeric(1))))
        },

        .runEdgeBetweenness = function(model) {
            plot <- self$results$edgeBetweenness$plot
            eb <- plot$state
            if (is.null(eb)) {
                eb <- tna::betweenness_network(x = model, directed = FALSE)
                plot$setState(eb)
            }
            table <- self$results$edgeBetweenness$table
            df <- .jtna_edge_betweenness_df(eb$weights)
            if (nrow(df) == 0L) table$setNote("empty", "No edge has a positive betweenness.")
            .jtna_fill_table(table, df)
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

        .showStabilityPlot = function(image, ...) {
            cs <- self$results$stability$plot$state
            if (is.null(cs)) return(FALSE)
            .jtna_draw("The stability plot", \() print(plot(cs)))
        },

        .showEdgeBetweennessPlot = function(image, ...) {
            eb <- self$results$edgeBetweenness$plot$state
            if (is.null(eb)) return(FALSE)
            .jtna_draw("The edge betweenness plot", \() {
                args <- .jtna_network_args(self$options, "edgeBetweenness_plot")
                do.call(cograph::splot, c(list(x = eb, directed = FALSE, edge_style = 1,
                    weight_digits = 0, background = "transparent"), args))
            })
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
