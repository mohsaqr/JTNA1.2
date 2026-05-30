# Binary Data (Clustering)

CNABinaryClusterClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "CNABinaryClusterClass",
  inherit = CNABinaryClusterBase,
  private = list(
    .run = function() {

      library("tna")
      source_file <- system.file("R", "cna_utils.R", package = "CNA")
      if (source_file == "") {
        pkg_dir <- find.package("CNA", quiet = TRUE)
        if (length(pkg_dir) > 0) {
          utils_path <- file.path(pkg_dir, "R", "cna_utils.R")
          if (file.exists(utils_path)) source(utils_path, local = TRUE)
        }
      }

      is_multi <- TRUE

      # Set dynamic instructions
      self$results$instructions$setContent(
        cna_instructions_html("binary_cluster")
      )

      # Validate inputs
      if (length(self$options$buildModel_variables_onehot) < 2) {
        self$results$errorText$setContent(
          "Please select at least 2 one-hot columns (actions)."
        )
        self$results$errorText$setVisible(TRUE)
        return()
      }

      # Clustering guard
      if (!isTRUE(self$options$clustering_run)) {
        self$results$clusteringNote$setContent(
          '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
          <p style="text-align:center; font-weight: bold; color: #1a5276; font-size: 14px;">
          Check "Run Clustering Analysis" to start clustering.
          </p>
          </div>'
        )
        self$results$clusteringNote$setVisible(TRUE)
        return()
      }

      # Hide build model title if no outputs selected
      if (!self$options$buildModel_show_matrix &&
          !self$options$buildModel_show_plot &&
          !self$options$buildModel_show_histo) {
        self$results$buildModelTitle$setVisible(FALSE)
      }

      # Build model
      model <- NULL

      if (self$results$buildModelContent$isFilled()) {
        model <- self$results$buildModelContent$state
      } else if (!is.null(self$data) && ncol(self$data) >= 1) {

        tryCatch({
          scaling <- self$options$buildModel_scaling
          if (scaling == "noScaling") scaling <- character(0L)
          type <- "co-occurrence"
          k <- self$options$clustering_k

          result <- cna_prepare_binary_data(self$data, self$options)
          seq_data <- result$seq_data

          n_sequences <- nrow(seq_data)
          if (k > n_sequences) {
            self$results$errorText$setContent(
              paste("k =", k, "exceeds number of rows (",
                    n_sequences, "). Reduce k.")
            )
            self$results$errorText$setVisible(TRUE)
            return()
          }

          clusters <- tna::cluster_sequences(
            data = seq_data,
            k = k,
            dissimilarity = self$options$clustering_dissimilarity,
            method = self$options$clustering_method
          )
          model <- tna::group_model(
            clusters, type = type, scaling = scaling
          )

        }, error = function(e) {
          self$results$errorText$setContent(
            paste("Data preparation error:", e$message)
          )
          self$results$errorText$setVisible(TRUE)
          return()
        })
      }

      if (!is.null(model)) {
        if (!self$results$buildModelContent$isFilled()) {
          self$results$buildModelContent$setContent(model)
          self$results$buildModelContent$setState(model)
        }
        self$results$buildModelContent$setVisible(
          self$options$buildModel_show_matrix
        )
        self$results$buildModel_plot$setVisible(
          self$options$buildModel_show_plot
        )
        self$results$buildModel_histo$setVisible(
          self$options$buildModel_show_histo
        )
        self$results$buildModel_frequencies$setVisible(
          self$options$buildModel_show_frequencies
        )
        self$results$buildModel_mosaic$setVisible(
          self$options$buildModel_show_mosaic
        )
      }

      # Run analyses
      cna_run_centrality(model, self$options, self$results)
      cna_run_community(model, self$options, self$results, is_multi)
      cna_run_cliques(model, self$options, self$results)
      cna_run_bootstrap(model, self$options, self$results, is_multi)
      cna_run_permutation(model, self$options, self$results)
      cna_run_compare(model, self$options, self$results, mode = "cluster")

      # Network Difference Plot
      if (!is.null(model) && isTRUE(self$options$compare_show_network_diff_plot)) {
        self$results$compare_network_diff_plot$setVisible(TRUE)
      }
    },

    # ── Plot functions ──

    .showBuildModelPlot = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      cna_plot_model(plotData, self$options, self$results, is_multi = TRUE)
    },

    .showBuildModelHisto = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (!self$options$buildModel_show_histo) return(FALSE)
      cna_plot_histo(plotData, self$options, is_multi = TRUE)
    },

    .showBuildModelFrequencies = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData) || !self$options$buildModel_show_frequencies) {
        return(FALSE)
      }
      cna_plot_frequencies(plotData)
    },

    .showBuildModelMosaic = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData) || !self$options$buildModel_show_mosaic) {
        return(FALSE)
      }
      cna_plot_mosaic(plotData, self$options)
    },

    .showCentralityPlot = function(image, ...) {
      plotData <- self$results$centralityTable$state
      if (is.null(plotData) || !self$options$centrality_show_plot) {
        return(FALSE)
      }
      cna_plot_centrality(plotData)
    },

    .showCommunityPlot = function(image, ...) {
      plotData <- self$results$community_plot$state
      if (is.null(plotData) || !self$options$community_show_plot) {
        return(FALSE)
      }
      cna_plot_community(plotData, self$options, is_multi = TRUE)
    },

    .showCliquesPlot = function(image, ...) {
      plotData <- self$results$cliques_multiple_plot$state
      if (is.null(plotData) || !self$options$cliques_show_plot) return(FALSE)
      cna_plot_cliques(plotData, self$options)
    },

    .showBootstrapPlot = function(image, ...) {
      plotData <- self$results$bootstrap_plot$state
      if (is.null(plotData) || !self$options$bootstrap_show_plot) {
        return(FALSE)
      }
      cna_plot_bootstrap(plotData, is_multi = TRUE)
    },

    .showPermutationPlot = function(image, ...) {
      plotData <- self$results$permutation_plot$state
      if (is.null(plotData) || !self$options$permutation_show_plot) {
        return(FALSE)
      }
      cna_plot_permutation(plotData)
    },

    .showComparePlot = function(image, ...) {
      if (!isTRUE(self$options$compare_show_plot)) return(FALSE)
      stateData <- self$results$compare_plot$state
      cna_plot_compare(stateData, self$options)
    },

    .showCompareNetworkDiffPlot = function(image, ...) {
      if (!isTRUE(self$options$compare_show_network_diff_plot)) return(FALSE)
      model <- self$results$buildModelContent$state
      cna_plot_network_diff(model, self$options)
    }
  )
)
