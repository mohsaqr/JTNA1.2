# Event Data (Single/Group)

CNAEventSingleClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "CNAEventSingleClass",
  inherit = CNAEventSingleBase,
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

      is_multi <- !is.null(self$options$buildModel_variables_group)

      # Set dynamic instructions
      self$results$instructions$setContent(
        cna_instructions_html("event_single")
      )

      # Validate inputs
      if (is.null(self$options$buildModel_variables_long_action)) {
        self$results$errorText$setVisible(FALSE)
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

          if (!is_multi) {
            result <- cna_prepare_event_data(self$data, self$options)
            model <- tna::build_model(
              x = result$prepData, type = type, scaling = scaling
            )
          } else {
            groupColumn <- self$options$buildModel_variables_group
            copyData <- self$data
            copyData[[self$options$buildModel_variables_long_action]] <-
              as.character(copyData[[self$options$buildModel_variables_long_action]])
            if (!is.null(self$options$buildModel_variables_long_time)) {
              copyData[[self$options$buildModel_variables_long_time]] <-
                as.POSIXct(copyData[[self$options$buildModel_variables_long_time]])
            }
            if (!is.null(self$options$buildModel_variables_actor)) {
              copyData[[self$options$buildModel_variables_actor]] <-
                as.character(copyData[[self$options$buildModel_variables_actor]])
            }
            if (!is.null(self$options$buildModel_variables_long_order)) {
              copyData[[self$options$buildModel_variables_long_order]] <-
                as.character(copyData[[self$options$buildModel_variables_long_order]])
            }

            columnToUseLong <- c(
              self$options$buildModel_variables_long_time,
              self$options$buildModel_variables_actor,
              self$options$buildModel_variables_long_action,
              self$options$buildModel_variables_long_order,
              groupColumn
            )
            longData <- copyData[
              columnToUseLong[!vapply(columnToUseLong, is.null, logical(1))]
            ]

            args_prepare_data <- list(
              data = longData,
              actor = self$options$buildModel_variables_actor,
              time = self$options$buildModel_variables_long_time,
              action = self$options$buildModel_variables_long_action,
              time_threshold = self$options$buildModel_threshold,
              order = self$options$buildModel_variables_long_order
            )
            args_prepare_data <- args_prepare_data[
              !vapply(args_prepare_data, is.null, logical(1))
            ]
            prepData <- do.call(tna::prepare_data, args_prepare_data)

            group <- prepData$long_data[
              !duplicated(prepData$long_data$.session_id),
            ][[groupColumn]]
            model <- tna::group_model(
              x = prepData, group = group, type = type, scaling = scaling
            )
          }

        }, error = function(e) {
          error_msg <- tolower(as.character(e$message))
          if (grepl("time|date|posix|format", error_msg) ||
              grepl("character string is not in a standard unambiguous format",
                    error_msg)) {
            self$results$errorText$setContent(
              "Please enter an appropriate time format"
            )
          } else {
            self$results$errorText$setContent(
              paste("Data preparation error:", e$message)
            )
          }
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
      if (is_multi) {
        cna_run_permutation(model, self$options, self$results)
        cna_run_compare(model, self$options, self$results, mode = "group")
        if (!is.null(model) && isTRUE(self$options$compare_show_network_diff_plot)) {
          self$results$compare_network_diff_plot$setVisible(TRUE)
        }
      }
    },

    # ── Plot functions ──

    .showBuildModelPlot = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      is_multi <- !is.null(self$options$buildModel_variables_group)
      cna_plot_model(plotData, self$options, self$results, is_multi = is_multi)
    },

    .showBuildModelHisto = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (!self$options$buildModel_show_histo) return(FALSE)
      is_multi <- !is.null(self$options$buildModel_variables_group)
      cna_plot_histo(plotData, self$options, is_multi = is_multi)
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
      is_multi <- !is.null(self$options$buildModel_variables_group)
      cna_plot_community(plotData, self$options, is_multi = is_multi)
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
      is_multi <- !is.null(self$options$buildModel_variables_group)
      cna_plot_bootstrap(plotData, is_multi = is_multi)
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
