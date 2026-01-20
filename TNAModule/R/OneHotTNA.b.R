# One-Hot Co-occurrence Network Analysis

OneHotTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "OneHotTNAClass",
  inherit = OneHotTNABase,
  private = list(
    .run = function() {
      library("tna")

      # Check if one-hot columns are provided
      if (length(self$options$buildModel_variables_onehot) < 2) {
        self$results$errorText$setContent("Please select at least 2 one-hot columns (actions)")
        self$results$errorText$setVisible(TRUE)
        return()
      }

      scaling <- self$options$buildModel_scaling
      onehot_cols <- self$options$buildModel_variables_onehot
      actor_col <- self$options$buildModel_variables_actor
      session_col <- self$options$buildModel_variables_session
      window_size <- self$options$buildModel_window

      model <- NULL

      if (self$results$buildModelContent$isFilled()) {
        model <- self$results$buildModelContent$state
      } else if (!is.null(self$data) && ncol(self$data) >= 1) {

        tryCatch({
          # Step 1: Convert one-hot to sequence format
          df <- as.data.frame(self$data)

          # Create sequence data: replace 1 with column name, 0 with NA
          seq_data <- as.data.frame(lapply(onehot_cols, function(col) {
            ifelse(df[[col]] == 1, col, NA)
          }))
          colnames(seq_data) <- onehot_cols

          # Step 2: Aggregate by actor + session/window if provided
          if (!is.null(actor_col) || !is.null(session_col)) {
            # Create grouping variable
            if (!is.null(actor_col) && !is.null(session_col)) {
              # Group by actor + windowed session
              session_numeric <- as.numeric(factor(df[[session_col]]))
              window_id <- floor((session_numeric - 1) / window_size)
              group_id <- paste(df[[actor_col]], window_id, sep = "_")
            } else if (!is.null(actor_col)) {
              # Group by actor only (with row-based window)
              row_window <- floor((seq_len(nrow(df)) - 1) / window_size)
              group_id <- paste(df[[actor_col]], row_window, sep = "_")
            } else {
              # Group by session only
              session_numeric <- as.numeric(factor(df[[session_col]]))
              group_id <- floor((session_numeric - 1) / window_size)
            }

            # Aggregate: take first non-NA value per column per group
            seq_data$..group_id.. <- group_id
            seq_data <- aggregate(
              seq_data[onehot_cols],
              by = list(..group_id.. = seq_data$..group_id..),
              FUN = function(x) {
                non_na <- na.omit(x)
                if (length(non_na) > 0) non_na[1] else NA
              }
            )
            seq_data$..group_id.. <- NULL
          } else if (window_size > 1) {
            # No actor/session, just window by rows
            row_window <- floor((seq_len(nrow(df)) - 1) / window_size)
            seq_data$..group_id.. <- row_window
            seq_data <- aggregate(
              seq_data[onehot_cols],
              by = list(..group_id.. = seq_data$..group_id..),
              FUN = function(x) {
                non_na <- na.omit(x)
                if (length(non_na) > 0) non_na[1] else NA
              }
            )
            seq_data$..group_id.. <- NULL
          }

          # Step 3: Build model using tna::build_model with type="co-occurrence"
          if (scaling == "noScaling") {
            scaling <- character(0L)
          }

          model <- tna::build_model(x = seq_data, type = "co-occurrence", scaling = scaling)

        }, error = function(e) {
          self$results$errorText$setContent(paste("Error:", e$message))
          self$results$errorText$setVisible(TRUE)
          return()
        })
      }

      if (!is.null(model)) {

        if (!self$results$buildModelContent$isFilled()) {
          self$results$buildModelContent$setContent(model)
          self$results$buildModelContent$setState(model)
        }
        self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)
        self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot)
        self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo)
        self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies)
      }

      ### Centrality
      if (!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {
        centrality_loops <- self$options$centrality_loops
        centrality_normalize <- self$options$centrality_normalize

        vectorCharacter <- character(0)
        if (self$options$centrality_OutStrength) vectorCharacter <- c(vectorCharacter, "OutStrength")
        if (self$options$centrality_InStrength) vectorCharacter <- c(vectorCharacter, "InStrength")
        if (self$options$centrality_ClosenessIn) vectorCharacter <- c(vectorCharacter, "ClosenessIn")
        if (self$options$centrality_ClosenessOut) vectorCharacter <- c(vectorCharacter, "ClosenessOut")
        if (self$options$centrality_Closeness) vectorCharacter <- c(vectorCharacter, "Closeness")
        if (self$options$centrality_Betweenness) vectorCharacter <- c(vectorCharacter, "Betweenness")
        if (self$options$centrality_BetweennessRSP) vectorCharacter <- c(vectorCharacter, "BetweennessRSP")
        if (self$options$centrality_Diffusion) vectorCharacter <- c(vectorCharacter, "Diffusion")
        if (self$options$centrality_Clustering) vectorCharacter <- c(vectorCharacter, "Clustering")

        cent <- self$results$centralityTable$state

        if (length(vectorCharacter) > 0 && is.null(cent)) {
          tryCatch({
            cent <- tna::centralities(x = model, loops = centrality_loops, normalize = centrality_normalize, measures = vectorCharacter)
            self$results$centralityTable$setState(cent)
          }, error = function(e) {
            self$results$centralityTable$setNote(key = "error", note = paste("Error:", e$message))
          })
        }

        # Add columns
        for (measure in vectorCharacter) {
          self$results$centralityTable$addColumn(name = measure, type = "number")
        }

        # Populate table
        if (!is.null(cent) && is.data.frame(cent) && nrow(cent) > 0) {
          for (i in 1:nrow(cent)) {
            rowValues <- list(state = rownames(cent)[i])
            for (measure in vectorCharacter) {
              if (measure %in% colnames(cent)) rowValues[[measure]] <- as.numeric(cent[i, measure])
            }
            self$results$centralityTable$addRow(rowKey = i, values = rowValues)
          }
        }

        self$results$centralityTitle$setVisible(self$options$centrality_show_table || self$options$centrality_show_plot)
        self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
        self$results$centralityTable$setVisible(self$options$centrality_show_table)
      }

      ### Community
      if (!is.null(model) && (self$options$community_show_table || self$options$community_show_plot)) {
        community_gamma <- as.numeric(self$options$community_gamma)
        methods <- self$options$community_methods

        coms <- self$results$community_plot$state
        if (is.null(coms)) {
          resultComs <- tryCatch({
            coms <- tna::communities(x = model, methods = methods, gamma = community_gamma)
            self$results$community_plot$setState(coms)

            if (isTRUE(self$options$community_show_table)) {
              membership <- coms[[methods]]$membership
              for (i in seq_along(membership)) {
                self$results$communityTable$addRow(rowKey = i, values = list(
                  state = names(membership)[i],
                  community = as.integer(membership[i])
                ))
              }
            }
            TRUE
          }, error = function(e) {
            self$results$communityErrorText$setContent(paste("Community detection error:", e$message))
            self$results$communityErrorText$setVisible(TRUE)
            FALSE
          })
          if (!resultComs) return()
        }

        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityTable$setVisible(self$options$community_show_table)
        self$results$communityTitle$setVisible(self$options$community_show_table || self$options$community_show_plot)
      }

      ### Cliques
      if (!is.null(model) && (isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot))) {
        cliques_size <- as.numeric(self$options$cliques_size)
        cliques_threshold <- as.numeric(self$options$cliques_threshold)

        cliques <- self$results$cliques_plot$state
        if (is.null(cliques)) {
          cliques <- tna::cliques(x = model, size = cliques_size, threshold = cliques_threshold)
          self$results$cliques_plot$setState(cliques)
          if (isTRUE(self$options$cliques_show_text)) {
            self$results$cliquesContent$setContent(cliques)
          }
        }

        self$results$cliques_plot$setVisible(self$options$cliques_show_plot)
        self$results$cliquesContent$setVisible(self$options$cliques_show_text)
        self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)
      }

      ### Bootstrap
      if (!is.null(model) && (isTRUE(self$options$bootstrap_show_table) || isTRUE(self$options$bootstrap_show_plot))) {
        bs <- self$results$bootstrap_plot$state
        if (is.null(bs)) {
          bs <- tna::bootstrap(
            x = model,
            iter = self$options$bootstrap_iteration,
            level = self$options$bootstrap_level,
            method = self$options$bootstrap_method,
            threshold = self$options$bootstrap_threshold,
            consistency_range = c(self$options$bootstrap_range_low, self$options$bootstrap_range_up)
          )
          self$results$bootstrap_plot$setState(bs)
        }

        # Populate table
        if (!is.null(bs) && isTRUE(self$options$bootstrap_show_table)) {
          if (!is.null(bs$summary) && nrow(bs$summary) > 0) {
            for (i in 1:nrow(bs$summary)) {
              row <- bs$summary[i, ]
              self$results$bootstrapTable$addRow(rowKey = i, values = list(
                from = as.character(row$from),
                to = as.character(row$to),
                weight = as.numeric(row$weight),
                p_value = as.numeric(row$p_value),
                cr_lower = as.numeric(row$cr_lower),
                cr_upper = as.numeric(row$cr_upper),
                ci_lower = as.numeric(row$ci_lower),
                ci_upper = as.numeric(row$ci_upper),
                significant = ifelse(row$sig, "Yes", "No")
              ))
            }
          }
        }

        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
        self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_table)
      }
    },

    # Plot functions
    .showBuildModelPlot = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData)) return(FALSE)

      tryCatch({
        plot(x = plotData,
          cut = self$options$buildModel_plot_cut,
          minimum = self$options$buildModel_plot_min_value,
          edge.label.cex = self$options$buildModel_plot_edge_label_size,
          node.width = self$options$buildModel_plot_node_size,
          label.cex = self$options$buildModel_plot_node_label_size,
          layout = self$options$buildModel_plot_layout
        )
      }, error = function(e) {
        self$results$errorText$setContent(paste0("Plot error: ", e$message))
        self$results$errorText$setVisible(TRUE)
      })
      TRUE
    },

    .showBuildModelHisto = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData) || !self$options$buildModel_show_histo) return(FALSE)
      hist(plotData, main = "Histogram of Edge Weights", xlab = "Edge Weights", ylab = "Frequency")
      TRUE
    },

    .showBuildModelFrequencies = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData) || !self$options$buildModel_show_frequencies) return(FALSE)
      tryCatch({
        p <- tna::plot_frequencies(x = plotData)
        if (!is.null(p)) print(p)
      }, error = function(e) {
        hist(plotData, main = "Frequencies Plot", xlab = "Edge Weights", ylab = "Frequency")
      })
      TRUE
    },

    .showCentralityPlot = function(image, ...) {
      plotData <- self$results$centralityTable$state
      if (is.null(plotData) || !self$options$centrality_show_plot) return(FALSE)
      print(plot(plotData))
      TRUE
    },

    .showCommunityPlot = function(image, ...) {
      plotData <- self$results$community_plot$state
      if (is.null(plotData) || !self$options$community_show_plot) return(FALSE)
      methods <- self$options$community_methods
      plot(x = plotData, method = methods)
      TRUE
    },

    .showCliquesPlot = function(image, ...) {
      plotData <- self$results$cliques_plot$state
      if (is.null(plotData) || !self$options$cliques_show_plot) return(FALSE)
      plot(x = plotData, ask = FALSE,
        cut = self$options$cliques_plot_cut,
        minimum = self$options$cliques_plot_min_value,
        edge.label.cex = self$options$cliques_plot_edge_label_size,
        node.width = self$options$cliques_plot_node_size,
        label.cex = self$options$cliques_plot_node_label_size,
        layout = self$options$cliques_plot_layout
      )
      TRUE
    },

    .showBootstrapPlot = function(image, ...) {
      plotData <- self$results$bootstrap_plot$state
      if (is.null(plotData) || !self$options$bootstrap_show_plot) return(FALSE)
      plot(x = plotData, cut = 0.01)
      TRUE
    }
  )
)
