# ClusterTNA - Cluster-based TNA (replicates GroupTNA with automatic clustering)

ClusterTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ClusterTNAClass",
  inherit = ClusterTNABase,
  private = list(
    .run = function() {
      library("tna")

      # Check required variables
      if(is.null(self$options$buildModel_variables_long_action) ||
         is.null(self$options$buildModel_variables_long_actor)) {
        return()
      }

      # Check if user wants to run
      if(!isTRUE(self$options$clustering_run)) {
        self$results$errorText$setContent("Check 'Run Clustering Analysis' to start.")
        self$results$errorText$setVisible(TRUE)
        return()
      }

      # Check if model already exists in state (avoid recomputing)
      model <- self$results$buildModelContent$state

      if(is.null(model)) {
        # Only run clustering if model doesn't exist

        # Convert factors to character to preserve labels
        df <- data.frame(
          actor = as.character(self$data[[self$options$buildModel_variables_long_actor]]),
          action = as.character(self$data[[self$options$buildModel_variables_long_action]]),
          stringsAsFactors = FALSE
        )

        # Add time if provided
        if(!is.null(self$options$buildModel_variables_long_time)) {
          df$time <- self$data[[self$options$buildModel_variables_long_time]]
        }

        # Step 1: Prepare data
        args <- list(data = df, actor = "actor", action = "action")
        if(!is.null(self$options$buildModel_variables_long_time)) {
          args$time <- "time"
        }

        prepData <- suppressMessages(suppressWarnings(
          do.call(tna::prepare_data, args)
        ))

        # Step 2: Cluster using PAM (stringdist crashes in jamovi)
        seqData <- as.data.frame(prepData$sequence_data)

        # For distance calculation only, replace NA with placeholder
        seqDataForDist <- seqData
        seqDataForDist[is.na(seqDataForDist)] <- "__NA__"

        # Convert to numeric matrix for distance
        seqMatrix <- as.matrix(seqDataForDist)
        seqNumeric <- apply(seqMatrix, 2, function(x) as.numeric(as.factor(x)))

        # Use fast Manhattan distance (base R, compiled C code)
        distMatrix <- dist(seqNumeric, method = "manhattan")

        # Cluster with PAM
        k <- self$options$clustering_k
        pamResult <- cluster::pam(distMatrix, k = k)
        assignments <- pamResult$clustering
        sizes <- as.integer(table(assignments))

        # Create tna_clustering object for group_model
        # Use original seqData (with NAs intact, not "*")
        clusters <- list(
          data = seqData,
          k = k,
          assignments = as.integer(assignments),
          silhouette = pamResult$silinfo$avg.width,
          sizes = sizes,
          method = "pam",
          distance = distMatrix
        )
        class(clusters) <- "tna_clustering"

        # Step 3: Build group model
        model <- suppressMessages(suppressWarnings(
          tna::group_model(clusters, type = "relative")
        ))

        # Store model in state
        self$results$buildModelContent$setContent(model)
        self$results$buildModelContent$setState(model)
      }

      # Show clustering info (uses cached model)
      self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)
      self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot)
      self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo)
      self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies)
      self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic)

      ### Centrality
      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {
        centrality_loops <- self$options$centrality_loops
        centrality_normalize <- self$options$centrality_normalize

        vectorCharacter <- character(0)
        if(self$options$centrality_OutStrength) vectorCharacter <- append(vectorCharacter, "OutStrength")
        if(self$options$centrality_InStrength) vectorCharacter <- append(vectorCharacter, "InStrength")
        if(self$options$centrality_ClosenessIn) vectorCharacter <- append(vectorCharacter, "ClosenessIn")
        if(self$options$centrality_ClosenessOut) vectorCharacter <- append(vectorCharacter, "ClosenessOut")
        if(self$options$centrality_Closeness) vectorCharacter <- append(vectorCharacter, "Closeness")
        if(self$options$centrality_Betweenness) vectorCharacter <- append(vectorCharacter, "Betweenness")
        if(self$options$centrality_BetweennessRSP) vectorCharacter <- append(vectorCharacter, "BetweennessRSP")
        if(self$options$centrality_Diffusion) vectorCharacter <- append(vectorCharacter, "Diffusion")
        if(self$options$centrality_Clustering) vectorCharacter <- append(vectorCharacter, "Clustering")

        cent <- self$results$centralityTable$state

        if(length(vectorCharacter) > 0 && is.null(cent)) {
          tryCatch({
            cent <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
            self$results$centralityTable$setState(cent)
          }, error = function(e) {
            self$results$centralityTable$setNote(key = "error", note = paste("Error:", e$message))
          })
        }

        # Add columns
        self$results$centralityTable$addColumn(name="group", type="text")
        self$results$centralityTable$addColumn(name="state", type="text")
        for(measure in vectorCharacter) {
          self$results$centralityTable$addColumn(name=measure, type="number")
        }

        # Populate table
        if(!is.null(cent) && is.data.frame(cent) && nrow(cent) > 0) {
          for (i in 1:nrow(cent)) {
            rowValues <- list(group = as.character(cent[i, "group"]), state = as.character(cent[i, "state"]))
            for(measure in vectorCharacter) {
              if(measure %in% colnames(cent)) rowValues[[measure]] <- as.numeric(cent[i, measure])
            }
            self$results$centralityTable$addRow(rowKey=i, values=rowValues)
          }
        }

        self$results$centralityTitle$setVisible(self$options$centrality_show_table || self$options$centrality_show_plot)
        self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
        self$results$centralityTable$setVisible(self$options$centrality_show_table)
      }

      ### Community
      if(!is.null(model) && isTRUE(self$options$community_show_plot)) {
        community_gamma <- as.numeric(self$options$community_gamma)
        methods <- self$options$community_methods

        coms <- self$results$community_plot$state
        if(is.null(coms)) {
          tryCatch({
            coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
            self$results$community_plot$setState(coms)
          }, error = function(e) {
            self$results$communityErrorText$setContent(paste("Error:", e$message))
            self$results$communityErrorText$setVisible(TRUE)
          })
        }

        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityTitle$setVisible(self$options$community_show_plot)
      }

      ### Cliques
      if(!is.null(model) && (isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot))) {
        cliques_size <- as.numeric(self$options$cliques_size)
        cliques_threshold <- as.numeric(self$options$cliques_threshold)

        cliques <- self$results$cliques_multiple_plot$state
        if(is.null(cliques)) {
          cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)
          self$results$cliques_multiple_plot$setState(cliques)
          if(isTRUE(self$options$cliques_show_text)) {
            self$results$cliquesContent$setContent(cliques)
          }
        }

        self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
        self$results$cliquesContent$setVisible(self$options$cliques_show_text)
        self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)
      }

      ### Bootstrap
      if(!is.null(model) && (isTRUE(self$options$bootstrap_show_table) || isTRUE(self$options$bootstrap_show_plot))) {
        bs <- self$results$bootstrap_plot$state
        if(is.null(bs)) {
          bs <- tna::bootstrap(
            x=model,
            iter=self$options$bootstrap_iteration,
            level=self$options$bootstrap_level,
            method=self$options$bootstrap_method,
            threshold=self$options$bootstrap_threshold,
            consistency_range=c(self$options$bootstrap_range_low, self$options$bootstrap_range_up)
          )
          self$results$bootstrap_plot$setState(bs)
        }

        # Populate bootstrap table
        if(!is.null(bs) && isTRUE(self$options$bootstrap_show_table)) {
          row_key <- 1
          for (group_name in names(bs)) {
            group_data <- bs[[group_name]]
            if (!is.null(group_data$summary) && nrow(group_data$summary) > 0) {
              for (i in 1:nrow(group_data$summary)) {
                row <- group_data$summary[i,]
                self$results$bootstrapTable$addRow(rowKey=row_key, values=list(
                  group = group_name,
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
                row_key <- row_key + 1
              }
            }
          }
        }

        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
        self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_table)
      }

      ### Permutation
      if(!is.null(model) && (isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))) {
        permTest <- self$results$permutation_plot$state
        if(is.null(permTest)) {
          permTest <- tna::permutation_test(
            x=model,
            iter=self$options$permutation_iter,
            paired=self$options$permutation_paired,
            level=self$options$permutation_level
          )
          self$results$permutation_plot$setState(permTest)
        }

        # Populate permutation table
        if(!is.null(permTest) && isTRUE(self$options$permutation_show_text)) {
          row_key <- 1
          for (comp_name in names(permTest)) {
            comp_data <- permTest[[comp_name]]
            if (!is.null(comp_data$edges$stats)) {
              stats_df <- comp_data$edges$stats
              for (i in 1:nrow(stats_df)) {
                self$results$permutationContent$addRow(rowKey=row_key, values=list(
                  group_comparison = comp_name,
                  edge_name = as.character(stats_df[i, "edge_name"]),
                  diff_true = as.numeric(stats_df[i, "diff_true"]),
                  effect_size = as.numeric(stats_df[i, "effect_size"]),
                  p_value = as.numeric(stats_df[i, "p_value"])
                ))
                row_key <- row_key + 1
              }
            }
          }
        }

        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationContent$setVisible(self$options$permutation_show_text)
        self$results$permutationTitle$setVisible(self$options$permutation_show_text || self$options$permutation_show_plot)
      }

      ### Sequences
      if(isTRUE(self$options$sequences_show_plot)) {
        self$results$sequences_plot$setVisible(TRUE)
      }

      ### Compare Sequences
      if(!is.null(model) && (isTRUE(self$options$compare_sequences_show_table) || isTRUE(self$options$compare_sequences_show_plot))) {
        compSeq <- self$results$compareSequences_plot$state
        if(is.null(compSeq)) {
          tryCatch({
            sub_range <- self$options$compare_sequences_sub_min:self$options$compare_sequences_sub_max
            compSeq <- tna::compare_sequences(
              x = model,
              sub = sub_range,
              min_freq = self$options$compare_sequences_min_freq,
              correction = self$options$compare_sequences_correction
            )

            # Remove patterns containing * (NAs)
            compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

            # Sort by total frequency (sum of all freq_ columns)
            freqCols <- colnames(compSeq)[grep("^freq_", colnames(compSeq))]
            compSeq$total_freq <- rowSums(compSeq[, freqCols, drop=FALSE])
            compSeq <- compSeq[order(-compSeq$total_freq), ]

            # Keep only top 20
            if(nrow(compSeq) > 20) {
              compSeq <- compSeq[1:20, ]
            }
            compSeq$total_freq <- NULL

            self$results$compareSequences_plot$setState(compSeq)
          }, error = function(e) {
            self$results$errorText$setContent(paste("Compare Sequences Error:", e$message))
            self$results$errorText$setVisible(TRUE)
          })
        }

        # Show plot first, then table
        self$results$compareSequences_plot$setVisible(self$options$compare_sequences_show_plot)
        self$results$compareSequencesTitle$setVisible(self$options$compare_sequences_show_table || self$options$compare_sequences_show_plot)

        # Populate table
        if(!is.null(compSeq) && isTRUE(self$options$compare_sequences_show_table)) {
          # Get column names dynamically (freq_X, prop_X for each cluster)
          colNames <- colnames(compSeq)
          freqCols <- colNames[grep("^freq_", colNames)]
          propCols <- colNames[grep("^prop_", colNames)]

          # Add dynamic columns
          for(fc in freqCols) {
            self$results$compareSequencesTable$addColumn(name=fc, title=fc, type="integer")
          }
          for(pc in propCols) {
            self$results$compareSequencesTable$addColumn(name=pc, title=pc, type="number")
          }
          self$results$compareSequencesTable$addColumn(name="statistic", title="Statistic", type="number")
          self$results$compareSequencesTable$addColumn(name="p_value", title="p-value", type="number")

          # Add rows
          for(i in 1:nrow(compSeq)) {
            rowValues <- list(
              pattern = as.character(compSeq[i, "pattern"]),
              length = nchar(gsub("[^-]", "", as.character(compSeq[i, "pattern"]))) + 1
            )
            for(fc in freqCols) {
              rowValues[[fc]] <- as.integer(compSeq[i, fc])
            }
            for(pc in propCols) {
              rowValues[[pc]] <- as.numeric(compSeq[i, pc])
            }
            rowValues$statistic <- as.numeric(compSeq[i, "statistic"])
            rowValues$p_value <- as.numeric(compSeq[i, "p_value"])
            self$results$compareSequencesTable$addRow(rowKey=i, values=rowValues)
          }
        }

        self$results$compareSequencesTable$setVisible(self$options$compare_sequences_show_table)
      }
    },

    # Plot functions
    .showBuildModelPlot = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData)) return(FALSE)

      if(length(plotData) == 1) {
        par(mfrow = c(1, 1))
      } else if(length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else if(length(plotData) <= 6) {
        par(mfrow = c(2, 3))
      } else if(length(plotData) <= 9) {
        par(mfrow = c(3, 3))
      } else {
        row <- ceiling(sqrt(length(plotData)))
        column <- ceiling(length(plotData) / row)
        par(mfrow = c(row, column))
      }

      tryCatch({
        plot(x=plotData,
          cut=0.1,
          minimum=self$options$buildModel_plot_min_value,
          edge.label.cex=self$options$buildModel_plot_edge_label_size,
          node.width=self$options$buildModel_plot_node_size,
          label.cex=self$options$buildModel_plot_node_label_size,
          layout=self$options$buildModel_plot_layout
        )
      }, error = function(e) {
        self$results$errorText$setContent(paste0("Plot error: ", e$message))
        self$results$errorText$setVisible(TRUE)
      })
      TRUE
    },

    .showBuildModelHisto = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData) || !self$options$buildModel_show_histo) return(FALSE)

      if(length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else {
        par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
      }

      for(i in 1:length(plotData)) {
        group_name <- names(plotData)[i]
        if(is.null(group_name)) group_name <- paste("Cluster", i)
        hist(x=plotData[[i]], main=paste("Histogram -", group_name), xlab="Edge Weights", ylab="Frequency")
      }
      TRUE
    },

    .showBuildModelFrequencies = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData) || !self$options$buildModel_show_frequencies) return(FALSE)
      tryCatch({
        p <- tna::plot_frequencies(x=plotData)
        if(!is.null(p)) print(p)
      }, error = function(e) {
        hist(x=plotData, main="Frequencies Plot", xlab="Edge Weights", ylab="Frequency")
      })
      TRUE
    },

    .showBuildModelMosaic = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData) || !self$options$buildModel_show_mosaic) return(FALSE)
      p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
      print(p)
      TRUE
    },

    .showCentralityPlot = function(image, ...) {
      plotData <- self$results$centralityTable$state
      if(is.null(plotData) || !self$options$centrality_show_plot) return(FALSE)
      print(plot(plotData))
      TRUE
    },

    .showCommunityPlot = function(image, ...) {
      plotData <- self$results$community_plot$state
      if(is.null(plotData) || !self$options$community_show_plot) return(FALSE)

      if(length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else {
        par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
      }
      plot(x=plotData, method=self$options$community_methods)
      TRUE
    },

    .showCliquesPlot1 = function(image, ...) { private$.showCliquesPlotN(1) },
    .showCliquesPlot2 = function(image, ...) { private$.showCliquesPlotN(2) },
    .showCliquesPlot3 = function(image, ...) { private$.showCliquesPlotN(3) },
    .showCliquesPlot4 = function(image, ...) { private$.showCliquesPlotN(4) },
    .showCliquesPlot5 = function(image, ...) { private$.showCliquesPlotN(5) },
    .showCliquesPlot6 = function(image, ...) { private$.showCliquesPlotN(6) },

    .showCliquesPlotN = function(n) {
      plotData <- self$results$cliques_multiple_plot$state
      if(is.null(plotData) || !self$options$cliques_show_plot) return(FALSE)
      if(lengths(plotData[1]) < n) {
        self$results$cliques_multiple_plot[[paste0("cliques_plot", n)]]$setVisible(FALSE)
        return(FALSE)
      }

      len <- length(plotData)
      par(mfrow = c(ceiling(sqrt(len)), ceiling(sqrt(len))))
      plot(x=plotData, ask=FALSE, first=n, n=1,
        cut=self$options$cliques_plot_cut,
        minimum=self$options$cliques_plot_min_value,
        edge.label.cex=self$options$cliques_plot_edge_label_size,
        node.width=self$options$cliques_plot_node_size,
        label.cex=self$options$cliques_plot_node_label_size,
        layout=self$options$cliques_plot_layout
      )
      TRUE
    },

    .showBootstrapPlot = function(image, ...) {
      plotData <- self$results$bootstrap_plot$state
      if(is.null(plotData) || !self$options$bootstrap_show_plot) return(FALSE)

      if(length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else {
        par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
      }
      plot(x=plotData, cut=0.01)
      TRUE
    },

    .showPermutationPlot = function(image, ...) {
      plotData <- self$results$permutation_plot$state
      if(is.null(plotData) || !self$options$permutation_show_plot) return(FALSE)

      if(length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else {
        par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
      }
      plot(x=plotData)
      TRUE
    },

    .showSequencesPlot = function(image, ...) {
      if(!self$options$sequences_show_plot) return(FALSE)
      tna_data <- self$results$buildModelContent$state
      if(is.null(tna_data)) return(FALSE)

      tryCatch({
        p <- tna::plot_sequences(
          x = tna_data,
          type = self$options$sequences_type,
          scale = self$options$sequences_scale,
          geom = self$options$sequences_geom,
          include_na = self$options$sequences_include_na,
          tick = self$options$sequences_tick
        )
        print(p)
      }, error = function(e) {
        plot(1, type="n", main="Sequence Error", sub=e$message)
      })
      TRUE
    },

    .showCompareSequencesPlot = function(image, ...) {
      if(!self$options$compare_sequences_show_plot) return(FALSE)
      plotData <- self$results$compareSequences_plot$state
      if(is.null(plotData)) return(FALSE)

      tryCatch({
        p <- plot(plotData)
        # Make text larger
        p <- p + ggplot2::theme(
          text = ggplot2::element_text(size = 14),
          axis.text = ggplot2::element_text(size = 12),
          axis.title = ggplot2::element_text(size = 14),
          legend.text = ggplot2::element_text(size = 12),
          legend.title = ggplot2::element_text(size = 14),
          strip.text = ggplot2::element_text(size = 12)
        )
        print(p)
      }, error = function(e) {
        plot(1, type="n", main="Compare Sequences Error", sub=e$message)
      })
      TRUE
    }
  )
)
