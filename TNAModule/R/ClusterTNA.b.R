# ClusterTNA - Cluster-based TNA (replicates GroupTNA with automatic clustering)

ClusterTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "ClusterTNAClass",
  inherit = ClusterTNABase,
  private = list(
    .run = function() {
      library("tna")

      # Set instructions content
      self$results$instructions$setContent(
        '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
        <div style="text-align:justify;">
        <ul>
          <li>Data should be in <b>long format</b> with one row per event/action.</li>
          <li><b>Action</b>: Column containing the actions/states/events (required).</li>
          <li><b>Actor</b>: Column identifying individuals (required). Actors will be clustered based on their transition patterns.</li>
          <li><b>Time</b> or <b>Order</b>: For ordering events chronologically (optional).</li>
          <li>Set the <b>Number of Clusters (k)</b> and enable <b>Run Clustering</b> to cluster actors.</li>
          <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html" target="_blank">TNA Clusters Chapter</a> | <a href="https://sonsoles.me/posts/tna-tutorial" target="_blank">Updated Tutorial</a> | <a href="https://sonsoles.me/posts/tna-clustering" target="_blank">Clustering</a></li>
        </ul>
        </div>
        </div>'
      )

      # Set clustering note
      self$results$clusteringNote$setContent(
        '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
        <p style="text-align:center; font-weight: bold; color: #1a5276; font-size: 14px;">
        ⏳ Note: Clustering may take time depending on your data size. Please be patient.
        </p>
        </div>'
      )

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

        # Step 2: Cluster using tna::cluster_sequences()
        seqData <- as.data.frame(prepData$sequence_data)

        # Use tna's cluster_sequences function with user-selected options
        k <- self$options$clustering_k
        dissimilarity <- self$options$clustering_dissimilarity
        method <- self$options$clustering_method

        # Validate that k doesn't exceed number of sequences
        n_sequences <- nrow(seqData)
        if (k > n_sequences) {
          self$results$errorText$setContent(paste("Number of clusters (k =", k, ") cannot exceed number of sequences (", n_sequences, "). Please reduce k."))
          self$results$errorText$setVisible(TRUE)
          return()
        }

        clusters <- tryCatch({
          tna::cluster_sequences(
            data = seqData,
            k = k,
            dissimilarity = dissimilarity,
            method = method
          )
        }, error = function(e) {
          self$results$errorText$setContent(paste("Clustering error:", e$message))
          self$results$errorText$setVisible(TRUE)
          NULL
        })

        if (is.null(clusters)) {
          return()
        }

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
      if(!is.null(model) && (isTRUE(self$options$community_show_plot) || isTRUE(self$options$community_show_table))) {
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

        # Populate communities table
        # Structure: coms$1$assignments, coms$2$assignments, etc.
        if(!is.null(coms) && isTRUE(self$options$community_show_table)) {
            row_key <- 1
            method_names <- NULL

            for(cluster_name in names(coms)) {
                cluster_coms <- coms[[cluster_name]]
                if(!is.null(cluster_coms) && !is.null(cluster_coms$assignments)) {
                    assignments <- cluster_coms$assignments

                    # Add columns for each community detection method (only once)
                    if(is.null(method_names)) {
                        method_names <- colnames(assignments)[colnames(assignments) != "state"]
                        for(method in method_names) {
                            self$results$communityTable$addColumn(name=method, title=method, type="integer")
                        }
                    }

                    # Add rows with community assignments
                    for (i in 1:nrow(assignments)) {
                        rowValues <- list()
                        rowValues$cluster <- as.character(cluster_name)
                        rowValues$state <- as.character(assignments[i, "state"])

                        for(method in method_names) {
                            rowValues[[method]] <- as.integer(assignments[i, method])
                        }

                        self$results$communityTable$addRow(rowKey=row_key, values=rowValues)
                        row_key <- row_key + 1
                    }
                }
            }
        }

        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityTable$setVisible(self$options$community_show_table)
        self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)
      }

      ### Cliques
      if(!is.null(model) && isTRUE(self$options$cliques_show_plot)) {
        cliques_size <- as.numeric(self$options$cliques_size)
        cliques_threshold <- as.numeric(self$options$cliques_threshold)
        if (cliques_threshold == 0) {
            cliques_threshold <- if (cliques_size <= 2) 0.1 else 0.01
        }

        cliques <- self$results$cliques_multiple_plot$state
        if(is.null(cliques)) {
          cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)
          self$results$cliques_multiple_plot$setState(cliques)
        }
        self$results$cliques_multiple_plot$setVisible(TRUE)
        self$results$cliquesTitle$setVisible(TRUE)
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
          max_rows <- self$options$bootstrap_table_max_rows
          show_all <- isTRUE(self$options$bootstrap_table_show_all)
          significant_only <- isTRUE(self$options$bootstrap_table_significant_only)

          for (group_name in names(bs)) {
            group_data <- bs[[group_name]]
            if (!is.null(group_data$summary) && nrow(group_data$summary) > 0) {
              summary_data <- group_data$summary
              # Sort by significance
              summary_data <- summary_data[order(-summary_data$sig, summary_data$p_value), ]

              # Filter for significant only if requested
              if (significant_only) {
                summary_data <- summary_data[summary_data$sig == TRUE, ]
              }

              if (nrow(summary_data) == 0) next

              for (i in 1:nrow(summary_data)) {
                # Check row limit unless show_all is enabled
                if (!show_all && row_key > max_rows) break

                row <- summary_data[i,]
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

              # Break outer loop if row limit reached
              if (!show_all && row_key > max_rows) break
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
          max_rows <- self$options$permutation_table_max_rows
          show_all <- isTRUE(self$options$permutation_table_show_all)
          for (comp_name in names(permTest)) {
            comp_data <- permTest[[comp_name]]
            if (!is.null(comp_data$edges$stats)) {
              stats_df <- comp_data$edges$stats
              # Sort by p_value for consistent ordering
              stats_df <- stats_df[order(stats_df$p_value, -abs(stats_df$diff_true)), ]
              for (i in 1:nrow(stats_df)) {
                # Check row limit unless show_all is enabled
                if (!show_all && row_key > max_rows) break
                self$results$permutationContent$addRow(rowKey=row_key, values=list(
                  group_comparison = comp_name,
                  edge_name = as.character(stats_df[i, "edge_name"]),
                  diff_true = as.numeric(stats_df[i, "diff_true"]),
                  effect_size = as.numeric(stats_df[i, "effect_size"]),
                  p_value = as.numeric(stats_df[i, "p_value"])
                ))
                row_key <- row_key + 1
              }
              # Break outer loop if row limit reached
              if (!show_all && row_key > max_rows) break
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

          # Add rows - extract vectors first to ensure atomic access
          patterns <- as.character(compSeq$pattern)
          statistics <- as.numeric(compSeq$statistic)
          p_values <- as.numeric(compSeq$p_value)

          for(i in 1:nrow(compSeq)) {
            patt <- patterns[i]
            rowValues <- list(
              pattern = patt,
              length = nchar(gsub("[^-]", "", patt)) + 1L
            )
            for(fc in freqCols) {
              rowValues[[fc]] <- as.integer(compSeq[[fc]])[i]
            }
            for(pc in propCols) {
              rowValues[[pc]] <- as.numeric(compSeq[[pc]])[i]
            }
            rowValues$statistic <- statistics[i]
            rowValues$p_value <- p_values[i]
            self$results$compareSequencesTable$addRow(rowKey=i, values=rowValues)
          }
        }

        self$results$compareSequencesTable$setVisible(self$options$compare_sequences_show_table)
      }

      ### Compare Network Properties
      showAnyCompare <- isTRUE(self$options$compare_show_summary) ||
                        isTRUE(self$options$compare_show_network) ||
                        isTRUE(self$options$compare_show_plot)

      if(!is.null(model) && showAnyCompare) {

        # Show instructions
        self$results$compareInstructions$setContent(
          '<div style="border: 2px solid #d4edda; border-radius: 10px; padding: 10px; background-color: #d4edda; margin: 10px 0;">
          <b>Compare Network Properties</b>: Compares general network properties between two clusters, including edge weight correlations, distances, and structural metrics like density and reciprocity.
          </div>'
        )
        self$results$compareInstructions$setVisible(TRUE)

        # Get available cluster names from model
        available_clusters <- names(model)
        num_clusters <- length(available_clusters)

        # Get selected cluster indices
        cluster_i <- self$options$compare_cluster_i
        cluster_j <- self$options$compare_cluster_j

        # Validate cluster indices
        if(cluster_i > num_clusters || cluster_j > num_clusters) {
          self$results$compareTitle$setContent(paste("Invalid cluster index. Available clusters: 1 to", num_clusters))
          self$results$compareTitle$setVisible(TRUE)
        } else if(cluster_i == cluster_j) {
          self$results$compareTitle$setContent("Please select two different clusters to compare")
          self$results$compareTitle$setVisible(TRUE)
        } else {

          cluster_i_name <- available_clusters[cluster_i]
          cluster_j_name <- available_clusters[cluster_j]

          compResult <- self$results$compare_plot$state
          if(is.null(compResult)) {
            tryCatch({
              # Call tna::compare for cluster_tna using cluster indices
              compResult <- tna::compare(
                x = model,
                i = cluster_i,
                j = cluster_j,
                scaling = self$options$compare_scaling,
                network = TRUE
              )

              # Store state along with cluster names
              self$results$compare_plot$setState(list(
                result = compResult,
                cluster_i = cluster_i_name,
                cluster_j = cluster_j_name
              ))
            }, error = function(e) {
              self$results$errorText$setContent(paste("Compare Error:", e$message))
              self$results$errorText$setVisible(TRUE)
            })
          } else {
            # Extract from stored state
            cluster_i_name <- compResult$cluster_i
            cluster_j_name <- compResult$cluster_j
            compResult <- compResult$result
          }

          # Set title with cluster names
          self$results$compareTitle$setContent(paste("Comparing:", cluster_i_name, "vs", cluster_j_name))

          # Summary metrics table
          if(!is.null(compResult) && isTRUE(self$options$compare_show_summary)) {
            if(!is.null(compResult$summary_metrics) && nrow(compResult$summary_metrics) > 0) {
              self$results$compareSummaryTable$setTitle(paste("Summary Metrics:", cluster_i_name, "vs", cluster_j_name))
              for(i in 1:nrow(compResult$summary_metrics)) {
                self$results$compareSummaryTable$addRow(rowKey=i, values=list(
                  metric = as.character(compResult$summary_metrics$metric[i]),
                  value = as.numeric(compResult$summary_metrics$value[i])
                ))
              }
            }
            self$results$compareSummaryTable$setVisible(TRUE)
          }

          # Network properties table
          if(!is.null(compResult) && isTRUE(self$options$compare_show_network)) {
            if(!is.null(compResult$network_metrics) && nrow(compResult$network_metrics) > 0) {
              self$results$compareNetworkTable$setTitle(paste("Network Properties:", cluster_i_name, "vs", cluster_j_name))

              # Update column titles with actual cluster names
              self$results$compareNetworkTable$getColumn("cluster_i")$setTitle(cluster_i_name)
              self$results$compareNetworkTable$getColumn("cluster_j")$setTitle(cluster_j_name)

              for(i in 1:nrow(compResult$network_metrics)) {
                row_data <- compResult$network_metrics[i, ]
                self$results$compareNetworkTable$addRow(rowKey=i, values=list(
                  metric = as.character(row_data$metric),
                  cluster_i = as.numeric(row_data[[2]]),
                  cluster_j = as.numeric(row_data[[3]])
                ))
              }
            }
            self$results$compareNetworkTable$setVisible(TRUE)
          }

          # Set visibility
          self$results$compare_plot$setVisible(self$options$compare_show_plot)
          self$results$compareTitle$setVisible(showAnyCompare)
        }
      }

      ### Network Difference Plot (independent section)
      if (!is.null(model) && isTRUE(self$options$compare_show_network_diff_plot)) {
        self$results$compare_network_diff_plot$setVisible(TRUE)
      }

      ### Sequence Indices
      if(self$options$indices_show_table) {
        self$results$indicesTitle$setContent("Calculating Sequence Indices...")
        self$results$indicesTitle$setVisible(TRUE)

        tryCatch({
          # Prepare data for sequence indices
          action_col <- self$options$buildModel_variables_long_action
          actor_col <- self$options$buildModel_variables_long_actor
          time_col <- self$options$buildModel_variables_long_time
          order_col <- self$options$buildModel_variables_long_order

          # Check action_col is valid
          if(is.null(action_col) || length(action_col) == 0) {
            self$results$indicesTitle$setContent("Error: Action variable is required")
          } else {
            args_prepare_data <- list(
              data = self$data,
              action = action_col
            )
            if(!is.null(actor_col) && length(actor_col) > 0) {
              args_prepare_data$actor <- actor_col
            }
            if(!is.null(time_col) && length(time_col) > 0) {
              args_prepare_data$time <- time_col
            }
            if(!is.null(order_col) && length(order_col) > 0) {
              args_prepare_data$order <- order_col
            }

            dataForIndices <- do.call(tna::prepare_data, args_prepare_data)

            if(is.null(dataForIndices)) {
              self$results$indicesTitle$setContent("Error: Could not prepare sequence data")
            } else {
              seq_data <- dataForIndices$sequence_data

              # Convert all columns to character (codyna requires character data)
              seq_data <- as.data.frame(lapply(seq_data, as.character), stringsAsFactors = FALSE)

              # Call sequence_indices
              fav_state <- self$options$indices_favorable
              has_favorable <- !is.null(fav_state) && length(fav_state) > 0 && !identical(fav_state, "")
              if (has_favorable) {
                indices <- codyna::sequence_indices(
                  data = seq_data,
                  favorable = fav_state,
                  omega = self$options$indices_omega
                )
              } else {
                indices <- codyna::sequence_indices(
                  data = seq_data,
                  omega = self$options$indices_omega
                )
              }

              # Add sequence ID
              indices$sequence_id <- 1:nrow(indices)

              # Add actor if provided
              if(!is.null(actor_col) && length(actor_col) > 0) {
                unique_actors <- unique(self$data[[actor_col]])
                if (length(unique_actors) == nrow(indices)) {
                  indices$actor <- as.character(unique_actors)
                } else {
                  indices$actor <- NA
                }
              } else {
                indices$actor <- NA
              }

              # Add cluster assignments from the model
              if(!is.null(model) && !is.null(model$clusters) && !is.null(model$clusters$assignments)) {
                indices$cluster <- as.integer(model$clusters$assignments)
              } else {
                indices$cluster <- NA
              }

              # Apply row limit
              total_rows <- nrow(indices)
              if (!isTRUE(self$options$indices_table_show_all)) {
                max_rows <- self$options$indices_table_max_rows
                if (total_rows > max_rows) {
                  indices <- indices[1:max_rows, ]
                }
              }

              # Populate the table
              if(nrow(indices) > 0) {
                for(i in 1:nrow(indices)) {
                  self$results$indicesTable$addRow(rowKey=i, values=list(
                    sequence_id = indices$sequence_id[i],
                    actor = if(is.na(indices$actor[i])) "" else as.character(indices$actor[i]),
                    cluster = if(is.na(indices$cluster[i])) NA else as.integer(indices$cluster[i]),
                    valid_n = as.integer(indices$valid_n[i]),
                    unique_states = as.integer(indices$unique_states[i]),
                    longitudinal_entropy = round(indices$longitudinal_entropy[i], 3),
                    simpson_diversity = round(indices$simpson_diversity[i], 3),
                    mean_spell_duration = round(indices$mean_spell_duration[i], 3),
                    self_loop_tendency = round(indices$self_loop_tendency[i], 3),
                    transition_rate = round(indices$transition_rate[i], 3),
                    first_state = as.character(indices$first_state[i]),
                    last_state = as.character(indices$last_state[i]),
                    dominant_state = as.character(indices$dominant_state[i]),
                    complexity_index = round(indices$complexity_index[i], 3)
                  ))
                }

                # Show count in title
                if (nrow(indices) < total_rows) {
                  self$results$indicesTitle$setContent(paste("Showing", nrow(indices), "of", total_rows, "sequences"))
                } else {
                  self$results$indicesTitle$setContent(paste("Sequence Indices for", total_rows, "sequences"))
                }
              }

              self$results$indicesTable$setVisible(TRUE)
            }
          }
        }, error = function(e) {
          self$results$indicesTitle$setContent(paste("Sequence Indices error:", e$message))
        })
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
      tryCatch({
          if(length(plotData) <= 4) {
            par(mfrow = c(2, 2))
          } else {
            par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
          }
          for(i in 1:length(plotData)) {
            group_name <- names(plotData)[i]
            if(is.null(group_name)) group_name <- paste("Cluster", i)
            w <- c(plotData[[i]]$weights)
            brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
            hist(x=plotData[[i]], breaks=brks, main=paste("Histogram -", group_name), xlab="Edge Weights", ylab="Frequency")
          }
      }, error = function(e) { plot(1, type="n", main="Histogram Error", sub=e$message) })
      TRUE
    },

    .showBuildModelFrequencies = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData) || !self$options$buildModel_show_frequencies) return(FALSE)
      tryCatch({
        p <- tna::plot_frequencies(x=plotData)
        if(!is.null(p)) print(p)
      }, error = function(e) {
        w <- c(plotData$weights)
        brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
        hist(x=plotData, breaks=brks, main="Frequencies Plot", xlab="Edge Weights", ylab="Frequency")
      })
      TRUE
    },

    .showBuildModelMosaic = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if(is.null(plotData) || !self$options$buildModel_show_mosaic) return(FALSE)
      tryCatch({
          p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
          print(p)
      }, error = function(e) { plot(1, type="n", main="Mosaic Plot Error", sub=e$message) })
      TRUE
    },

    .showCentralityPlot = function(image, ...) {
      plotData <- self$results$centralityTable$state
      if(is.null(plotData) || !self$options$centrality_show_plot) return(FALSE)
      tryCatch({
          print(plot(plotData))
      }, error = function(e) { plot(1, type="n", main="Centrality Plot Error", sub=e$message) })
      TRUE
    },

    .showCommunityPlot = function(image, ...) {
      plotData <- self$results$community_plot$state
      if(is.null(plotData) || !self$options$community_show_plot) return(FALSE)
      tryCatch({
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
          plot(plotData)
      }, error = function(e) { plot(1, type="n", main="Community Plot Error", sub=e$message) })
      TRUE
    },

    .showCliquesMultiPlot = function(image, ...) {
      plotData <- self$results$cliques_multiple_plot$state
      if (!is.null(plotData) && self$options$cliques_show_plot) {
        tryCatch({
            n <- lengths(plotData[1])
            if (n == 0) return(FALSE)
            nc <- ceiling(sqrt(n))
            nr <- ceiling(n / nc)
            par(mfrow = c(nr, nc))
            for (i in seq_len(n)) {
                plot(x=plotData, ask=FALSE, first=i, n=1,
                    cut=self$options$cliques_plot_cut,
                    minimum=self$options$cliques_plot_min_value,
                    edge.label.cex=self$options$cliques_plot_edge_label_size,
                    node.width=self$options$cliques_plot_node_size,
                    label.cex=self$options$cliques_plot_node_label_size,
                    layout=self$options$cliques_plot_layout)
            }
        }, error = function(e) { plot(1, type="n", main="Cliques Plot Error", sub=e$message) })
        TRUE
      } else {
        FALSE
      }
    },

    .showBootstrapPlot = function(image, ...) {
      plotData <- self$results$bootstrap_plot$state
      if(is.null(plotData) || !self$options$bootstrap_show_plot) return(FALSE)
      tryCatch({
          if(length(plotData) <= 4) {
            par(mfrow = c(2, 2))
          } else {
            par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
          }
          plot(x=plotData, cut=0.1)
      }, error = function(e) { plot(1, type="n", main="Bootstrap Plot Error", sub=e$message) })
      TRUE
    },

    .showPermutationPlot = function(image, ...) {
      plotData <- self$results$permutation_plot$state
      if(is.null(plotData) || !self$options$permutation_show_plot) return(FALSE)
      tryCatch({
          if(length(plotData) <= 4) {
            par(mfrow = c(2, 2))
          } else {
            par(mfrow = c(ceiling(sqrt(length(plotData))), ceiling(sqrt(length(plotData)))))
          }
          plot(x=plotData)
      }, error = function(e) { plot(1, type="n", main="Permutation Plot Error", sub=e$message) })
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
    },
    .showComparePlot = function(image, ...) {
      if(!self$options$compare_show_plot) return(FALSE)
      stateData <- self$results$compare_plot$state
      if(is.null(stateData)) return(FALSE)

      tryCatch({
        # Extract comparison result and cluster names from state
        plotData <- stateData$result
        name_x <- stateData$cluster_i
        name_y <- stateData$cluster_j

        if(is.null(plotData)) return(FALSE)

        p <- plot(
          x = plotData,
          type = self$options$compare_plot_type,
          name_x = name_x,
          name_y = name_y
        )
        print(p)
      }, error = function(e) {
        plot(1, type="n", main="Compare Plot Error", sub=e$message)
      })
      TRUE
    },
    .showCompareNetworkDiffPlot = function(image, ...) {
      if(!isTRUE(self$options$compare_show_network_diff_plot)) return(FALSE)

      model <- self$results$buildModelContent$state
      if(is.null(model)) return(FALSE)

      # Set up multi-panel layout for pairwise comparisons
      n_groups <- length(model)
      n_comparisons <- choose(n_groups, 2)

      if(n_comparisons == 1) {
        par(mfrow = c(1, 1))
      } else if(n_comparisons <= 4) {
        par(mfrow = c(2, 2))
      } else if(n_comparisons <= 6) {
        par(mfrow = c(2, 3))
      } else if(n_comparisons <= 9) {
        par(mfrow = c(3, 3))
      } else {
        rows <- ceiling(sqrt(n_comparisons))
        cols <- ceiling(n_comparisons / rows)
        par(mfrow = c(rows, cols))
      }

      tryCatch({
        # Get group names for labels
        group_names <- names(model)

        # Loop through all pairwise comparisons
        for(i in 1:(n_groups - 1)) {
          for(j in (i + 1):n_groups) {
            plot_title <- paste(group_names[i], "vs", group_names[j])
            tna::plot_compare(
              x = model,
              i = i,
              j = j,
              cut = self$options$compare_network_diff_plot_cut,
              minimum = self$options$compare_network_diff_plot_min_value,
              edge.label.cex = self$options$compare_network_diff_plot_edge_label_size,
              node.width = self$options$compare_network_diff_plot_node_size,
              label.cex = self$options$compare_network_diff_plot_node_label_size,
              layout = self$options$compare_network_diff_plot_layout,
              title = plot_title
            )
          }
        }
      }, error = function(e) {
        plot(1, type="n", axes=FALSE, xlab="", ylab="",
             main="Network Difference Plot Error", sub=e$message)
      })
      TRUE
    }
  )
)
