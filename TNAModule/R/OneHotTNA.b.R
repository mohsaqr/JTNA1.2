# One-Hot Co-occurrence Network Analysis

OneHotTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "OneHotTNAClass",
  inherit = OneHotTNABase,
  private = list(
    .run = function() {
      library("tna")

      # Set instructions content
      self$results$instructions$setContent(
        '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
        <div style="text-align:justify;">
        <ul>
          <li>Data should have <b>one-hot encoded columns</b> (0/1) for each action.</li>
          <li><b>One-Hot Columns</b>: Select binary columns representing actions (required, at least 2). Column names become node labels.</li>
          <li><b>Actor</b>: Column identifying individuals (optional).</li>
          <li><b>Session</b>: Column identifying sessions or time periods (optional).</li>
          <li><b>Group</b>: Column for group comparison (optional). Separate networks will be built for each group.</li>
          <li><b>Window Size</b>: Actions within this many rows are considered co-occurring.</li>
          <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html" target="_blank">TNA Tutorial</a> | <a href="https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html" target="_blank">FTNA</a> | <a href="https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html" target="_blank">Group TNA</a></li>
        </ul>
        </div>
        </div>'
      )

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
      group_col <- self$options$buildModel_variables_group
      window_size <- self$options$buildModel_window

      # Validate window_size to prevent division by zero
      if (is.null(window_size) || window_size < 1) {
        self$results$errorText$setContent("Window size must be at least 1")
        self$results$errorText$setVisible(TRUE)
        return()
      }

      model <- NULL
      is_group_model <- !is.null(group_col)

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

          # Track group variable if provided
          group_vector <- NULL
          if (is_group_model) {
            seq_data$..group_var.. <- as.character(df[[group_col]])
          }

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
            cols_to_agg <- if (is_group_model) c(onehot_cols, "..group_var..") else onehot_cols
            seq_data <- aggregate(
              seq_data[cols_to_agg],
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
            cols_to_agg <- if (is_group_model) c(onehot_cols, "..group_var..") else onehot_cols
            seq_data <- aggregate(
              seq_data[cols_to_agg],
              by = list(..group_id.. = seq_data$..group_id..),
              FUN = function(x) {
                non_na <- na.omit(x)
                if (length(non_na) > 0) non_na[1] else NA
              }
            )
            seq_data$..group_id.. <- NULL
          }

          # Extract group vector and remove from seq_data
          if (is_group_model) {
            group_vector <- seq_data$..group_var..
            seq_data$..group_var.. <- NULL
          }

          # Step 3: Build model
          if (scaling == "noScaling") {
            scaling <- character(0L)
          }

          if (is_group_model) {
            model <- tna::group_model(x = seq_data, group = group_vector, type = "co-occurrence", scaling = scaling)
          } else {
            model <- tna::build_model(x = seq_data, type = "co-occurrence", scaling = scaling)
          }

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
        self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic)
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
            rowValues <- list()
            # Add group column if it's a group model
            if ("group" %in% colnames(cent)) {
              rowValues$group <- as.character(cent[i, "group"])
              rowValues$state <- as.character(cent[i, "state"])
            } else {
              rowValues$group <- ""
              rowValues$state <- rownames(cent)[i]
            }
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
            all_edges <- bs$summary
            # Sort by significance
            all_edges <- all_edges[order(-all_edges$sig, all_edges$p_value), ]

            # Filter for significant only if requested
            if (isTRUE(self$options$bootstrap_table_significant_only)) {
              all_edges <- all_edges[all_edges$sig == TRUE, ]
            }

            # Limit rows unless show all is enabled
            if (!isTRUE(self$options$bootstrap_table_show_all)) {
              max_rows <- self$options$bootstrap_table_max_rows
              if (nrow(all_edges) > max_rows) {
                all_edges <- all_edges[1:max_rows, ]
              }
            }

            for (i in 1:nrow(all_edges)) {
              row <- all_edges[i, ]
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

      ### Permutation Test (only for group models)
      if (!is.null(model) && is_group_model && (isTRUE(self$options$permutation_show_table) || isTRUE(self$options$permutation_show_plot))) {
        permutationTest <- self$results$permutation_plot$state
        if (is.null(permutationTest)) {
          tryCatch({
            permutationTest <- tna::permutation_test(
              x = model,
              iter = self$options$permutation_iter,
              paired = self$options$permutation_paired,
              level = self$options$permutation_level
            )
            self$results$permutation_plot$setState(permutationTest)
          }, error = function(e) {
            self$results$permutationTitle$setVisible(TRUE)
            self$results$permutationTable$setNote(key = "error", note = paste("Error:", e$message))
          })
        }

        # Populate table if we have data
        if (!is.null(permutationTest) && isTRUE(self$options$permutation_show_table)) {
          row_key_counter <- 1
          max_rows <- self$options$permutation_table_max_rows
          show_all <- isTRUE(self$options$permutation_table_show_all)
          for (comparison_name in names(permutationTest)) {
            comparison_data <- permutationTest[[comparison_name]]
            if (!is.null(comparison_data$edges) && !is.null(comparison_data$edges$stats)) {
              stats_df <- comparison_data$edges$stats
              stats_df$diff_true <- as.numeric(stats_df$diff_true)
              stats_df$effect_size <- as.numeric(stats_df$effect_size)
              stats_df$p_value <- as.numeric(stats_df$p_value)
              filtered_sorted_stats <- stats_df[order(stats_df$p_value, -stats_df$diff_true), ]

              for (i in 1:nrow(filtered_sorted_stats)) {
                # Check row limit unless show_all is enabled
                if (!show_all && row_key_counter > max_rows) break
                edge_name_value <- filtered_sorted_stats[i, "edge_name"]
                if (is.null(edge_name_value) || is.na(edge_name_value) || edge_name_value == "") {
                  edge_name_value <- rownames(filtered_sorted_stats)[i]
                  if (is.null(edge_name_value) || edge_name_value == "") {
                    edge_name_value <- paste("Edge", i)
                  }
                } else {
                  edge_name_value <- trimws(as.character(edge_name_value))
                }

                rowValues <- list(
                  group_comparison = as.character(comparison_name),
                  edge_name = edge_name_value,
                  diff_true = as.numeric(filtered_sorted_stats[i, "diff_true"]),
                  effect_size = as.numeric(filtered_sorted_stats[i, "effect_size"]),
                  p_value = as.numeric(filtered_sorted_stats[i, "p_value"])
                )
                self$results$permutationTable$addRow(rowKey = as.character(row_key_counter), values = rowValues)
                row_key_counter <- row_key_counter + 1
              }
              # Break outer loop if row limit reached
              if (!show_all && row_key_counter > max_rows) break
            }
          }
        }

        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationTable$setVisible(self$options$permutation_show_table)
        self$results$permutationTitle$setVisible(isTRUE(self$options$permutation_show_table) || isTRUE(self$options$permutation_show_plot))
      }

      ### Pattern Discovery
      if (isTRUE(self$options$pattern_show_table)) {
        self$results$patternTitle$setVisible(TRUE)

        tryCatch({
          # Get the sequence data from the model building process
          # We need to rebuild seq_data if not available
          df <- as.data.frame(self$data)
          onehot_cols <- self$options$buildModel_variables_onehot
          actor_col <- self$options$buildModel_variables_actor
          session_col <- self$options$buildModel_variables_session
          window_size <- self$options$buildModel_window

          # Create sequence data: replace 1 with column name, 0 with NA
          seq_data <- as.data.frame(lapply(onehot_cols, function(col) {
            ifelse(df[[col]] == 1, col, NA)
          }))
          colnames(seq_data) <- paste0("Action_T", seq_along(onehot_cols))

          # Aggregate by actor + session/window if provided
          if (!is.null(actor_col) || !is.null(session_col)) {
            if (!is.null(actor_col) && !is.null(session_col)) {
              session_numeric <- as.numeric(factor(df[[session_col]]))
              window_id <- floor((session_numeric - 1) / window_size)
              group_id <- paste(df[[actor_col]], window_id, sep = "_")
            } else if (!is.null(actor_col)) {
              row_window <- floor((seq_len(nrow(df)) - 1) / window_size)
              group_id <- paste(df[[actor_col]], row_window, sep = "_")
            } else {
              session_numeric <- as.numeric(factor(df[[session_col]]))
              group_id <- floor((session_numeric - 1) / window_size)
            }

            seq_data$..group_id.. <- group_id
            seq_data <- aggregate(
              seq_data[paste0("Action_T", seq_along(onehot_cols))],
              by = list(..group_id.. = seq_data$..group_id..),
              FUN = function(x) {
                non_na <- na.omit(x)
                if (length(non_na) > 0) non_na[1] else NA
              }
            )
            seq_data$..group_id.. <- NULL
          }

          # Discover patterns using codyna
          pattern_type <- self$options$pattern_type

          # Calculate max possible length/gap based on sequence width
          max_cols <- ncol(seq_data)

          # Adjust ranges to fit data
          len_min <- min(self$options$pattern_len_min, max_cols - 1)
          len_max <- min(self$options$pattern_len_max, max_cols - 1)
          gap_min <- min(self$options$pattern_gap_min, max_cols - 2)
          gap_max <- min(self$options$pattern_gap_max, max_cols - 2)

          # Ensure valid ranges
          if (len_min < 2) len_min <- 2
          if (len_max < len_min) len_max <- len_min
          if (gap_min < 1) gap_min <- 1
          if (gap_max < gap_min) gap_max <- gap_min

          len_range <- len_min:len_max
          gap_range <- gap_min:gap_max

          # All pattern types need both len and gap
          patterns <- codyna::discover_patterns(
            data = seq_data,
            type = pattern_type,
            len = len_range,
            gap = gap_range,
            min_support = self$options$pattern_min_support,
            min_count = self$options$pattern_min_count
          )

          # Populate table with row limit
          if (!is.null(patterns) && nrow(patterns) > 0) {
            total_patterns <- nrow(patterns)

            # Apply row limit unless show_all is checked
            if (!isTRUE(self$options$pattern_table_show_all)) {
              max_rows <- self$options$pattern_table_max_rows
              if (nrow(patterns) > max_rows) {
                patterns <- patterns[1:max_rows, ]
              }
            }

            for (i in 1:nrow(patterns)) {
              self$results$patternTable$addRow(rowKey = i, values = list(
                pattern = as.character(patterns$pattern[i]),
                length = as.integer(patterns$length[i]),
                count = as.integer(patterns$count[i]),
                proportion = as.numeric(patterns$proportion[i]),
                support = as.numeric(patterns$support[i])
              ))
            }

            # Update title with count info
            if (total_patterns > nrow(patterns)) {
              self$results$patternTitle$setContent(
                paste0("Pattern Discovery (showing ", nrow(patterns), " of ", total_patterns, " patterns)")
              )
            } else {
              self$results$patternTitle$setContent(
                paste0("Pattern Discovery (", total_patterns, " patterns found)")
              )
            }
          } else {
            self$results$patternTitle$setContent("Pattern Discovery (no patterns found)")
          }

          self$results$patternTable$setVisible(TRUE)

        }, error = function(e) {
          self$results$patternTitle$setContent(paste("Pattern Discovery Error:", e$message))
        })
      }

      ### Compare Network Properties (only for group models)
      showAnyCompare <- isTRUE(self$options$compare_show_summary) ||
                        isTRUE(self$options$compare_show_network) ||
                        isTRUE(self$options$compare_show_plot)

      if (!is.null(model) && is_group_model && showAnyCompare) {
        # Show instructions
        self$results$compareInstructions$setContent(
          '<div style="border: 2px solid #d4edda; border-radius: 10px; padding: 10px; background-color: #d4edda; margin: 10px 0;">
          <b>Compare Network Properties</b>: Compares general network properties between two groups including correlations, distances, and structural metrics. Select two groups to compare from the dropdowns below.
          </div>'
        )
        self$results$compareInstructions$setVisible(TRUE)
        self$results$compareTitle$setVisible(TRUE)

        tryCatch({
          # Get available groups
          available_groups <- names(model)

          # Get selected groups or use defaults
          group_i_name <- self$options$compare_group_i
          group_j_name <- self$options$compare_group_j

          if (is.null(group_i_name) || group_i_name == "") {
            group_i_name <- available_groups[1]
          }
          if (is.null(group_j_name) || group_j_name == "") {
            if (length(available_groups) >= 2) {
              group_j_name <- available_groups[2]
            } else {
              group_j_name <- available_groups[1]
            }
          }

          # Call tna::compare
          compResult <- tna::compare(
            x = model,
            i = group_i_name,
            j = group_j_name,
            scaling = self$options$compare_scaling,
            network = TRUE
          )

          # Store state for plot
          self$results$compare_plot$setState(list(
            result = compResult,
            group_i = group_i_name,
            group_j = group_j_name
          ))

          # Summary table
          if (isTRUE(self$options$compare_show_summary)) {
            self$results$compareSummaryTable$setTitle(paste("Summary Metrics:", group_i_name, "vs", group_j_name))

            if (!is.null(compResult$correlation)) {
              self$results$compareSummaryTable$addRow(rowKey = 1, values = list(
                metric = "Pearson Correlation",
                value = as.numeric(compResult$correlation)
              ))
            }
            if (!is.null(compResult$cosine_similarity)) {
              self$results$compareSummaryTable$addRow(rowKey = 2, values = list(
                metric = "Cosine Similarity",
                value = as.numeric(compResult$cosine_similarity)
              ))
            }
            if (!is.null(compResult$euclidean_distance)) {
              self$results$compareSummaryTable$addRow(rowKey = 3, values = list(
                metric = "Euclidean Distance",
                value = as.numeric(compResult$euclidean_distance)
              ))
            }
            if (!is.null(compResult$manhattan_distance)) {
              self$results$compareSummaryTable$addRow(rowKey = 4, values = list(
                metric = "Manhattan Distance",
                value = as.numeric(compResult$manhattan_distance)
              ))
            }
            if (!is.null(compResult$frobenius_norm)) {
              self$results$compareSummaryTable$addRow(rowKey = 5, values = list(
                metric = "Frobenius Norm",
                value = as.numeric(compResult$frobenius_norm)
              ))
            }
            if (!is.null(compResult$jaccard_index)) {
              self$results$compareSummaryTable$addRow(rowKey = 6, values = list(
                metric = "Jaccard Index",
                value = as.numeric(compResult$jaccard_index)
              ))
            }
            self$results$compareSummaryTable$setVisible(TRUE)
          }

          # Network properties table
          if (isTRUE(self$options$compare_show_network) && !is.null(compResult$network)) {
            self$results$compareNetworkTable$setTitle(paste("Network Properties:", group_i_name, "vs", group_j_name))

            network_df <- compResult$network
            row_count <- 1
            for (metric_name in rownames(network_df)) {
              self$results$compareNetworkTable$addRow(rowKey = row_count, values = list(
                metric = metric_name,
                group_i = as.numeric(network_df[metric_name, 1]),
                group_j = as.numeric(network_df[metric_name, 2])
              ))
              row_count <- row_count + 1
            }
            self$results$compareNetworkTable$setVisible(TRUE)
          }

          # Show plot
          self$results$compare_plot$setVisible(isTRUE(self$options$compare_show_plot))

        }, error = function(e) {
          self$results$compareInstructions$setContent(
            paste('<div style="border: 2px solid #f8d7da; border-radius: 10px; padding: 10px; background-color: #f8d7da; margin: 10px 0;">',
                  '<b>Error:</b>', e$message, '</div>')
          )
        })
      }

      ### Network Difference Plot (independent section)
      if (!is.null(model) && is_group_model && isTRUE(self$options$compare_show_network_diff_plot)) {
        self$results$compare_network_diff_plot$setVisible(TRUE)
      }
    },

    # Plot functions
    .showBuildModelPlot = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData)) return(FALSE)

      tryCatch({
        # Check if it's a group model (only apply multi-panel layout if group variable is used)
        is_group_model <- !is.null(self$options$buildModel_variables_group)
        if (is_group_model && (inherits(plotData, "group_tna") || (is.list(plotData) && length(plotData) > 1 && !inherits(plotData, "tna")))) {
          n_groups <- length(plotData)
          if (n_groups <= 4) {
            par(mfrow = c(2, 2))
          } else {
            par(mfrow = c(ceiling(sqrt(n_groups)), ceiling(sqrt(n_groups))))
          }
        }
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

    .showBuildModelMosaic = function(image, ...) {
      plotData <- self$results$buildModelContent$state
      if (is.null(plotData) || !self$options$buildModel_show_mosaic) return(FALSE)
      tryCatch({
        p <- tna::plot_mosaic(x = plotData, digits = self$options$buildModel_digits)
        if (!is.null(p)) print(p)
      }, error = function(e) {
        self$results$errorText$setContent(paste0("Mosaic plot error: ", e$message))
        self$results$errorText$setVisible(TRUE)
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
      # Check if it's a group model result (only apply multi-panel layout if group variable is used)
      is_group_model <- !is.null(self$options$buildModel_variables_group)
      if (is_group_model && is.list(plotData) && length(plotData) > 1) {
        n_groups <- length(plotData)
        if (n_groups <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(ceiling(sqrt(n_groups)), ceiling(sqrt(n_groups))))
        }
      }
      plot(x = plotData, method = methods)
      TRUE
    },

    .showCliquesPlot = function(image, ...) {
      plotData <- self$results$cliques_plot$state
      if (is.null(plotData) || !self$options$cliques_show_plot) return(FALSE)
      # Check if it's a group model result (only apply multi-panel layout if group variable is used)
      is_group_model <- !is.null(self$options$buildModel_variables_group)
      if (is_group_model && is.list(plotData) && length(plotData) > 1 && !inherits(plotData, "tna_cliques")) {
        n_groups <- length(plotData)
        if (n_groups <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(ceiling(sqrt(n_groups)), ceiling(sqrt(n_groups))))
        }
      }
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
      # Check if it's a group model result (only apply multi-panel layout if group variable is used)
      is_group_model <- !is.null(self$options$buildModel_variables_group)
      if (is_group_model && is.list(plotData) && length(plotData) > 1) {
        n_groups <- length(plotData)
        if (n_groups <= 4) {
          par(mfrow = c(2, 2))
        } else {
          par(mfrow = c(ceiling(sqrt(n_groups)), ceiling(sqrt(n_groups))))
        }
      }
      plot(x = plotData,
        cut = self$options$bootstrap_plot_cut,
        minimum = self$options$bootstrap_plot_min_value,
        edge.label.cex = self$options$bootstrap_plot_edge_label_size,
        node.width = self$options$bootstrap_plot_node_size,
        label.cex = self$options$bootstrap_plot_node_label_size,
        layout = self$options$bootstrap_plot_layout
      )
      TRUE
    },

    .showPermutationPlot = function(image, ...) {
      plotData <- self$results$permutation_plot$state
      if (is.null(plotData) || !self$options$permutation_show_plot) return(FALSE)

      if (length(plotData) == 1) {
        par(mfrow = c(1, 1))
      } else if (length(plotData) <= 4) {
        par(mfrow = c(2, 2))
      } else if (length(plotData) <= 6) {
        par(mfrow = c(2, 3))
      } else if (length(plotData) <= 9) {
        par(mfrow = c(3, 3))
      } else {
        row <- ceiling(sqrt(length(plotData)))
        column <- ceiling(length(plotData) / row)
        par(mfrow = c(row, column))
      }

      plot(x = plotData)
      TRUE
    },

    .showComparePlot = function(image, ...) {
      if (!isTRUE(self$options$compare_show_plot)) return(FALSE)

      stateData <- self$results$compare_plot$state
      if (is.null(stateData)) return(FALSE)

      tryCatch({
        plotData <- stateData$result
        name_x <- stateData$group_i
        name_y <- stateData$group_j

        p <- plot(
          x = plotData,
          type = self$options$compare_plot_type,
          name_x = name_x,
          name_y = name_y
        )
        print(p)
      }, error = function(e) {
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "Compare Plot Error", sub = e$message)
      })
      TRUE
    },

    .showCompareNetworkDiffPlot = function(image, ...) {
      if (!isTRUE(self$options$compare_show_network_diff_plot)) return(FALSE)

      model <- self$results$buildModelContent$state
      if (is.null(model)) return(FALSE)

      # Set up multi-panel layout for pairwise comparisons
      n_groups <- length(model)
      n_comparisons <- choose(n_groups, 2)

      if (n_comparisons == 1) {
        par(mfrow = c(1, 1))
      } else if (n_comparisons <= 4) {
        par(mfrow = c(2, 2))
      } else if (n_comparisons <= 6) {
        par(mfrow = c(2, 3))
      } else if (n_comparisons <= 9) {
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
        for (i in 1:(n_groups - 1)) {
          for (j in (i + 1):n_groups) {
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
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
             main = "Network Difference Plot Error", sub = e$message)
      })
      TRUE
    }
  )
)
