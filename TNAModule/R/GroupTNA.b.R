# This file is a generated template, your changes will not be overwritten

GroupTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "GroupTNAClass",
  inherit = GroupTNABase,
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
          <li><b>Actor</b>: Column identifying individuals (required).</li>
          <li><b>Group</b>: Column defining groups for comparison (required). Separate networks will be built for each group.</li>
          <li><b>Time</b> or <b>Order</b>: For ordering events chronologically (optional).</li>
          <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html" target="_blank">Group TNA Chapter</a> | <a href="https://sonsoles.me/posts/tna-tutorial" target="_blank">Updated Tutorial</a> | <a href="https://sonsoles.me/posts/tna-group" target="_blank">Group TNA Tutorial</a></li>
        </ul>
        </div>
        </div>'
      )

      type <- self$options$buildModel_type
      scaling <- self$options$buildModel_scaling

      ### Build Model

      # Check if required variables are provided, if not, hide error and return early
      if(is.null(self$options$buildModel_variables_long_action) || 
         is.null(self$options$buildModel_variables_long_actor) ||
         is.null(self$options$buildModel_variables_long_group)) {
        self$results$errorText$setVisible(FALSE)
        return()
      }

      model <- NULL

      if(!isTRUE(self$options$buildModel_show_matrix) &&
        !isTRUE(self$options$buildModel_show_plot) &&
        !isTRUE(self$options$buildModel_show_histo) &&
        (
          !isTRUE(self$options$buildModel_show_mosaic) ||
          (
            isTRUE(self$options$buildModel_show_mosaic) && 
            self$options$buildModel_type == "relative"
          )
        )
      )
      {
        self$results$buildModelTitle$setVisible(FALSE)
      }

      if(self$results$buildModelContent$isFilled()) {
        model <- self$results$buildModelContent$state
      }
      else if(!is.null(self$data) && ncol(self$data) >= 1) {

        # Wrap data preparation in error handling
        tryCatch({
          dataForTNA <- NULL

          copyData <- self$data
          copyData[[self$options$buildModel_variables_long_action]] <- as.character(copyData[[self$options$buildModel_variables_long_action]])

          if(!is.null(self$options$buildModel_variables_long_time)) {
            copyData[[self$options$buildModel_variables_long_time]] <- as.POSIXct(copyData[[self$options$buildModel_variables_long_time]])
          }
          if(!is.null(self$options$buildModel_variables_long_actor)) {
            copyData[[self$options$buildModel_variables_long_actor]] <- as.character(copyData[[self$options$buildModel_variables_long_actor]])
          }
          if(!is.null(self$options$buildModel_variables_long_order)) {
            copyData[[self$options$buildModel_variables_long_order]] <- as.character(copyData[[self$options$buildModel_variables_long_order]])
          }

          
          threshold <- self$options$buildModel_threshold

          columnToUseLong <- c(
            self$options$buildModel_variables_long_time,
            self$options$buildModel_variables_long_actor,
            self$options$buildModel_variables_long_action,
            self$options$buildModel_variables_long_group,
            self$options$buildModel_variables_long_order
          )

          longData <- copyData[columnToUseLong]

          if(ncol(longData) > 0) {
            actorColumn <- self$options$buildModel_variables_long_actor
            timeColumn <- self$options$buildModel_variables_long_time
            actionColumn <- self$options$buildModel_variables_long_action
            groupColumn <- self$options$buildModel_variables_long_group
            orderColumn <- self$options$buildModel_variables_long_order


            args_prepare_data <- list(
                data = longData,
                actor = actorColumn,
                time = timeColumn,
                action = actionColumn,
                time_threshold = threshold,
                order = orderColumn
            ) 

            args_prepare_data <- args_prepare_data[!sapply(args_prepare_data, is.null)]

            dataForTNA <- do.call(tna::prepare_data, args_prepare_data)
          }

          if(!is.null(dataForTNA)) {

              if(scaling == "noScaling") {
                  scaling = character(0L)
              }

              group <- dataForTNA$long_data[!duplicated(dataForTNA$long_data$.session_id),]

              if(type == "attention") {
                  lambda <- self$options$buildModel_lambda
                  model <- tna::group_model(x=dataForTNA, group=group[[groupColumn]], type=type, scaling=scaling, lambda=lambda)
              } else {
                  model <- tna::group_model(x=dataForTNA, group=group[[groupColumn]], type=type, scaling=scaling)
              }
          }
          
        }, error = function(e) {
          # Check if error is related to time format or contains time-related keywords
          error_msg <- tolower(as.character(e$message))
          if(grepl("time|date|posix|format", error_msg) || 
             grepl("character string is not in a standard unambiguous format", error_msg)) {
              self$results$errorText$setContent("Please enter an appropriate time format")
          } else {
              self$results$errorText$setContent(paste("Data preparation error:", e$message))
          }
          self$results$errorText$setVisible(TRUE)
          return()
        })
      }

      if(!is.null(model)) { 
          
          if(!self$results$buildModelContent$isFilled()) {
              self$results$buildModelContent$setContent(model)
              self$results$buildModelContent$setState(model)
          }
          self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)

          # model
          self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot) # plot
          

          # # histo
          self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo) # plot
          
          # # frequencies
          self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies) # plot
          

          # # mosaic
          self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic ) # plot

      }

      ### Centrality

      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {
          centrality_loops <- self$options$centrality_loops
          centrality_normalize <- self$options$centrality_normalize

          # Build vectorCharacter based on selected options (always needed)
          vectorCharacter <- character(0)
          if(self$options$centrality_OutStrength) {
              vectorCharacter <- append(vectorCharacter, "OutStrength")
          }
          if(self$options$centrality_InStrength) {
              vectorCharacter <- append(vectorCharacter, "InStrength")
          }
          if(self$options$centrality_ClosenessIn) {
              vectorCharacter <- append(vectorCharacter, "ClosenessIn")
          }
          if(self$options$centrality_ClosenessOut) {
              vectorCharacter <- append(vectorCharacter, "ClosenessOut")
          }
          if(self$options$centrality_Closeness) {
              vectorCharacter <- append(vectorCharacter, "Closeness")
          }
          if(self$options$centrality_Betweenness) {
              vectorCharacter <- append(vectorCharacter, "Betweenness")
          }
          if(self$options$centrality_BetweennessRSP) {
              vectorCharacter <- append(vectorCharacter, "BetweennessRSP")
          }
          if(self$options$centrality_Diffusion) {
              vectorCharacter <- append(vectorCharacter, "Diffusion")
          }
          if(self$options$centrality_Clustering) {
              vectorCharacter <- append(vectorCharacter, "Clustering")
          }

          # Use state to store centrality results and avoid recalculation, but allow recalculation when needed
          cent <- self$results$centralityTable$state
          
          if(length(vectorCharacter) > 0 && !is.null(model)) {
              # Only calculate if not already filled or if explicitly requested
              if(is.null(cent) || !self$results$centralityTable$isFilled()) {
                  tryCatch({
                      cent_result <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
                      
                      # Validate the result before storing
                      if(!is.null(cent_result) && is.data.frame(cent_result)) {
                          cent <- cent_result
                          self$results$centralityTable$setState(cent)
                      } else {
                          self$results$centralityTable$setNote(key = "centrality_error", note = "Invalid centrality calculation result")
                      }
                  }, error = function(e) {
                      # Add error handling for centrality calculation
                      self$results$centralityTable$setNote(key = "centrality_error", note = paste("Centrality calculation error:", e$message))
                      cent <- NULL
                  })
              }
          }

          # Add columns if needed (jamovi will handle duplicates)
          self$results$centralityTable$addColumn(name="group", type="text")
          self$results$centralityTable$addColumn(name="state", type="text")

          if(self$options$centrality_OutStrength) {
              self$results$centralityTable$addColumn(name="OutStrength", type="number")
          }
          if(self$options$centrality_InStrength) {
              self$results$centralityTable$addColumn(name="InStrength", type="number")
          }
          if(self$options$centrality_ClosenessIn) {
              self$results$centralityTable$addColumn(name="ClosenessIn", type="number")
          }
          if(self$options$centrality_ClosenessOut) {
              self$results$centralityTable$addColumn(name="ClosenessOut", type="number")
          }
          if(self$options$centrality_Closeness) {
              self$results$centralityTable$addColumn(name="Closeness", type="number")
          }
          if(self$options$centrality_Betweenness) {
              self$results$centralityTable$addColumn(name="Betweenness", type="integer")
          }
          if(self$options$centrality_BetweennessRSP) {
              self$results$centralityTable$addColumn(name="BetweennessRSP", type="number")
          }
          if(self$options$centrality_Diffusion) {
              self$results$centralityTable$addColumn(name="Diffusion", type="number")
          }
          if(self$options$centrality_Clustering) {
              self$results$centralityTable$addColumn(name="Clustering", type="number")
          }

          # Populate table if we have data (either from state or newly calculated)
          if(!is.null(cent) && length(vectorCharacter) > 0) {
              # Handle Group TNA centrality data structure
              row_count <- 1
              
              # GroupTNA centralities returns a data frame with columns: group, state, measures...
              if(is.data.frame(cent) && !is.null(cent) && !is.na(nrow(cent)) && nrow(cent) > 0) {
                  for (i in 1:nrow(cent)) {
                      rowValues <- list()
                      
                      # Extract values directly from data frame columns
                      rowValues$group <- as.character(cent[i, "group"])
                      rowValues$state <- as.character(cent[i, "state"])

                      if ("OutStrength" %in% vectorCharacter && "OutStrength" %in% colnames(cent)) {
                          rowValues$OutStrength <- as.numeric(cent[i, "OutStrength"])
                      }
                      if ("InStrength" %in% vectorCharacter && "InStrength" %in% colnames(cent)) {
                          rowValues$InStrength <- as.numeric(cent[i, "InStrength"])
                      }
                      if ("ClosenessIn" %in% vectorCharacter && "ClosenessIn" %in% colnames(cent)) {
                          rowValues$ClosenessIn <- as.numeric(cent[i, "ClosenessIn"])
                      }
                      if ("ClosenessOut" %in% vectorCharacter && "ClosenessOut" %in% colnames(cent)) {
                          rowValues$ClosenessOut <- as.numeric(cent[i, "ClosenessOut"])
                      }
                      if ("Closeness" %in% vectorCharacter && "Closeness" %in% colnames(cent)) {
                          rowValues$Closeness <- as.numeric(cent[i, "Closeness"])
                      }
                      if ("Betweenness" %in% vectorCharacter && "Betweenness" %in% colnames(cent)) {
                          rowValues$Betweenness <- as.numeric(cent[i, "Betweenness"])
                      }
                      if ("BetweennessRSP" %in% vectorCharacter && "BetweennessRSP" %in% colnames(cent)) {
                          rowValues$BetweennessRSP <- as.numeric(cent[i, "BetweennessRSP"])
                      }
                      if ("Diffusion" %in% vectorCharacter && "Diffusion" %in% colnames(cent)) {
                          rowValues$Diffusion <- as.numeric(cent[i, "Diffusion"])
                      }
                      if ("Clustering" %in% vectorCharacter && "Clustering" %in% colnames(cent)) {
                          rowValues$Clustering <- as.numeric(cent[i, "Clustering"])
                      }
                      
                      self$results$centralityTable$addRow(rowKey=row_count, values=rowValues)
                      row_count <- row_count + 1
                  }
              }
              
              # Add informational note if no rows were added
              if(row_count == 1) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "No centrality data available. Please check your data and settings.")
              }
          } else {
              # Add informational note if conditions not met
              if(is.null(cent)) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "Centrality calculation not performed.")
              } else if(length(vectorCharacter) == 0) {
                  self$results$centralityTable$setNote(key = "centrality_info", note = "Please select at least one centrality measure to display results.")
              }
          }
          
          # Set visibility based on user selection
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
          resultComs <- tryCatch({
            coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
            self$results$community_plot$setState(coms)
            TRUE
          }, error = function(e) {
            self$results$communityTitle$setVisible(TRUE)
            self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
            self$results$communityErrorText$setVisible(TRUE)
            FALSE
          })

          if(!resultComs) return()
        }

        # Populate communities table
        # Structure: coms$A$assignments, coms$B$assignments, etc.
        if(!is.null(coms) && isTRUE(self$options$community_show_table)) {
            row_key <- 1
            method_names <- NULL

            for(group_name in names(coms)) {
                group_coms <- coms[[group_name]]
                if(!is.null(group_coms) && !is.null(group_coms$assignments)) {
                    assignments <- group_coms$assignments

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
                        rowValues$group <- as.character(group_name)
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

        # Set visibility
        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityTable$setVisible(self$options$community_show_table)
        self$results$communityContent$setVisible(FALSE)
        self$results$communityTitle$setVisible(isTRUE(self$options$community_show_plot) || isTRUE(self$options$community_show_table))
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)
      if (cliques_threshold == 0) {
          cliques_threshold <- if (cliques_size <= 2) 0.1 else 0.01
      }

      if(!is.null(model) && (isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot)) ) {

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
          self$results$cliquesTitle$setVisible(isTRUE(self$options$cliques_show_text) || isTRUE(self$options$cliques_show_plot))
      }

      ### Bootstrap

      if(!is.null(model) && (isTRUE(self$options$bootstrap_show_table) || isTRUE(self$options$bootstrap_show_plot))) {
        
        # Use state to avoid recalculating expensive bootstrap, but allow recalculation when needed
        bs <- self$results$bootstrap_plot$state
        if(is.null(bs)) {
          iteration <- self$options$bootstrap_iteration
          level <- self$options$bootstrap_level
          method <- self$options$bootstrap_method

          range_low <- self$options$bootstrap_range_low
          range_up <- self$options$bootstrap_range_up

          threshold <- self$options$bootstrap_threshold

          bs <- tna::bootstrap(
            x=model, 
            iter=iteration,
            level=level,
            method=method,
            threshold=threshold,
            consistency_range=c(range_low, range_up)
          )

          # Store state to avoid recalculation
          self$results$bootstrap_plot$setState(bs)
        }

        # Populate table if we have data and table option is enabled
        if(!is.null(bs) && isTRUE(self$options$bootstrap_show_table)) {
          row_key_counter <- 1
          max_rows <- self$options$bootstrap_table_max_rows
          show_all <- isTRUE(self$options$bootstrap_table_show_all)
          significant_only <- isTRUE(self$options$bootstrap_table_significant_only)

          # Debug: Print structure to understand data format
          tryCatch({
            # Iterate through each group (A, B, C, etc.)
            for (group_name in names(bs)) {
              group_data <- bs[[group_name]]

              # Check if group has summary data
              if (!is.null(group_data) && !is.null(group_data$summary)) {
                summary_data <- group_data$summary

                # Check if summary_data has rows
                if (is.data.frame(summary_data) && !is.null(summary_data) && !is.na(nrow(summary_data)) && nrow(summary_data) > 0) {
                  # Sort by significance (significant first), then by p-value
                  sorted_summary <- summary_data[order(-summary_data$sig, summary_data$p_value), ]

                  # Filter for significant only if requested
                  if (significant_only) {
                    sorted_summary <- sorted_summary[sorted_summary$sig == TRUE, ]
                  }

                  # Skip if no rows after filtering
                  if (nrow(sorted_summary) == 0) next

                  # Add each edge to the table (with row limit check)
                  for (i in 1:nrow(sorted_summary)) {
                    # Check row limit unless show_all is enabled
                    if (!show_all && row_key_counter > max_rows) break

                    rowValues <- list(
                      group = group_name,
                      from = as.character(sorted_summary[i, "from"]),
                      to = as.character(sorted_summary[i, "to"]),
                      weight = as.numeric(sorted_summary[i, "weight"]),
                      p_value = as.numeric(sorted_summary[i, "p_value"]),
                      cr_lower = as.numeric(sorted_summary[i, "cr_lower"]),
                      cr_upper = as.numeric(sorted_summary[i, "cr_upper"]),
                      ci_lower = as.numeric(sorted_summary[i, "ci_lower"]),
                      ci_upper = as.numeric(sorted_summary[i, "ci_upper"]),
                      significant = ifelse(sorted_summary[i, "sig"], "Yes", "No")
                    )
                    self$results$bootstrapTable$addRow(rowKey=as.character(row_key_counter), values=rowValues)
                    row_key_counter <- row_key_counter + 1
                  }

                  # Break outer loop if row limit reached
                  if (!show_all && row_key_counter > max_rows) break
                }
              }
            }

            # If no data was added, add a debug message
            if (row_key_counter == 1) {
              self$results$bootstrapTable$setNote(key = "bootstrap_debug", note = "No bootstrap results found. Check that bootstrap analysis completed successfully.")
            }

          }, error = function(e) {
            # Error handling - show structure info
            self$results$bootstrapTable$setNote(key = "bootstrap_error", note = paste("Bootstrap table error:", e$message))
          })
        }
        
        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
        self$results$bootstrapTitle$setVisible(isTRUE(self$options$bootstrap_show_plot) || isTRUE(self$options$bootstrap_show_table))
      }



      ## Permutation

      if(!is.null(model) && (isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))) {
        
        # Use state to avoid recalculating expensive permutation test, but allow recalculation when needed
        permutationTest <- self$results$permutation_plot$state
        if(is.null(permutationTest)) {
          permutationTest <- tna::permutation_test(
            x=model, 
            iter=self$options$permutation_iter, 
            paired=self$options$permutation_paired,
            level=self$options$permutation_level
          )

          # Store state to avoid recalculation
          self$results$permutation_plot$setState(permutationTest)
        }

        # Populate table if we have data and table option is enabled
        if (!is.null(permutationTest) && isTRUE(self$options$permutation_show_text)) {
          row_key_counter <- 1
          max_rows <- self$options$permutation_table_max_rows
          show_all <- isTRUE(self$options$permutation_table_show_all)
          for (comparison_name in names(permutationTest)) {
            comparison_data <- permutationTest[[comparison_name]]
            if (!is.null(comparison_data$edges) && !is.null(comparison_data$edges$stats)) {
              stats_df <- comparison_data$edges$stats

              # Handle NAs and ensure numeric types
              stats_df$diff_true <- as.numeric(stats_df$diff_true)
              stats_df$effect_size <- as.numeric(stats_df$effect_size)
              stats_df$p_value <- as.numeric(stats_df$p_value)

              # Sort by p_value and then by diff_true (descending)
              filtered_sorted_stats <- stats_df[order(stats_df$p_value, -stats_df$diff_true), ]

              # Add debug info about edge names
              if("edge_name" %in% colnames(filtered_sorted_stats)) {
                sample_edges <- head(filtered_sorted_stats$edge_name, 3)
                # Also check character encoding and length
                debug_info <- paste("Found", nrow(filtered_sorted_stats), "edges. Sample:",
                                  paste(sample_edges, collapse="; "),
                                  ". Lengths:", paste(nchar(sample_edges), collapse=","))
                self$results$permutationContent$setNote(key = "edge_debug", note = debug_info)
              } else {
                available_cols <- paste(colnames(filtered_sorted_stats), collapse=", ")
                self$results$permutationContent$setNote(key = "edge_debug", note = paste("No edge_name column. Available:", available_cols))
              }

              for (i in 1:nrow(filtered_sorted_stats)) {
                # Check row limit unless show_all is enabled
                if (!show_all && row_key_counter > max_rows) break
                # Get edge name with robust handling
                edge_name_value <- filtered_sorted_stats[i, "edge_name"]
                
                # Handle various edge name formats and potential issues
                if(is.null(edge_name_value) || is.na(edge_name_value) || edge_name_value == "" || edge_name_value == " -> ") {
                  # Try to construct from row name or use fallback
                  edge_name_value <- rownames(filtered_sorted_stats)[i]
                  if(is.null(edge_name_value) || edge_name_value == "") {
                    edge_name_value <- paste("Edge", i)
                  }
                } else {
                  # Clean the edge name and ensure it's properly formatted
                  edge_name_value <- trimws(as.character(edge_name_value))
                  # Replace any problematic characters that might cause display issues
                  edge_name_value <- gsub("[\u00A0\u2013\u2014]", "->", edge_name_value)
                }
                
                rowValues <- list(
                  group_comparison = comparison_name,
                  edge_name = edge_name_value,
                  diff_true = as.numeric(filtered_sorted_stats[i, "diff_true"]),
                  effect_size = as.numeric(filtered_sorted_stats[i, "effect_size"]),
                  p_value = as.numeric(filtered_sorted_stats[i, "p_value"])
                )
                self$results$permutationContent$addRow(rowKey=as.character(row_key_counter), values=rowValues)
                row_key_counter <- row_key_counter + 1
              }
              # Break outer loop if row limit reached
              if (!show_all && row_key_counter > max_rows) break
            }
          }
        }
        # Set visibility
        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationContent$setVisible(self$options$permutation_show_text)
        self$results$permutationTitle$setVisible(isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))
      }

      ### Sequence Analysis

      if(isTRUE(self$options$sequences_show_plot)) {

          # Set visibility for sequence analysis
          self$results$sequences_plot$setVisible(TRUE)

      } else {
          self$results$sequences_plot$setVisible(FALSE)
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
          # Get column names dynamically (freq_X, prop_X for each group)
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
          <b>Compare Network Properties</b>: Compares general network properties between two groups, including edge weight correlations, distances, and structural metrics like density and reciprocity.
          </div>'
        )
        self$results$compareInstructions$setVisible(TRUE)

        # Get available group names from model
        available_groups <- names(model)

        # Get selected group names, auto-select first two if not specified
        group_i_name <- self$options$compare_group_i
        group_j_name <- self$options$compare_group_j

        # Auto-select first group if not specified
        if(is.null(group_i_name) || group_i_name == "") {
          if(length(available_groups) >= 1) {
            group_i_name <- available_groups[1]
          }
        }

        # Auto-select second group if not specified
        if(is.null(group_j_name) || group_j_name == "") {
          if(length(available_groups) >= 2) {
            group_j_name <- available_groups[2]
          }
        }

        # Check if groups are valid
        if(is.null(group_i_name) || is.null(group_j_name) || group_i_name == "" || group_j_name == "") {
          self$results$compareTitle$setContent("Need at least two groups to compare")
          self$results$compareTitle$setVisible(TRUE)
          return()
        }

        if(group_i_name == group_j_name) {
          self$results$compareTitle$setContent("Please select two different groups to compare")
          self$results$compareTitle$setVisible(TRUE)
          return()
        }

        compResult <- self$results$compare_plot$state
        if(is.null(compResult)) {
          tryCatch({
            # Call tna::compare for group_tna using group names
            compResult <- tna::compare(
              x = model,
              i = group_i_name,
              j = group_j_name,
              scaling = self$options$compare_scaling,
              network = TRUE
            )

            # Store state along with group names
            self$results$compare_plot$setState(list(
              result = compResult,
              group_i = group_i_name,
              group_j = group_j_name
            ))
          }, error = function(e) {
            self$results$errorText$setContent(paste("Compare Error:", e$message))
            self$results$errorText$setVisible(TRUE)
          })
        } else {
          # Extract from stored state
          compResult <- compResult$result
        }

        # Set title with group names
        self$results$compareTitle$setContent(paste("Comparing:", group_i_name, "vs", group_j_name))

        # Summary metrics table
        if(!is.null(compResult) && isTRUE(self$options$compare_show_summary)) {
          if(!is.null(compResult$summary_metrics) && nrow(compResult$summary_metrics) > 0) {
            self$results$compareSummaryTable$setTitle(paste("Summary Metrics:", group_i_name, "vs", group_j_name))
            for(i in 1:nrow(compResult$summary_metrics)) {
              self$results$compareSummaryTable$addRow(rowKey=i, values=list(
                metric = as.character(compResult$summary_metrics$metric[i]),
                value = as.numeric(compResult$summary_metrics$value[i])
              ))
            }
          }
          self$results$compareSummaryTable$setVisible(TRUE)
        }

        # Network metrics table
        if(!is.null(compResult) && isTRUE(self$options$compare_show_network)) {
          if(!is.null(compResult$network_metrics) && nrow(compResult$network_metrics) > 0) {
            self$results$compareNetworkTable$setTitle(paste("Network Properties:", group_i_name, "vs", group_j_name))

            # Update column titles with actual group names
            self$results$compareNetworkTable$getColumn("group_i")$setTitle(group_i_name)
            self$results$compareNetworkTable$getColumn("group_j")$setTitle(group_j_name)

            for(i in 1:nrow(compResult$network_metrics)) {
              row_data <- compResult$network_metrics[i, ]
              self$results$compareNetworkTable$addRow(rowKey=i, values=list(
                metric = as.character(row_data$metric),
                group_i = as.numeric(row_data[[2]]),
                group_j = as.numeric(row_data[[3]])
              ))
            }
          }
          self$results$compareNetworkTable$setVisible(TRUE)
        }

        # Set visibility
        self$results$compare_plot$setVisible(self$options$compare_show_plot)
        self$results$compareTitle$setVisible(showAnyCompare)
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
          group_col <- self$options$buildModel_variables_long_group

          args_prepare_data <- list(
            data = self$data,
            action = action_col
          )
          if(!is.null(actor_col) && length(actor_col) > 0 && actor_col != "") {
            args_prepare_data$actor <- actor_col
          }
          if(!is.null(time_col) && length(time_col) > 0 && time_col != "") {
            args_prepare_data$time <- time_col
          }
          if(!is.null(order_col) && length(order_col) > 0 && order_col != "") {
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
            if(!is.null(actor_col) && length(actor_col) > 0 && actor_col != "") {
              unique_actors <- unique(self$data[[actor_col]])
              if (length(unique_actors) == nrow(indices)) {
                indices$actor <- as.character(unique_actors)
              } else {
                indices$actor <- NA
              }
            } else {
              indices$actor <- NA
            }

            # Add group if provided
            if(!is.null(group_col) && length(group_col) > 0 && group_col != "") {
              if(!is.null(actor_col) && length(actor_col) > 0 && actor_col != "") {
                unique_actors <- unique(self$data[[actor_col]])
                if (length(unique_actors) == nrow(indices)) {
                  group_values <- sapply(unique_actors, function(a) {
                    self$data[[group_col]][self$data[[actor_col]] == a][1]
                  })
                  indices$group <- as.character(group_values)
                } else {
                  indices$group <- NA
                }
              } else {
                indices$group <- NA
              }
            } else {
              indices$group <- NA
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
                  group = if(is.na(indices$group[i])) "" else as.character(indices$group[i]),
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
        }, error = function(e) {
          self$results$indicesTitle$setContent(paste("Sequence Indices error:", e$message))
        })
      }
    },
    .showBuildModelPlot=function(image, ...) {
      plotData <- self$results$buildModelContent$state

      if(!is.null(plotData) && self$options$buildModel_show_plot)  {
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
            cut=0.1, # Default cut level set to 0.1
            minimum=self$options$buildModel_plot_min_value,
            edge.label.cex=self$options$buildModel_plot_edge_label_size,
            node.width=self$options$buildModel_plot_node_size,
            label.cex=self$options$buildModel_plot_node_label_size,
            layout=self$options$buildModel_plot_layout
          )
        }, error = function(e) {
          layout_name <- self$options$buildModel_plot_layout
          self$results$errorText$setContent(paste0("Layout '", layout_name, "' is not available for this network. Please try a different layout."))
          self$results$errorText$setVisible(TRUE)
        })
        TRUE
      }
      else {
        FALSE
      }
    },
    .showBuildModelHisto=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_histo)  {
        tryCatch({
        # Check if plotData is a list (multiple groups) or single model
        if(is.list(plotData) && length(plotData) > 1) {
          # Multiple groups - set up layout like other plots
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
          
          # Create histogram for each group
          for(i in 1:length(plotData)) {
            group_name <- names(plotData)[i]
            if(is.null(group_name)) group_name <- paste("Group", i)
            
            w <- c(plotData[[i]]$weights)
            brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
            hist(x=plotData[[i]], breaks=brks,
                 main=paste("Histogram -", group_name),
                 xlab="Edge Weights (Probabilities)",
                 ylab="Frequency")
          }
        } else {
          # Single model - use single plot layout
          par(mfrow = c(1, 1))
          w <- c(plotData$weights)
          brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
          hist(x=plotData, breaks=brks, main="Histogram of Edge Weights (Probabilities)",
               xlab="Edge Weights (Probabilities)", ylab="Frequency")
        }
        }, error = function(e) {
            plot(1, type="n", main="Histogram Error", sub=e$message)
        })
        TRUE
      } else {
        FALSE
      }
    },
    .showBuildModelFrequencies=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_frequencies)  {
        # Use the tna package plot_frequencies function and print the result
        tryCatch({
          p <- tna::plot_frequencies(x=plotData)
          if(!is.null(p)) {
            print(p)
          }
        }, error = function(e) {
          # Fallback to hist if plot_frequencies fails
          w <- c(plotData$weights)
          brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
          hist(x=plotData, breaks=brks, main="Frequencies Plot",
               xlab="Edge Weights", ylab="Frequency")
        })
        TRUE
      } else {
        FALSE
      }
    },
    .showBuildModelMosaic=function(image, ...) {
      plotData <- self$results$buildModelContent$state

      if(!is.null(plotData) && self$options$buildModel_show_mosaic)  {
        tryCatch({
            p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
            print(p)
        }, error = function(e) {
            plot(1, type="n", main="Mosaic Plot Error", sub=e$message)
        })
        TRUE
      } else {
        FALSE
      }
    },
    .showCentralityPlot=function(image, ...) {

      plotData <- self$results$centralityTable$state

      if(!is.null(plotData) && self$options$centrality_show_plot)  {
        tryCatch({
            centPlot <- plot(plotData)
            print(centPlot)
        }, error = function(e) {
            plot(1, type="n", main="Centrality Plot Error", sub=e$message)
        })
        TRUE
      } else {
        FALSE
      }
    },
    .showCommunityPlot=function(image, ...) {
      plotData <- self$results$community_plot$state

      if(!is.null(plotData) && self$options$community_show_plot)  {
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
        }, error = function(e) {
            plot(1, type="n", main="Community Plot Error", sub=e$message)
        })
        TRUE
      }
      else {
        FALSE
      }
    },
    .showCliquesMultiPlot=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        if (!is.null(plotData) && self$options$cliques_show_plot) {
            tryCatch({
                # group_tna_cliques is a list of tna_cliques per group
                # Count total clique plots across all groups
                total <- sum(sapply(plotData, function(g) length(g$weights)))
                if (total == 0) return(FALSE)
                nc <- ceiling(sqrt(total))
                nr <- ceiling(total / nc)
                par(mfrow = c(nr, nc))
                plot(x=plotData, ask=FALSE,
                    cut=self$options$cliques_plot_cut,
                    minimum=self$options$cliques_plot_min_value,
                    edge.label.cex=self$options$cliques_plot_edge_label_size,
                    node.width=self$options$cliques_plot_node_size,
                    label.cex=self$options$cliques_plot_node_label_size,
                    layout=self$options$cliques_plot_layout)
            }, error = function(e) { plot(1, type="n", main="Cliques Plot Error", sub=e$message) })
            TRUE
        } else {
            FALSE
        }
    },
    .showBootstrapPlot=function(image, ...) {
        plotData <- self$results$bootstrap_plot$state
        if(!is.null(plotData) && self$options$bootstrap_show_plot)  {
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
                plot(x=plotData, cut = 0.1)
            }, error = function(e) { plot(1, type="n", main="Bootstrap Plot Error", sub=e$message) })
        }
        TRUE
    },
    .showPermutationPlot=function(image, ...) {
      plotData <- self$results$permutation_plot$state
      if(!is.null(plotData) && self$options$permutation_show_plot)  {
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
            plot(x=plotData)
        }, error = function(e) { plot(1, type="n", main="Permutation Plot Error", sub=e$message) })
        TRUE
      }
      else {
        FALSE
      }
    },
        .showSequencesPlot=function(image, ...) {

            if(self$options$sequences_show_plot) {

                # Get the TNA data
                tna_data <- self$results$buildModelContent$state

                if(!is.null(tna_data)) {

                    # Call the tna::plot_sequences function directly
                    tryCatch({
                        plot_result <- tna::plot_sequences(
                            x = tna_data,
                            type = self$options$sequences_type,
                            scale = self$options$sequences_scale,
                            geom = self$options$sequences_geom,
                            include_na = self$options$sequences_include_na,
                            tick = self$options$sequences_tick
                        )

                        print(plot_result)
                    }, error = function(e) {
                        # Simple fallback plot
                        plot(1, type="n", main="Sequence Analysis Error",
                             sub=paste("Error:", e$message))
                    })
                }
            }
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
        # Extract comparison result and group names from state
        plotData <- stateData$result
        name_x <- stateData$group_i
        name_y <- stateData$group_j

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
