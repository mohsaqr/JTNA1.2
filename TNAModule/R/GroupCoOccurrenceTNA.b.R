# Group Co-occurrence Network Analysis

GroupCoOccurrenceTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "GroupCoOccurrenceTNAClass",
  inherit = GroupCoOccurrenceTNABase,
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
          <li><b>Group</b>: Column defining groups for comparison (required). Separate co-occurrence networks will be built for each group.</li>
          <li><b>Time</b> or <b>Order</b>: For ordering events chronologically (optional).</li>
          <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html" target="_blank">TNA Tutorial</a> | <a href="https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html" target="_blank">FTNA</a> | <a href="https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html" target="_blank">Group TNA</a></li>
        </ul>
        </div>
        </div>'
      )

      type <- "co-occurrence"  # Hardcoded to co-occurrence
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
        !isTRUE(self$options$buildModel_show_histo))
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

              model <- tna::group_model(x=dataForTNA, group=group[[groupColumn]], type=type, scaling=scaling)
          }

        }, error = function(e) {
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

          self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot)
          self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo)
          self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies)
      }

      ### Centrality

      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {
          centrality_loops <- self$options$centrality_loops
          centrality_normalize <- self$options$centrality_normalize

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

          cent <- self$results$centralityTable$state

          if(length(vectorCharacter) > 0 && !is.null(model)) {
              if(is.null(cent) || !self$results$centralityTable$isFilled()) {
                  tryCatch({
                      cent_result <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)

                      if(!is.null(cent_result) && is.data.frame(cent_result)) {
                          cent <- cent_result
                          self$results$centralityTable$setState(cent)
                      }
                  }, error = function(e) {
                      self$results$centralityTable$setNote(key = "centrality_error", note = paste("Centrality calculation error:", e$message))
                      cent <- NULL
                  })
              }
          }

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

          if(!is.null(cent) && length(vectorCharacter) > 0) {
              row_count <- 1

              if(is.data.frame(cent) && !is.null(cent) && !is.na(nrow(cent)) && nrow(cent) > 0) {
                  for (i in 1:nrow(cent)) {
                      rowValues <- list()

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

        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityTable$setVisible(self$options$community_show_table)
        self$results$communityContent$setVisible(FALSE)
        self$results$communityTitle$setVisible(isTRUE(self$options$community_show_plot) || isTRUE(self$options$community_show_table))
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)

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

          self$results$bootstrap_plot$setState(bs)
        }

        if(!is.null(bs) && isTRUE(self$options$bootstrap_show_table)) {
          row_key_counter <- 1
          max_rows <- self$options$bootstrap_table_max_rows
          show_all <- isTRUE(self$options$bootstrap_table_show_all)
          significant_only <- isTRUE(self$options$bootstrap_table_significant_only)

          tryCatch({
            for (group_name in names(bs)) {
              group_data <- bs[[group_name]]

              if (!is.null(group_data) && !is.null(group_data$summary)) {
                summary_data <- group_data$summary

                if (is.data.frame(summary_data) && !is.null(summary_data) && !is.na(nrow(summary_data)) && nrow(summary_data) > 0) {
                  sorted_summary <- summary_data[order(-summary_data$sig, summary_data$p_value), ]

                  # Filter for significant only if requested
                  if (significant_only) {
                    sorted_summary <- sorted_summary[sorted_summary$sig == TRUE, ]
                  }

                  if (nrow(sorted_summary) == 0) next

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
          }, error = function(e) {
            self$results$bootstrapTable$setNote(key = "bootstrap_error", note = paste("Bootstrap table error:", e$message))
          })
        }

        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
        self$results$bootstrapTitle$setVisible(isTRUE(self$options$bootstrap_show_plot) || isTRUE(self$options$bootstrap_show_table))
      }

      ## Permutation

      if(!is.null(model) && (isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))) {

        permutationTest <- self$results$permutation_plot$state
        if(is.null(permutationTest)) {
          permutationTest <- tna::permutation_test(
            x=model,
            iter=self$options$permutation_iter,
            paired=self$options$permutation_paired,
            level=self$options$permutation_level
          )

          self$results$permutation_plot$setState(permutationTest)
        }

        if (!is.null(permutationTest) && isTRUE(self$options$permutation_show_text)) {
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

                if(is.null(edge_name_value) || is.na(edge_name_value) || edge_name_value == "" || edge_name_value == " -> ") {
                  edge_name_value <- rownames(filtered_sorted_stats)[i]
                  if(is.null(edge_name_value) || edge_name_value == "") {
                    edge_name_value <- paste("Edge", i)
                  }
                } else {
                  edge_name_value <- trimws(as.character(edge_name_value))
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
        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationContent$setVisible(self$options$permutation_show_text)
        self$results$permutationTitle$setVisible(isTRUE(self$options$permutation_show_text) || isTRUE(self$options$permutation_show_plot))
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
        if(!is.null(group_i_name) && !is.null(group_j_name) && group_i_name != "" && group_j_name != "" && group_i_name != group_j_name) {

          compResult <- self$results$compare_plot$state
          if(is.null(compResult)) {
            tryCatch({
              compResult <- tna::compare(
                x = model,
                i = group_i_name,
                j = group_j_name,
                scaling = self$options$compare_scaling,
                network = TRUE
              )

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
            compResult <- compResult$result
          }

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

          # Network properties table
          if(!is.null(compResult) && isTRUE(self$options$compare_show_network)) {
            if(!is.null(compResult$network_metrics) && nrow(compResult$network_metrics) > 0) {
              self$results$compareNetworkTable$setTitle(paste("Network Properties:", group_i_name, "vs", group_j_name))
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

          self$results$compare_plot$setVisible(self$options$compare_show_plot)
          self$results$compareTitle$setVisible(showAnyCompare)
        }
      }

      ### Network Difference Plot (independent section)
      if (!is.null(model) && isTRUE(self$options$compare_show_network_diff_plot)) {
        self$results$compare_network_diff_plot$setVisible(TRUE)
      }
    },
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
        if(is.null(group_name)) group_name <- paste("Group", i)
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
    .showCentralityPlot = function(image, ...) {
      plotData <- self$results$centralityTable$state
      if(is.null(plotData) || !self$options$centrality_show_plot) return(FALSE)
      print(plot(plotData))
      TRUE
    },
    .showCommunityPlot = function(image, ...) {
      plotData <- self$results$community_plot$state
      if(is.null(plotData) || !self$options$community_show_plot) return(FALSE)

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
    .showComparePlot = function(image, ...) {
      if(!self$options$compare_show_plot) return(FALSE)
      stateData <- self$results$compare_plot$state
      if(is.null(stateData)) return(FALSE)

      tryCatch({
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
