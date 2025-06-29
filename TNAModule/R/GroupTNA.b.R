# This file is a generated template, your changes will not be overwritten

GroupTNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
  "GroupTNAClass",
  inherit = GroupTNABase,
  private = list(
    .run = function() {
      library("tna")

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

      if(!self$options$buildModel_show_matrix &&
        !self$options$buildModel_show_plot &&
        !self$options$buildModel_show_histo &&
        (
          !self$options$buildModel_show_mosaic ||
          (
            self$options$buildModel_show_mosaic && 
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

          ##### TO REMOVE
          values_to_replace <- c("Applications", "Ethics", "General", "La_types", "Theory")
          new_value <- "Resources"
          longData[[actionColumn]] <- replace(
              longData[[actionColumn]], 
              longData[[actionColumn]] %in% values_to_replace, 
              new_value
          )
          ##### END TO REMOVE

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

      if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot) ) {
          centrality_loops <- self$options$centrality_loops
          centrality_normalize <- self$options$centrality_normalize

          vectorCharacter <- character(0)    

          # OPTIMIZED: Check if table is already filled before expensive centrality calculation
          fullTable <- self$results$centralityTable$isFilled()

          # Only add columns if table is not filled
          if(!fullTable) {
              self$results$centralityTable$addColumn(name="group", type="text")
              self$results$centralityTable$addColumn(name="state", type="text")

              if(self$options$centrality_OutStrength) {
                  vectorCharacter <- append(vectorCharacter, "OutStrength")
                  self$results$centralityTable$addColumn(name="OutStrength", type="number")
              }
              if(self$options$centrality_InStrength) {
                  vectorCharacter <- append(vectorCharacter, "InStrength")
                  self$results$centralityTable$addColumn(name="InStrength", type="number")
              }
              if(self$options$centrality_ClosenessIn) {
                  vectorCharacter <- append(vectorCharacter, "ClosenessIn")
                  self$results$centralityTable$addColumn(name="ClosenessIn", type="number")
              }
              if(self$options$centrality_ClosenessOut) {
                  vectorCharacter <- append(vectorCharacter, "ClosenessOut")
                  self$results$centralityTable$addColumn(name="ClosenessOut", type="number")
              }
              if(self$options$centrality_Closeness) {
                  vectorCharacter <- append(vectorCharacter, "Closeness")
                  self$results$centralityTable$addColumn(name="Closeness", type="number")
              }
              if(self$options$centrality_Betweenness) {
                  vectorCharacter <- append(vectorCharacter, "Betweenness")
                  self$results$centralityTable$addColumn(name="Betweenness", type="integer")
              }
              if(self$options$centrality_BetweennessRSP) {
                  vectorCharacter <- append(vectorCharacter, "BetweennessRSP")
                  self$results$centralityTable$addColumn(name="BetweennessRSP", type="number")
              }
              if(self$options$centrality_Diffusion) {
                  vectorCharacter <- append(vectorCharacter, "Diffusion")
                  self$results$centralityTable$addColumn(name="Diffusion", type="number")
              }
              if(self$options$centrality_Clustering) {
                  vectorCharacter <- append(vectorCharacter, "Clustering")
                  self$results$centralityTable$addColumn(name="Clustering", type="number")
              }
          }

          # OPTIMIZED: Use state to store centrality results and avoid recalculation
          cent <- self$results$centralityTable$state
          if(length(vectorCharacter) > 0 && !is.null(model) && (!self$results$centrality_plot$isFilled() || !fullTable) ) {
              cent <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
              self$results$centralityTable$setState(cent)
          }

              for (i in 1:lengths(cent[1])) {
                  index <- 1
                  rowValues <- list()

                  rowValues$group <- as.character(cent[i, index])

                  index <- index + 1
                  rowValues$state <- as.character(cent[i, index])

                  if ("OutStrength" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$OutStrength <- as.numeric(cent[i, index])
                  }
                  if ("InStrength" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$InStrength <- as.numeric(cent[i, index])
                  }
                  if ("ClosenessIn" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$ClosenessIn <- as.numeric(cent[i, index])
                  }
                  if ("ClosenessOut" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$ClosenessOut <- as.numeric(cent[i, index])
                  }
                  if ("Closeness" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$Closeness <- as.numeric(cent[i, index])
                  }
                  if ("Betweenness" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$Betweenness <- as.numeric(cent[i, index])
                  }
                  if ("BetweennessRSP" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$BetweennessRSP <- as.numeric(cent[i, index])
                  }
                  if ("Diffusion" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$Diffusion <- as.numeric(cent[i, index])
                  }
                  if ("Clustering" %in% vectorCharacter) {
                      index <- index + 1
                      rowValues$Clustering <- as.numeric(cent[i, index])
                  }
                  self$results$centralityTable$addRow(rowKey=i, values=rowValues)
          }
          self$results$centralityTitle$setVisible(self$options$centrality_show_plot || self$options$centrality_show_table)
          self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
          self$results$centralityTable$setVisible(self$options$centrality_show_table)
          
      }


      ### Community
      if(!is.null(model) &&  (self$options$community_show_table || self$options$community_show_plot) ) {
        community_gamma <- as.numeric(self$options$community_gamma)
        methods <- self$options$community_methods

        # OPTIMIZED: Use state to avoid recalculating community detection
        coms <- self$results$community_plot$state
        if(is.null(coms) || (!self$results$communityContent$isFilled() && self$options$community_show_table)) {
          resultComs <- tryCatch({
            coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
            
            # Store state to avoid recalculation
            self$results$community_plot$setState(coms)
            if(!self$results$communityContent$isFilled()) {
                self$results$communityContent$setContent(coms)
            }
            TRUE
          }, error = function(e) {
            self$results$communityTitle$setVisible(TRUE)
            self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
            self$results$communityErrorText$setVisible(TRUE)
            FALSE
          })
          
          if(!resultComs) return()
        }

        # Set visibility
        self$results$community_plot$setVisible(self$options$community_show_plot)
        self$results$communityContent$setVisible(self$options$community_show_table)
        self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)

      if(!is.null(model) && ( self$options$cliques_show_text || self$options$cliques_show_plot) ) {

          # OPTIMIZED: Use state to avoid recalculating expensive cliques analysis
          cliques <- self$results$cliques_multiple_plot$state
          if(is.null(cliques) || (!self$results$cliquesContent$isFilled() && self$options$cliques_show_text)) {
              cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)

              # Store state to avoid recalculation
              self$results$cliques_multiple_plot$setState(cliques)
              
              if(!self$results$cliquesContent$isFilled() && self$options$cliques_show_text) {
                self$results$cliquesContent$setContent(cliques)
              }
          }
          
          # Set visibility
          self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
          self$results$cliquesContent$setVisible(self$options$cliques_show_text)
          self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)
      }

      ### Bootstrap

      if(!is.null(model) && ( self$options$bootstrap_show_table || self$options$bootstrap_show_plot)) {
        
        # OPTIMIZED: Use state to avoid recalculating expensive bootstrap
        bs <- self$results$bootstrap_plot$state
        if(is.null(bs) || (!self$results$bootstrapTable$isFilled() && self$options$bootstrap_show_table)) {
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

        # OPTIMIZED: Only populate table if not already done
        if(!is.null(bs) && self$options$bootstrap_show_table && !self$results$bootstrapTable$isFilled()) {
          row_key_counter <- 1
          
          # Debug: Print structure to understand data format
          tryCatch({
            # Iterate through each group (A, B, C, etc.)
            for (group_name in names(bs)) {
              group_data <- bs[[group_name]]
              
              # Check if group has summary data
              if (!is.null(group_data) && !is.null(group_data$summary)) {
                summary_data <- group_data$summary
                
                # Check if summary_data has rows
                if (is.data.frame(summary_data) && nrow(summary_data) > 0) {
                  # Sort by significance (significant first), then by p-value
                  sorted_summary <- summary_data[order(-summary_data$sig, summary_data$p_value), ]
                  
                  # Add each edge to the table
                  for (i in 1:nrow(sorted_summary)) {
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
        self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_table)

      }



      ## Permutation

      if(!is.null(model) && ( self$options$permutation_show_text || self$options$permutation_show_plot)) {
        
        # OPTIMIZED: Use state to avoid recalculating expensive permutation test
        permutationTest <- self$results$permutation_plot$state
        if(is.null(permutationTest) || (!self$results$permutationContent$isFilled() && self$options$permutation_show_text)) {
          permutationTest <- tna::permutation_test(
            x=model, 
            iter=self$options$permutation_iter, 
            paired=self$options$permutation_paired,
            level=self$options$permutation_level
          )

          # Store state to avoid recalculation
          self$results$permutation_plot$setState(permutationTest)
        }

        # OPTIMIZED: Only populate table if not already done
        if (!is.null(permutationTest) && self$options$permutation_show_text && !self$results$permutationContent$isFilled()) {
          row_key_counter <- 1
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

              for (i in 1:nrow(filtered_sorted_stats)) {
                rowValues <- list(
                  group_comparison = comparison_name,
                  edge_name = paste0(filtered_sorted_stats[i, "from"], " -> ", filtered_sorted_stats[i, "to"]),
                  diff_true = filtered_sorted_stats[i, "diff_true"],
                  effect_size = filtered_sorted_stats[i, "effect_size"],
                  p_value = filtered_sorted_stats[i, "p_value"]
                )
                self$results$permutationContent$addRow(rowKey=as.character(row_key_counter), values=rowValues)
                row_key_counter <- row_key_counter + 1
              }
            }
          }
        }
        # Set visibility
        self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
        self$results$permutationContent$setVisible(self$options$permutation_show_text)
        self$results$permutationTitle$setVisible(self$options$permutation_show_text || self$options$permutation_show_plot)
      }

      ### Sequence Analysis

      if(self$options$sequences_show_plot) {
          
          # Set visibility for sequence analysis
          self$results$sequences_plot$setVisible(TRUE)
          
      } else {
          self$results$sequences_plot$setVisible(FALSE)
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
        plot(x=plotData, 
          cut=0.1, # Default cut level set to 0.1
          minimum=self$options$buildModel_plot_min_value,
          edge.label.cex=self$options$buildModel_plot_edge_label_size,
          node.width=self$options$buildModel_plot_node_size,
          label.cex=self$options$buildModel_plot_node_label_size,
          layout=self$options$buildModel_plot_layout,
          pie=NULL
        )
        TRUE
      }
      else {
        FALSE
      }
    },
    .showBuildModelHisto=function(image, ...) {
      plotData <- self$results$buildModelContent$state
      
      if(!is.null(plotData) && self$options$buildModel_show_histo)  {
        # Use single plot layout for better title display
        par(mfrow = c(1, 1))
        hist(x=plotData, main="Histogram of Edge Weights (Probabilities)", 
             xlab="Edge Weights (Probabilities)", ylab="Frequency")
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
          hist(x=plotData, main="Frequencies Plot", 
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
        p <- tna::plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
        print(p)
        TRUE
      } else {
        FALSE
      }
    },
    .showCentralityPlot=function(image, ...) {

      plotData <- self$results$centralityTable$state

      if(!is.null(plotData) && self$options$centrality_show_plot)  {
        centPlot <- plot(plotData) 
        print(centPlot)
        TRUE
      } else {
        FALSE
      }
    },
    .showCommunityPlot=function(image, ...) {
      plotData <- self$results$community_plot$state

      if(!is.null(plotData) && self$options$community_show_plot)  {
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
        methods <- self$options$community_methods
        plot(x=plotData, method=methods, pie=NULL)
        TRUE
      }
      else {
        FALSE
      }
    },
    .showCliquesPlot1=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state

        number_value <- lengths(plotData[1])
        
        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 0)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=1, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot1$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot2=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])
        
        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 1)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=2, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot2$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot3=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 2)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=3, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot3$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot4=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 3)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=4, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot4$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot5=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 4)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=5, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }
        else {
            self$results$cliques_multiple_plot$cliques_plot5$setVisible(FALSE)
            FALSE
        }
    },
    .showCliquesPlot6=function(image, ...) {
        plotData <- self$results$cliques_multiple_plot$state
        
        number_value <- lengths(plotData[1])

        if(!is.null(plotData) && self$options$cliques_show_plot && number_value > 5)  {
            len <- length(plotData)
            if (len == 0) return(FALSE)
            column <- ceiling(sqrt(len))
            row <- ceiling(len / column)

            par(mfrow = c(row, column))
            plot(x=plotData, 
                ask=FALSE, 
                first=6, 
                n=1,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$cliques_plot_min_value,
                edge.label.cex=self$options$cliques_plot_edge_label_size,
                node.width=self$options$cliques_plot_node_size,
                label.cex=self$options$cliques_plot_node_label_size,
                layout=self$options$cliques_plot_layout
            )
            TRUE
        }   
        else {
            self$results$cliques_multiple_plot$cliques_plot6$setVisible(FALSE)
            FALSE
        }
    },
    .showBootstrapPlot=function(image, ...) {

        plotData <- self$results$bootstrap_plot$state

        if(!is.null(plotData) && self$options$bootstrap_show_plot)  {
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
            m_sig <- plotData[[1]]$weights_sig

            mat_color <- matrix()
            mat_color[m_sig == 0] <- "red"
            plot(
                x=plotData,
                cut=0.1, # Default cut level set to 0.1
                minimum=self$options$bootstrap_plot_min_value,
                edge.label.cex=self$options$bootstrap_plot_edge_label_size,
                node.width=self$options$bootstrap_plot_node_size,
                label.cex=self$options$bootstrap_plot_node_label_size,
                layout=self$options$bootstrap_plot_layout,
                edge.color = mat_color,
                pie=NULL
            )
            TRUE
        }
        else {
            FALSE
        }     
    },

    .showPermutationPlot=function(image, ...) {

      plotData <- self$results$permutation_plot$state
      if(!is.null(plotData) && self$options$permutation_show_plot)  {

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
        }
  )
)
