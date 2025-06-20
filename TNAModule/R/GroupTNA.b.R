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

      if(is.null(self$options$buildModel_variables_long_action)) {
        self$results$errorText$setVisible(TRUE)
        self$results$errorText$setContent("Action should be provided")
        return()
      }

      if(is.null(self$options$buildModel_variables_long_group)) {
        self$results$errorText$setVisible(TRUE)
        self$results$errorText$setContent("Group should be provided")
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

          fullTable <- self$results$centralityTable$isFilled()

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

        resultComs <- TRUE
        if((!self$results$communityContent$isFilled() || !self$results$community_plot$isFilled())) { 
          coms <- NULL

          resultComs <- tryCatch({
            coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
            TRUE
          }, error = function(e) {
            self$results$communityTitle$setVisible(TRUE)
            self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
            self$results$communityErrorText$setVisible(TRUE)
            FALSE
          })

          if(resultComs) {
            # Plot
            if(!self$results$community_plot$isFilled()) {
              self$results$community_plot$setState(coms)
            }
            # Text
            if(!self$results$communityContent$isFilled()) {
              self$results$communityContent$setContent(coms)
            }
          }

             
        }
        if(resultComs) {
          self$results$community_plot$setVisible(self$options$community_show_plot)
          self$results$communityContent$setVisible(self$options$community_show_table)
          self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)
        }
      }

      ### Cliques

      cliques_size <- as.numeric(self$options$cliques_size)
      cliques_threshold <- as.numeric(self$options$cliques_threshold)

      if(!is.null(model) && ( self$options$cliques_show_text || self$options$cliques_show_plot) ) {

          if(!self$results$cliques_multiple_plot$isFilled() || !self$results$cliquesContent$isFilled()) {
              cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)

              if(!self$results$cliquesContent$isFilled()) {
                self$results$cliquesContent$setContent(cliques)
              }

              # Plot
              if(!self$results$cliques_multiple_plot$isFilled()) {
                self$results$cliques_multiple_plot$setState(cliques)
              }
          }
          self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
          self$results$cliquesContent$setVisible(self$options$cliques_show_text)
          self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)
      }

      ### Bootstrap

      if(!is.null(model) && ( self$options$bootstrap_show_text || self$options$bootstrap_show_plot)) {
        if(!self$results$bootstrapContent$isFilled() || !self$results$bootstrap_plot$isFilled()) {
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

          if(!self$results$bootstrapContent$isFilled()) {
            self$results$bootstrapContent$setContent(bs)
          }

          # Plot
          if(!self$results$bootstrap_plot$isFilled()) {

            self$results$bootstrap_plot$setState(bs)
          }
        }
        self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
        self$results$bootstrapContent$setVisible(self$options$bootstrap_show_text)
        self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_text)

      }

      ### Comparison

      if(!is.null(model) && ( self$options$compare_show_text || self$options$compare_show_plot || self$options$compare_show_TNAplot)) {
          indexModel1 <- self$options$compare_model1
          indexModel2 <- self$options$compare_model2
          size <- length(model)

          if(size < indexModel1 || size < indexModel2) {
              self$results$errorText$setVisible(TRUE)
              self$results$errorText$setContent(paste("Comparison - The index given can't be bigger than the number of TNA (", size, ")"))
              return()
          }

          if( !self$results$comparisonContent$isFilled() || 
              !self$results$comparison_plot$isFilled() || 
              !self$results$comparisonTNA_plot$isFilled()
          )
          {
              compare_group <- tna::compare(model, i=indexModel1, j=indexModel2)

              # Title

              # Text
              if(!self$results$comparisonContent$isFilled()) {
                self$results$comparisonContent$setContent(compare_group)
              }

              # Plot compareTNA
              if(!self$results$comparisonTNA_plot$isFilled()) {
                self$results$comparisonTNA_plot$setState(compare_group)
              }
          }
          self$results$comparisonTitle$setVisible(self$options$compare_show_text || self$options$compare_show_plot || self$options$compare_show_TNAplot)
          self$results$comparisonContent$setVisible(self$options$compare_show_text)
          self$results$comparison_plot$setVisible(self$options$compare_show_plot)
          self$results$comparisonTNA_plot$setVisible(self$options$compare_show_TNAplot)
      }

      ## Permutation

      if(!is.null(model) && ( self$options$permutation_show_text || self$options$permutation_show_plot)) {
        if( !self$results$permutationContent$isFilled() || 
              !self$results$permutation_plot$isFilled()
          )
          {
            permutationTest <- tna::permutation_test(
              x=model, 
              iter=self$options$permutation_iter, 
              paired=self$options$permutation_paired,
              level=self$options$permutation_level
            )

            # Text
            if(!self$results$permutationContent$isFilled()) {
              self$results$permutationContent$setContent(permutationTest)
            }

            # Plot
            if(!self$results$permutation_plot$isFilled()) {
              self$results$permutation_plot$setState(permutationTest)
            }
          }
          self$results$permutationTitle$setVisible(self$options$permutation_show_text || self$options$permutation_show_plot)
          self$results$permutationContent$setVisible(self$options$permutation_show_text)
          self$results$permutation_plot$setVisible(self$options$permutation_show_plot)
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
    .showComparisonPlot=function(image, ...) {
        plotData <- self$results$buildModelContent$state

        if(!is.null(plotData) && self$options$compare_show_plot)  {
            tna::plot_compare(
                x=plotData,
                i = self$options$compare_model1,
                j = self$options$compare_model2
            )
            TRUE
        }
        else {
            FALSE
        }
    },
    .showComparisonTNAPlot=function(image, ...) {
        plotData <- self$results$comparisonTNA_plot$state

        if(!is.null(plotData) && self$options$compare_show_TNAplot)  {
            p <- plot(
                    x=plotData,
                    type = self$options$compare_TNAPlot_type,
                    population = self$options$compare_TNAPlot_population,
                    method = self$options$compare_TNAPlot_method,
                    name_x = "Model A",
                    name_y = "Model B"
                )
            print(p)
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
                
                # Get the TNA data (same as used for histogram and frequencies)
                tna_data <- self$results$buildModelContent$state
                
                if(!is.null(tna_data)) {
                    
                    # Build the plot_sequences function call
                    plot_args <- list(
                        x = tna_data,
                        type = self$options$sequences_type,
                        scale = self$options$sequences_scale,
                        geom = self$options$sequences_geom,
                        include_na = self$options$sequences_include_na,
                        show_n = self$options$sequences_show_n,
                        tick = self$options$sequences_tick
                    )
                    
                    # Call the tna::plot_sequences function and print the result
                    tryCatch({
                        plot_result <- do.call(tna::plot_sequences, plot_args)
                        
                        # Modify the plot to be more compact if it's a ggplot
                        if(inherits(plot_result, "ggplot")) {
                            plot_result <- plot_result + 
                                ggplot2::theme(
                                    aspect.ratio = 0.3,  # Make plots much shorter
                                    plot.margin = ggplot2::margin(5, 5, 5, 5),
                                    strip.text = ggplot2::element_text(size = 8),
                                    axis.text = ggplot2::element_text(size = 7),
                                    legend.text = ggplot2::element_text(size = 7),
                                    axis.title = ggplot2::element_text(size = 8)
                                )
                        }
                        
                        print(plot_result)
                    }, error = function(e) {
                        # Fallback to a simple plot if there's an error
                        plot(1, type="n", main="Sequence Analysis Error", 
                             sub=paste("Error:", e$message))
                    })
                }
            }
            TRUE
        }
  )
)
