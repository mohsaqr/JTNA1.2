# This file is a generated template, your changes will not be overwritten

SNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "SNAClass",
    inherit = SNABase,
    private = list(
        .run = function() {

            library("tna")

            # Set instructions content
            self$results$instructions$setContent(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
                <div style="text-align:justify;">
                <ul>
                  <li>Data should be in <b>edge-list format</b> with columns for source, target, and optionally weight.</li>
                  <li><b>From (Source)</b>: Column containing the source node of each edge (required).</li>
                  <li><b>To (Target)</b>: Column containing the target node of each edge (required).</li>
                  <li><b>Weight</b>: Column containing edge weights (optional). Defaults to 1 if not provided.</li>
                  <li><b>Group</b>: Column for grouping (optional). Creates separate networks for each group.</li>
                  <li><b>Aggregation</b>: How to combine multiple edges between the same nodes (sum, mean, max, min).</li>
                  <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html" target="_blank">TNA Tutorial</a></li>
                </ul>
                </div>
                </div>'
            )

            # Check if required variables are provided
            if(is.null(self$options$sna_from) || is.null(self$options$sna_to)) {
                self$results$errorText$setVisible(FALSE)
                return()
            }

            model <- NULL
            groupModels <- NULL

            # Hide title if no outputs selected
            if(!self$options$sna_show_summary && !self$options$sna_show_plot && !self$options$sna_show_degree_plot) {
                self$results$snaModelTitle$setVisible(FALSE)
            }

            # Check if model is already built (cached)
            if(self$results$snaModelContent$isFilled()) {
                state <- self$results$snaModelContent$state
                if(is.list(state) && !is.null(state$groupModels)) {
                    groupModels <- state$groupModels
                    model <- state$groupModels[[1]]  # Use first for centrality
                } else {
                    model <- state
                }
            }
            else if(!is.null(self$data) && ncol(self$data) >= 2) {

                # Build the SNA model
                tryCatch({

                    # Prepare edge-list data frame
                    from_col <- as.character(self$data[[self$options$sna_from]])
                    to_col <- as.character(self$data[[self$options$sna_to]])

                    # Handle weight column - default to 1 if not provided
                    if(!is.null(self$options$sna_weight)) {
                        weight_col <- as.numeric(self$data[[self$options$sna_weight]])
                    } else {
                        weight_col <- rep(1, nrow(self$data))
                    }

                    # Handle group column
                    hasGroup <- !is.null(self$options$sna_group)
                    if(hasGroup) {
                        group_col <- as.character(self$data[[self$options$sna_group]])
                    }

                    df <- data.frame(
                        from = from_col,
                        to = to_col,
                        weight = weight_col,
                        stringsAsFactors = FALSE
                    )

                    if(hasGroup) {
                        df$group <- group_col
                    }

                    # Remove rows with NA values
                    df <- df[complete.cases(df), ]

                    if(nrow(df) == 0) {
                        self$results$errorText$setContent("No valid edges found in data after removing missing values.")
                        self$results$errorText$setVisible(TRUE)
                        return()
                    }

                    # Get aggregation function
                    agg_fn <- switch(self$options$sna_aggregate,
                        "sum" = sum,
                        "mean" = mean,
                        "max" = max,
                        "min" = min,
                        sum  # default
                    )

                    # Build SNA model(s)
                    if(hasGroup) {
                        # Build separate models for each group
                        groups <- unique(df$group)
                        groupModels <- list()

                        for(g in groups) {
                            groupDf <- df[df$group == g, c("from", "to", "weight")]
                            groupModels[[g]] <- tna::sna(groupDf, aggregate = agg_fn)
                        }

                        # Store models in state
                        self$results$snaModelContent$setState(list(groupModels = groupModels))

                        # Create display content for all groups
                        displayContent <- ""
                        for(g in names(groupModels)) {
                            m <- groupModels[[g]]
                            matrixStr <- paste(capture.output(print(m$weights)), collapse = "\n")
                            displayContent <- paste0(
                                displayContent,
                                "=== Group: ", g, " ===\n",
                                "State Labels:\n  ",
                                paste(m$labels, collapse = ", "),
                                "\n\nEdge Weight Matrix:\n",
                                matrixStr, "\n\n"
                            )
                        }
                        self$results$snaModelContent$setContent(displayContent)

                        model <- groupModels[[1]]  # Use first for centrality

                    } else {
                        # Build single model
                        model <- tna::sna(df[, c("from", "to", "weight")], aggregate = agg_fn)

                        # Store model in state
                        self$results$snaModelContent$setState(model)

                        # Create a string representation of the weights matrix for display
                        matrixStr <- paste(capture.output(print(model$weights)), collapse = "\n")
                        displayContent <- paste0(
                            "State Labels:\n  ",
                            paste(model$labels, collapse = ", "),
                            "\n\nEdge Weight Matrix:\n",
                            matrixStr
                        )
                        self$results$snaModelContent$setContent(displayContent)
                    }

                }, error = function(e) {
                    self$results$errorText$setContent(paste("Error building SNA model:", e$message))
                    self$results$errorText$setVisible(TRUE)
                    return()
                })
            }

            # Set visibility of model outputs
            if(!is.null(model) || !is.null(groupModels)) {
                self$results$sna_plot$setVisible(self$options$sna_show_plot)

                # Set dynamic plot size
                self$results$sna_plot$setSize(
                    self$options$sna_plot_width,
                    self$options$sna_plot_height
                )
            }

            ### Graph Level Metrics Section

            if(!is.null(model) && (self$options$sna_show_summary || self$options$sna_show_degree_plot)) {
                self$results$graphMetricsTitle$setVisible(TRUE)
            }

            ### Summary Table

            if(!is.null(model) && self$options$sna_show_summary) {
                # Get summary as dataframe
                summaryDf <- summary(model)

                # Add rows from summary
                for(i in 1:nrow(summaryDf)) {
                    self$results$summaryTable$addRow(rowKey=i, values=list(
                        metric = as.character(summaryDf[i, "metric"]),
                        value = as.numeric(summaryDf[i, "value"])
                    ))
                }

                self$results$summaryTable$setVisible(TRUE)
            }

            ### Degree Distribution Plot

            if(!is.null(model) && self$options$sna_show_degree_plot) {
                self$results$degree_plot$setVisible(TRUE)
            }

            ### Centrality

            if(!is.null(model) && self$options$centrality_show_table) {

                # Build measures vector
                measures <- character(0)
                if(self$options$centrality_OutStrength) measures <- c(measures, "OutStrength")
                if(self$options$centrality_InStrength) measures <- c(measures, "InStrength")
                if(self$options$centrality_Closeness) measures <- c(measures, "Closeness")
                if(self$options$centrality_ClosenessIn) measures <- c(measures, "ClosenessIn")
                if(self$options$centrality_ClosenessOut) measures <- c(measures, "ClosenessOut")
                if(self$options$centrality_Betweenness) measures <- c(measures, "Betweenness")
                if(self$options$centrality_BetweennessRSP) measures <- c(measures, "BetweennessRSP")
                if(self$options$centrality_Diffusion) measures <- c(measures, "Diffusion")
                if(self$options$centrality_Clustering) measures <- c(measures, "Clustering")

                if(length(measures) > 0) {
                    # Get centralities dataframe
                    cent <- tna::centralities(x=model, loops=self$options$centrality_loops,
                                        normalize=self$options$centrality_normalize, measures=measures)

                    # Add columns
                    for(m in measures) {
                        self$results$centralityTable$addColumn(name=m, type="number")
                    }

                    # Add rows from dataframe
                    for(i in 1:nrow(cent)) {
                        rowData <- as.list(cent[i, ])
                        rowData$state <- as.character(cent[i, "state"])
                        self$results$centralityTable$addRow(rowKey=i, values=rowData)
                    }
                }

                self$results$centralityTitle$setVisible(TRUE)
                self$results$centralityTable$setVisible(TRUE)
            }

            ### Community Detection

            if(!is.null(model) && self$options$community_show_table) {

                # Build methods vector
                methods <- character(0)
                if(self$options$community_walktrap) methods <- c(methods, "walktrap")
                if(self$options$community_fast_greedy) methods <- c(methods, "fast_greedy")
                if(self$options$community_label_prop) methods <- c(methods, "label_prop")
                if(self$options$community_infomap) methods <- c(methods, "infomap")
                if(self$options$community_edge_betweenness) methods <- c(methods, "edge_betweenness")
                if(self$options$community_leading_eigen) methods <- c(methods, "leading_eigen")
                if(self$options$community_spinglass) methods <- c(methods, "spinglass")

                if(length(methods) > 0) {
                    # Get communities
                    comm <- tna::communities(x=model, methods=methods)

                    # Add columns for each method
                    for(m in methods) {
                        self$results$communityTable$addColumn(name=m, title=m, type="integer")
                    }

                    # Get assignments dataframe
                    assignments <- comm$assignments

                    # Add rows
                    for(i in 1:nrow(assignments)) {
                        rowData <- list(state = rownames(assignments)[i])
                        for(m in methods) {
                            rowData[[m]] <- as.integer(assignments[i, m])
                        }
                        self$results$communityTable$addRow(rowKey=i, values=rowData)
                    }
                }

                self$results$communityTitle$setVisible(TRUE)
                self$results$communityTable$setVisible(TRUE)
            }

            ### Edge Betweenness

            if(!is.null(model) && self$options$edge_betweenness_show_table) {

                # Get edge betweenness network
                ebNetwork <- tna::betweenness_network(x=model, directed=self$options$edge_betweenness_directed)

                # Get the weights matrix (which now contains betweenness values)
                ebMatrix <- ebNetwork$weights

                # Convert to edge list
                rowIdx <- 1
                for(i in 1:nrow(ebMatrix)) {
                    for(j in 1:ncol(ebMatrix)) {
                        if(ebMatrix[i, j] > 0) {
                            self$results$edgeBetweennessTable$addRow(rowKey=rowIdx, values=list(
                                from = rownames(ebMatrix)[i],
                                to = colnames(ebMatrix)[j],
                                betweenness = as.numeric(ebMatrix[i, j])
                            ))
                            rowIdx <- rowIdx + 1
                        }
                    }
                }

                self$results$edgeBetweennessTitle$setVisible(TRUE)
                self$results$edgeBetweennessTable$setVisible(TRUE)
            }

        },
        .showSNAPlot=function(image, ...) {
            state <- self$results$snaModelContent$state

            if(is.null(state) || !self$options$sna_show_plot) {
                return(TRUE)
            }

            tryCatch({
                # Handle edge color
                edgeCol <- if(self$options$sna_plot_edge_color == "default") NULL else self$options$sna_plot_edge_color

                # Handle labels - use FALSE to hide, or size value to show
                showNodeLabels <- self$options$sna_plot_show_node_labels
                showEdgeLabels <- self$options$sna_plot_show_edge_labels

                # Build plot arguments
                plotArgs <- list(
                    cut = self$options$sna_plot_cut,
                    minimum = self$options$sna_plot_min_value,
                    layout = self$options$sna_plot_layout,
                    bg = "transparent",
                    # Quick controls (sliders)
                    vsize = self$options$sna_plot_node_size_slider,
                    esize = self$options$sna_plot_edge_size_slider,
                    # Labels
                    labels = showNodeLabels,
                    edge.labels = showEdgeLabels,
                    edge.label.cex = self$options$sna_plot_edge_label_size,
                    label.cex = self$options$sna_plot_node_label_size,
                    # Advanced node settings
                    node.width = self$options$sna_plot_node_size,
                    borders = self$options$sna_plot_borders,
                    border.width = self$options$sna_plot_border_width,
                    # Advanced edge settings
                    asize = self$options$sna_plot_asize,
                    # Layout
                    repulsion = self$options$sna_plot_repulsion,
                    # Colors
                    theme = self$options$sna_plot_theme,
                    # Display
                    details = self$options$sna_plot_details
                )

                # Add curve parameters if curvature is set
                if(self$options$sna_plot_curve > 0) {
                    plotArgs$curve <- self$options$sna_plot_curve
                    plotArgs$curveAll <- self$options$sna_plot_curveAll
                }

                # Add edge color if not default
                if(!is.null(edgeCol)) {
                    plotArgs$edge.color <- edgeCol
                }

                # Check if we have group models
                if(is.list(state) && !is.null(state$groupModels)) {
                    groupModels <- state$groupModels
                    nGroups <- length(groupModels)

                    # Calculate grid layout
                    nCols <- ceiling(sqrt(nGroups))
                    nRows <- ceiling(nGroups / nCols)

                    # Set up multi-panel plot
                    par(mfrow = c(nRows, nCols), mar = c(2, 2, 3, 2))

                    for(g in names(groupModels)) {
                        plotArgs$x <- groupModels[[g]]
                        plotArgs$title <- g
                        do.call(plot, plotArgs)
                    }

                    # Reset par
                    par(mfrow = c(1, 1))
                } else {
                    # Single model plot
                    plotArgs$x <- state
                    do.call(plot, plotArgs)
                }

            }, error = function(e) {
                self$results$errorText$setContent(paste0("Plot error: ", e$message))
                self$results$errorText$setVisible(TRUE)
            })

            TRUE
        },
        .showDegreePlot=function(image, ...) {
            state <- self$results$snaModelContent$state

            if(is.null(state) || !self$options$sna_show_degree_plot) {
                return(FALSE)
            }

            # Check if we have group models
            if(is.list(state) && !is.null(state$groupModels)) {
                # Use first group model for degree plot
                model <- state$groupModels[[1]]
            } else {
                model <- state
            }

            # Plot degree distribution
            print(hist(model))

            TRUE
        }
    )
)
