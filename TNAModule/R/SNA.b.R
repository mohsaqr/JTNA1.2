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

            # Hide title if no outputs selected
            if(!self$options$sna_show_matrix && !self$options$sna_show_plot) {
                self$results$snaModelTitle$setVisible(FALSE)
            }

            # Check if model is already built (cached)
            if(self$results$snaModelContent$isFilled()) {
                model <- self$results$snaModelContent$state
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

                    df <- data.frame(
                        from = from_col,
                        to = to_col,
                        weight = weight_col,
                        stringsAsFactors = FALSE
                    )

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

                    # Build SNA model using tna::sna()
                    model <- tna::sna(df, aggregate = agg_fn)

                    # Store model in state
                    self$results$snaModelContent$setState(model)

                    # Create a string representation of the weights matrix for display
                    # (avoid using print(model) which has a bug with NULL inits)
                    matrixStr <- paste(capture.output(print(model$weights)), collapse = "\n")
                    displayContent <- paste0(
                        "State Labels:\n  ",
                        paste(model$labels, collapse = ", "),
                        "\n\nEdge Weight Matrix:\n",
                        matrixStr
                    )
                    self$results$snaModelContent$setContent(displayContent)

                }, error = function(e) {
                    self$results$errorText$setContent(paste("Error building SNA model:", e$message))
                    self$results$errorText$setVisible(TRUE)
                    return()
                })
            }

            # Set visibility of model outputs
            if(!is.null(model)) {
                self$results$snaModelContent$setVisible(self$options$sna_show_matrix)
                self$results$sna_plot$setVisible(self$options$sna_show_plot)

                # Set dynamic plot size
                self$results$sna_plot$setSize(
                    self$options$sna_plot_width,
                    self$options$sna_plot_height
                )
            }

            ### Centrality Analysis

            if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot)) {

                centrality_loops <- self$options$centrality_loops
                centrality_normalize <- self$options$centrality_normalize

                vectorCharacter <- character(0)
                fullTable <- self$results$centralityTable$isFilled()

                # Build measures vector based on selected options
                if(self$options$centrality_OutStrength) {
                    vectorCharacter <- append(vectorCharacter, "OutStrength")
                    self$results$centralityTable$addColumn(name="OutStrength", type="number")
                }
                if(self$options$centrality_InStrength) {
                    vectorCharacter <- append(vectorCharacter, "InStrength")
                    self$results$centralityTable$addColumn(name="InStrength", type="number")
                }
                if(self$options$centrality_Betweenness) {
                    vectorCharacter <- append(vectorCharacter, "Betweenness")
                    self$results$centralityTable$addColumn(name="Betweenness", type="integer")
                }

                # Calculate centrality measures
                cent <- self$results$centralityTable$state
                if(length(vectorCharacter) > 0 && !is.null(model) &&
                    (!self$results$centrality_plot$isFilled() || !fullTable))
                {
                    cent <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
                    self$results$centralityTable$setState(cent)
                }

                # Populate centrality table
                if(!is.null(cent) && nrow(cent) > 0) {
                    for (i in 1:nrow(cent)) {
                        index <- 1
                        rowValues <- list()

                        rowValues$state <- as.character(cent[i, index])

                        if ("OutStrength" %in% vectorCharacter) {
                            index <- index + 1
                            rowValues$OutStrength <- as.numeric(cent[i, index])
                        }
                        if ("InStrength" %in% vectorCharacter) {
                            index <- index + 1
                            rowValues$InStrength <- as.numeric(cent[i, index])
                        }
                        if ("Betweenness" %in% vectorCharacter) {
                            index <- index + 1
                            rowValues$Betweenness <- as.numeric(cent[i, index])
                        }
                        self$results$centralityTable$addRow(rowKey=i, values=rowValues)
                    }
                }

                # Set visibility
                self$results$centralityTitle$setVisible(self$options$centrality_show_plot || self$options$centrality_show_table)
                self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
                self$results$centralityTable$setVisible(self$options$centrality_show_table)
            }

        },
        .showSNAPlot=function(image, ...) {
            plotData <- self$results$snaModelContent$state

            if(!is.null(plotData) && self$options$sna_show_plot) {
                tryCatch({
                    # Handle positive edge color
                    posCol <- if(self$options$sna_plot_posCol == "default") NULL else self$options$sna_plot_posCol

                    # Handle negative edge color
                    negCol <- if(self$options$sna_plot_negCol == "default") NULL else self$options$sna_plot_negCol

                    # Build plot arguments
                    plotArgs <- list(
                        x = plotData,
                        cut = self$options$sna_plot_cut,
                        minimum = self$options$sna_plot_min_value,
                        edge.label.cex = self$options$sna_plot_edge_label_size,
                        node.width = self$options$sna_plot_node_size,
                        label.cex = self$options$sna_plot_node_label_size,
                        layout = self$options$sna_plot_layout,
                        bg = "transparent",
                        # New parameters
                        repulsion = self$options$sna_plot_repulsion,
                        vsize = self$options$sna_plot_vsize,
                        borders = self$options$sna_plot_borders,
                        border.width = self$options$sna_plot_border_width,
                        esize = self$options$sna_plot_esize,
                        asize = self$options$sna_plot_asize,
                        theme = self$options$sna_plot_theme,
                        details = self$options$sna_plot_details
                    )

                    # Add curve parameters if curvature is set
                    if(self$options$sna_plot_curve > 0) {
                        plotArgs$curve <- self$options$sna_plot_curve
                        plotArgs$curveAll <- self$options$sna_plot_curveAll
                    }

                    # Add color parameters if not default
                    if(!is.null(posCol)) plotArgs$posCol <- posCol
                    if(!is.null(negCol)) plotArgs$negCol <- negCol

                    # Execute plot
                    do.call(plot, plotArgs)

                }, error = function(e) {
                    layout_name <- self$options$sna_plot_layout
                    self$results$errorText$setContent(paste0("Plot error: ", e$message))
                    self$results$errorText$setVisible(TRUE)
                })
            }
            TRUE
        },
        .showCentralityPlot=function(image, ...) {
            plotData <- self$results$centralityTable$state

            if(!is.null(plotData) && self$options$centrality_show_plot) {
                centPlot <- plot(plotData)
                print(centPlot)
            }
            TRUE
        }
    )
)
