# This file is a generated template, your changes will not be overwritten

TNAClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "TNAClass",
    inherit = TNABase,
    private = list(
        .run = function() {

            library("tna")
            library("codyna")

            # Set instructions content
            self$results$instructions$setContent(
                '<div style="border: 2px solid #e6f4fe; border-radius: 15px; padding: 15px; background-color: #e6f4fe; margin-top: 10px;">
                <div style="text-align:justify;">
                <ul>
                  <li>Data should be in <b>long format</b> with one row per event/action.</li>
                  <li><b>Action</b>: Column containing the actions/states/events (required). Each unique value becomes a node.</li>
                  <li><b>Actor</b>: Column identifying individuals (optional). Separates sequences by person.</li>
                  <li><b>Time</b> or <b>Order</b>: For ordering events chronologically (optional, use one or the other).</li>
                  <li>Learn more: <a href="https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html" target="_blank">TNA Chapter</a> | <a href="https://sonsoles.me/posts/tna-tutorial" target="_blank">Updated Tutorial</a></li>
                </ul>
                </div>
                </div>'
            )

            type <- self$options$buildModel_type
            scaling <- self$options$buildModel_scaling

            ### Build Model

            # Check if action variable is provided, if not, hide error and return early
            if(is.null(self$options$buildModel_variables_long_action)) {
                self$results$errorText$setVisible(FALSE)
                return()
            }

            model <- NULL

            if( !self$options$buildModel_show_matrix && 
                !self$options$buildModel_show_plot && 
                !self$options$buildModel_show_histo && 
                (    
                    !self$options$buildModel_show_mosaic ||
                    (self$options$buildModel_show_mosaic && self$options$buildModel_type == "relative") )    
                )
            {
                self$results$buildModelTitle$setVisible(FALSE)
            }

            dataForTNA <- NULL

            if(self$results$buildModelContent$isFilled()) {
                model <- self$results$buildModelContent$state
            }
            else if(!is.null(self$data) && ncol(self$data) >= 1) {

                # Wrap data preparation in error handling
                tryCatch({

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
                        self$options$buildModel_variables_long_order
                    )

                    longData <- copyData[columnToUseLong]

                    if(ncol(longData) > 0) {
                        actorColumn <- self$options$buildModel_variables_long_actor
                        timeColumn <- self$options$buildModel_variables_long_time
                        actionColumn <- self$options$buildModel_variables_long_action
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

                        if(type == "attention") {
                            lambda <- self$options$buildModel_lambda
                            model <- tna::build_model(x=dataForTNA, type=type, scaling=scaling, lambda=lambda)
                        } else {
                            model <- tna::build_model(x=dataForTNA, type=type, scaling=scaling)
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

                # Store dataForTNA for Pattern Discovery
                if(!is.null(dataForTNA) && !self$results$patternTable$isFilled()) {
                    self$results$patternTable$setState(dataForTNA)
                }
                # Retrieve dataForTNA if it was stored
                if(is.null(dataForTNA) && !is.null(self$results$patternTable$state)) {
                    dataForTNA <- self$results$patternTable$state
                }
                self$results$buildModelContent$setVisible(self$options$buildModel_show_matrix)

                self$results$buildModel_plot$setVisible(self$options$buildModel_show_plot) # plot
                
                self$results$buildModel_histo$setVisible(self$options$buildModel_show_histo) # plot
                
                self$results$buildModel_frequencies$setVisible(self$options$buildModel_show_frequencies) # plot
                
                self$results$buildModel_mosaic$setVisible(self$options$buildModel_show_mosaic) # plot

            }

            ### Centrality

            if(!is.null(model) && (self$options$centrality_show_table || self$options$centrality_show_plot) ) {
                centrality_loops <- self$options$centrality_loops
                centrality_normalize <- self$options$centrality_normalize

                vectorCharacter <- character(0)    

                fullTable <- self$results$centralityTable$isFilled()             


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
                if(length(vectorCharacter) > 0 && !is.null(model) &&
                    (!self$results$centrality_plot$isFilled() || !fullTable))
                {
                    cent <- tna::centralities(x=model, loops=centrality_loops, normalize=centrality_normalize, measures=vectorCharacter)
                    self$results$centralityTable$setState(cent)
                }

                # Check if cent is valid before iterating
                if(is.null(cent) || !is.data.frame(cent) || nrow(cent) == 0) {
                    self$results$centralityTitle$setVisible(FALSE)
                } else {
                for (i in 1:nrow(cent)) {
                    rowValues <- list()
                    rowValues$state <- as.character(cent[i, "state"])
                    for (measure in vectorCharacter) {
                        if (measure %in% colnames(cent)) {
                            rowValues[[measure]] <- as.numeric(cent[i, measure])
                        }
                    }
                    self$results$centralityTable$addRow(rowKey=i, values=rowValues)
                }
                self$results$centralityTitle$setVisible(self$options$centrality_show_plot || self$options$centrality_show_table)
                self$results$centrality_plot$setVisible(self$options$centrality_show_plot)
                self$results$centralityTable$setVisible(self$options$centrality_show_table)
                }

            }

            ### Centrality Stability

            if(!is.null(model) && (self$options$centrality_stability_show_table || self$options$centrality_stability_show_plot)) {

                # Build measures vector based on selected stability measures
                stabilityMeasures <- character(0)
                if(self$options$centrality_stability_InStrength) stabilityMeasures <- c(stabilityMeasures, "InStrength")
                if(self$options$centrality_stability_OutStrength) stabilityMeasures <- c(stabilityMeasures, "OutStrength")
                if(self$options$centrality_stability_Betweenness) stabilityMeasures <- c(stabilityMeasures, "Betweenness")

                # Default to basic measures if none selected
                if(length(stabilityMeasures) == 0) {
                    stabilityMeasures <- c("InStrength", "OutStrength", "Betweenness")
                }

                # Check if we need to compute stability
                csResult <- self$results$centrality_stability_plot$state
                if(is.null(csResult)) {

                    csResult <- tryCatch({
                        tna::estimate_cs(
                            x = model,
                            loops = self$options$centrality_loops,
                            normalize = self$options$centrality_normalize,
                            measures = stabilityMeasures,
                            iter = self$options$centrality_stability_iteration,
                            threshold = self$options$centrality_stability_threshold,
                            certainty = self$options$centrality_stability_certainty,
                            progressbar = FALSE
                        )
                    }, error = function(e) {
                        self$results$errorText$setContent(paste("Centrality stability error:", e$message))
                        self$results$errorText$setVisible(TRUE)
                        NULL
                    })

                    if(!is.null(csResult)) {
                        self$results$centrality_stability_plot$setState(csResult)
                    }
                }

                # Populate stability table with CS-coefficients
                if(!is.null(csResult) && self$options$centrality_stability_show_table) {
                    for(measure in names(csResult)) {
                        if(!is.null(csResult[[measure]]$cs_coefficient)) {
                            self$results$centralityStabilityTable$addRow(rowKey=measure, values=list(
                                measure = measure,
                                cs_coefficient = csResult[[measure]]$cs_coefficient
                            ))
                        }
                    }
                }

                self$results$centralityStabilityTitle$setVisible(self$options$centrality_stability_show_table || self$options$centrality_stability_show_plot)
                self$results$centralityStabilityTable$setVisible(self$options$centrality_stability_show_table)
                self$results$centrality_stability_plot$setVisible(self$options$centrality_stability_show_plot)
            }

            ### Edge betweenness

            if(!is.null(model) && (self$options$edgeBetweenness_show_table || self$options$edgeBetweenness_show_plot)) {
                edgeBetwenness <- tna::betweenness_network(x=model)

                # Plot
                if(!self$results$edgeBetweenness_plot$isFilled()) {
                    self$results$edgeBetweenness_plot$setState(edgeBetwenness)
                }
                
                # Table - show weights
                matrix_to_from_to_value <- function(your_matrix) {
                  if (is.null(rownames(your_matrix))) {
                    rownames(your_matrix) <- paste0("Row", 1:nrow(your_matrix))
                  }
                  if (is.null(colnames(your_matrix))) {
                    colnames(your_matrix) <- paste0("Col", 1:ncol(your_matrix))
                  }
                  df <- setNames(as.data.frame(as.table(your_matrix)), c("from", "to", "value"))
                  return(df)
                }

                if(!is.null(edgeBetwenness) && !is.null(edgeBetwenness$weights)) {
                    betable <- matrix_to_from_to_value(edgeBetwenness$weights)

                    betable <- betable[betable$value > 0, ]

                    betable <- betable[order(-betable$value), ]

                    for (i in 1:nrow(betable)) {
                        self$results$edgeBetweennessTable$addRow(rowKey=i, values=list(
                            from=as.character(betable[i, "from"]),
                            to=as.character(betable[i, "to"]),
                            value=betable[i, "value"]
                        ))
                    }

                    has_results_to_display <- nrow(betable) > 0
                    self$results$edgeBetweennessTable$setVisible(self$options$edgeBetweenness_show_table && has_results_to_display)
                    self$results$edgeBetweennessNoResultsNote$setVisible(self$options$edgeBetweenness_show_table && !has_results_to_display)
                    if (self$options$edgeBetweenness_show_table && !has_results_to_display) {
                        self$results$edgeBetweennessNoResultsNote$setContent("No weights available for Edge Betweenness based on current filters.")
                    } else {
                        self$results$edgeBetweennessNoResultsNote$setContent("") # Clear the note if results are displayed
                    }

                } else {
                    self$results$edgeBetweennessTable$setVisible(FALSE)
                    self$results$edgeBetweennessNoResultsNote$setVisible(self$options$edgeBetweenness_show_table)
                    self$results$edgeBetweennessNoResultsNote$setContent("No weights available for Edge Betweenness.")
                }
                
                # Set plot visibility
                self$results$edgeBetweenness_plot$setVisible(self$options$edgeBetweenness_show_plot)
                self$results$edgeBetweennessTitle$setVisible(self$options$edgeBetweenness_show_table || self$options$edgeBetweenness_show_plot)
            }


            ### Community

            if(!is.null(model) && (self$options$community_show_table || self$options$community_show_plot)) {
                community_gamma <- as.numeric(self$options$community_gamma)
                methods <- self$options$community_methods

                # Initialize coms variable to avoid scope issues
                coms <- self$results$community_plot$state
                
                if((!self$results$communityContent$isFilled() || !self$results$community_plot$isFilled())) { 

                    if(is.null(coms)) {
                        coms <- NULL
                    }
                    
                    resultComs <- tryCatch({
                        coms <- tna::communities(x=model, methods=methods, gamma=community_gamma)
                        TRUE
                    }, error = function(e) {
                        self$results$communityTitle$setVisible(TRUE)
                        self$results$communityErrorText$setContent(paste("The methods", methods, "should be change :\n\t", conditionMessage(e)) )
                        self$results$communityErrorText$setVisible(TRUE)
                        FALSE
                    })
                    
                    if(!resultComs) {
                        return()
                    }

                    if(!self$results$community_plot$isFilled()) {
                        self$results$community_plot$setState(coms)
                    }
                    if(!self$results$communityContent$isFilled()) {
                        self$results$communityContent$setContent(coms)
                    }
                    
                }

                # Populate communities table
                if(!is.null(coms) && self$options$community_show_table) {
                    if(!is.null(coms$assignments)) {
                        assignments <- coms$assignments
                        
                        # Add columns for each community detection method
                        method_names <- colnames(assignments)[-1]  # Exclude 'state' column
                        for(method in method_names) {
                            self$results$communityTable$addColumn(name=method, title=method, type="integer")
                        }
                        
                        # Add rows with community assignments
                        for (i in 1:nrow(assignments)) {
                            rowValues <- list()
                            rowValues$state <- as.character(assignments[i, "state"])
                            
                            for(method in method_names) {
                                rowValues[[method]] <- as.integer(assignments[i, method])
                            }
                            
                            self$results$communityTable$addRow(rowKey=i, values=rowValues)
                        }
                    }
                }
                
                self$results$community_plot$setVisible(self$options$community_show_plot)
                self$results$communityTable$setVisible(self$options$community_show_table)
                self$results$communityContent$setVisible(FALSE)
                self$results$communityTitle$setVisible(self$options$community_show_plot || self$options$community_show_table)

            }

            ### Cliques

            cliques_size <- as.numeric(self$options$cliques_size)
            cliques_threshold <- as.numeric(self$options$cliques_threshold)
            if (cliques_threshold == 0) {
                cliques_threshold <- if (cliques_size <= 2) 0.1 else 0.01
            }

            if(!is.null(model) && ( self$options$cliques_show_text || self$options$cliques_show_plot) ) {

                if(!self$results$cliques_multiple_plot$isFilled() || !self$results$cliquesContent$isFilled()) {
                    cliques <- tna::cliques(x=model, size=cliques_size, threshold=cliques_threshold)
                    n_cliques <- lengths(cliques[1])

                    if (n_cliques == 0) {
                        self$results$cliquesTitle$setContent(
                            paste0("No cliques of size ", cliques_size, " found with threshold ", cliques_threshold,
                                   ". Try lowering the threshold or reducing the clique size."))
                        self$results$cliquesTitle$setVisible(TRUE)
                        self$results$cliques_multiple_plot$setVisible(FALSE)
                        self$results$cliquesContent$setVisible(FALSE)
                    } else {
                        if(!self$results$cliquesContent$isFilled()) {
                            self$results$cliquesContent$setContent(cliques)
                        }

                        if(!self$results$cliques_multiple_plot$isFilled()) {
                            self$results$cliques_multiple_plot$setState(cliques)
                        }
                        self$results$cliques_multiple_plot$setVisible(self$options$cliques_show_plot)
                        self$results$cliquesContent$setVisible(self$options$cliques_show_text)
                        self$results$cliquesTitle$setVisible(self$options$cliques_show_text || self$options$cliques_show_plot)
                    }
                }
            }

                        ### Bootstrap

            if(!is.null(model) && ( self$options$bootstrap_show_plot || self$options$bootstrap_show_table)) {

                # Check if we need to compute bootstrap
                bs <- self$results$bootstrap_plot$state
                if(is.null(bs) || !self$results$bootstrap_plot$isFilled()) {
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

                # Populate bootstrap table
                if(!is.null(bs) && !is.null(bs$summary) && self$options$bootstrap_show_table) {
                    all_edges <- bs$summary
                    if(nrow(all_edges) > 0) {
                        # Sort by significance (TRUE/significant first, then FALSE/non-significant)
                        all_edges <- all_edges[order(-all_edges$sig), ]

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
                            self$results$bootstrapTable$addRow(rowKey=i, values=list(
                                from=as.character(all_edges[i, "from"]),
                                to=as.character(all_edges[i, "to"]),
                                weight=all_edges[i, "weight"],
                                p_value=all_edges[i, "p_value"],
                                cr_lower=all_edges[i, "cr_lower"],
                                cr_upper=all_edges[i, "cr_upper"],
                                ci_lower=all_edges[i, "ci_lower"],
                                ci_upper=all_edges[i, "ci_upper"],
                                significant=ifelse(all_edges[i, "sig"], "Yes", "No")
                            ))
                        }
                    }
                }
                
                self$results$bootstrap_plot$setVisible(self$options$bootstrap_show_plot)
                self$results$bootstrapTable$setVisible(self$options$bootstrap_show_table)
                self$results$bootstrapTitle$setVisible(self$options$bootstrap_show_plot || self$options$bootstrap_show_table)

            }

            ### Sequence Analysis

            if(!is.null(model) && self$options$sequences_show_plot) {
                # Store model for plot rendering
                if(!self$results$sequences_plot$isFilled()) {
                    self$results$sequences_plot$setState(model)
                }
                self$results$sequences_plot$setVisible(TRUE)
            }

            ### Pattern Discovery

            if(self$options$pattern_show_table) {
                # Always show title when checkbox is checked
                self$results$patternTitle$setContent("Pattern Discovery Running...")
                self$results$patternTitle$setVisible(TRUE)

                tryCatch({
                    # Re-prepare data if dataForTNA is NULL (e.g., when model is cached)
                    if(is.null(dataForTNA) && !is.null(self$data) && ncol(self$data) >= 1) {
                        copyData <- self$data
                        copyData[[self$options$buildModel_variables_long_action]] <- as.character(copyData[[self$options$buildModel_variables_long_action]])

                        if(!is.null(self$options$buildModel_variables_long_actor)) {
                            copyData[[self$options$buildModel_variables_long_actor]] <- as.character(copyData[[self$options$buildModel_variables_long_actor]])
                        }

                        columnToUseLong <- c(
                            self$options$buildModel_variables_long_time,
                            self$options$buildModel_variables_long_actor,
                            self$options$buildModel_variables_long_action,
                            self$options$buildModel_variables_long_order
                        )
                        longData <- copyData[columnToUseLong]

                        args_prepare_data <- list(
                            data = longData,
                            actor = self$options$buildModel_variables_long_actor,
                            time = self$options$buildModel_variables_long_time,
                            action = self$options$buildModel_variables_long_action,
                            time_threshold = self$options$buildModel_threshold,
                            order = self$options$buildModel_variables_long_order
                        )
                        args_prepare_data <- args_prepare_data[!sapply(args_prepare_data, is.null)]
                        dataForTNA <- do.call(tna::prepare_data, args_prepare_data)
                    }

                    if(is.null(dataForTNA)) {
                        self$results$patternTitle$setContent("ERROR: Could not prepare data")
                        return()
                    }
                    if(is.null(dataForTNA$sequence_data)) {
                        self$results$patternTitle$setContent(paste("ERROR: sequence_data is NULL. Names:", paste(names(dataForTNA), collapse=", ")))
                        return()
                    }

                    # Get sequence data in wide format from prepared data
                    seq_data <- dataForTNA$sequence_data

                    # Determine pattern type and parameters
                    pattern_type <- self$options$pattern_type

                    # Build arguments for discover_patterns
                    custom_pattern <- self$options$pattern_custom
                    if(pattern_type == "custom" && !is.null(custom_pattern) && nchar(custom_pattern) > 0) {
                        # Custom pattern search
                        patterns <- codyna::discover_patterns(
                            data = seq_data,
                            pattern = self$options$pattern_custom,
                            min_support = self$options$pattern_min_support,
                            min_count = self$options$pattern_min_count
                        )
                    } else {
                        # Type-based pattern discovery
                        len_range <- self$options$pattern_len_min:self$options$pattern_len_max
                        gap_range <- self$options$pattern_gap_min:self$options$pattern_gap_max

                        patterns <- codyna::discover_patterns(
                            data = seq_data,
                            type = pattern_type,
                            len = len_range,
                            gap = gap_range,
                            min_support = self$options$pattern_min_support,
                            min_count = self$options$pattern_min_count
                        )
                    }

                    # Apply optional filters (Level type returns NULL when not selected)
                    if(!is.null(self$options$pattern_starts_with)) {
                        patterns <- patterns[startsWith(patterns$pattern, self$options$pattern_starts_with), ]
                    }
                    if(!is.null(self$options$pattern_ends_with)) {
                        patterns <- patterns[endsWith(patterns$pattern, self$options$pattern_ends_with), ]
                    }
                    # Contains filter supports multiple values (Levels type returns vector)
                    if(!is.null(self$options$pattern_contains) && length(self$options$pattern_contains) > 0) {
                        # Pattern must contain ALL selected values
                        keep <- rep(TRUE, nrow(patterns))
                        for(val in self$options$pattern_contains) {
                            keep <- keep & grepl(val, patterns$pattern, fixed=TRUE)
                        }
                        patterns <- patterns[keep, ]
                    }

                    # Populate table with row limit
                    if(!is.null(patterns) && nrow(patterns) > 0) {
                        total_patterns <- nrow(patterns)

                        # Apply row limit unless show_all is checked
                        if (!isTRUE(self$options$pattern_table_show_all)) {
                            max_rows <- self$options$pattern_table_max_rows
                            if (nrow(patterns) > max_rows) {
                                patterns <- patterns[1:max_rows, ]
                            }
                        }

                        for(i in 1:nrow(patterns)) {
                            self$results$patternTable$addRow(rowKey=i, values=list(
                                pattern = as.character(patterns$pattern[i]),
                                length = as.integer(patterns$length[i]),
                                count = as.integer(patterns$count[i]),
                                proportion = patterns$proportion[i],
                                support = patterns$support[i]
                            ))
                        }

                        # Show count in title
                        if (nrow(patterns) < total_patterns) {
                            self$results$patternTitle$setContent(paste("Showing", nrow(patterns), "of", total_patterns, "patterns"))
                        } else {
                            self$results$patternTitle$setContent(paste("Found", total_patterns, "patterns"))
                        }
                    }

                    self$results$patternTable$setVisible(TRUE)

                }, error = function(e) {
                    self$results$patternTitle$setContent(paste("Pattern Discovery error:", e$message))
                })
            }

            ### Sequence Indices
            if(self$options$indices_show_table) {
                self$results$indicesTitle$setContent("Step 1: Starting...")
                self$results$indicesTitle$setVisible(TRUE)

                tryCatch({
                    self$results$indicesTitle$setContent("Step 2: Checking dataForTNA...")

                    # Re-prepare data if dataForTNA is NULL (when model is cached)
                    if(is.null(dataForTNA) && !is.null(self$data) && ncol(self$data) >= 1) {
                        self$results$indicesTitle$setContent("Step 3: Preparing data...")
                        action_col <- self$options$buildModel_variables_long_action
                        actor_col <- self$options$buildModel_variables_long_actor
                        time_col <- self$options$buildModel_variables_long_time
                        order_col <- self$options$buildModel_variables_long_order

                        # Check action_col is valid
                        if(is.null(action_col) || length(action_col) == 0) {
                            self$results$indicesTitle$setContent("Error: Action variable is required")
                            return()
                        }

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

                        self$results$indicesTitle$setContent("Step 4: Calling prepare_data...")
                        dataForTNA <- do.call(tna::prepare_data, args_prepare_data)
                    }

                    if(is.null(dataForTNA)) {
                        self$results$indicesTitle$setContent("Error: dataForTNA is NULL")
                        return()
                    }

                    self$results$indicesTitle$setContent("Preparing sequence data...")
                    seq_data <- dataForTNA$sequence_data

                    # Convert all columns to character (codyna requires character data)
                    seq_data <- as.data.frame(lapply(seq_data, as.character), stringsAsFactors = FALSE)

                    self$results$indicesTitle$setContent("Calculating indices...")
                    # Call sequence_indices
                    indices <- codyna::sequence_indices(data = seq_data)

                    # Add sequence ID
                    indices$sequence_id <- 1:nrow(indices)

                    # Add actor if provided
                    actor_col <- self$options$buildModel_variables_long_actor
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

                }, error = function(e) {
                    self$results$indicesTitle$setContent(paste("Sequence Indices error:", e$message))
                })
            }

        },
        .showBuildModelPlot=function(image, ...) {
            plotData <- self$results$buildModelContent$state

            if(!is.null(plotData) && self$options$buildModel_show_plot)  {
                tryCatch({
                    plot(x=plotData,
                        cut=self$options$buildModel_plot_cut,
                        minimum=self$options$buildModel_plot_min_value,
                        edge.label.cex=self$options$buildModel_plot_edge_label_size,
                        node.width=self$options$buildModel_plot_node_size,
                        label.cex=self$options$buildModel_plot_node_label_size,
                        layout=self$options$buildModel_plot_layout,
                        bg="transparent"
                    )
                }, error = function(e) {
                    layout_name <- self$options$buildModel_plot_layout
                    self$results$errorText$setContent(paste0("Layout '", layout_name, "' is not available for this network. Please try a different layout."))
                    self$results$errorText$setVisible(TRUE)
                })
            }
            TRUE
        },
        .showBuildModelHisto=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_histo)  {
                tryCatch({
                    par(mfrow = c(1, 1))
                    w <- c(plotData$weights)
                    brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
                    hist(x=plotData, breaks=brks, main="Histogram of Edge Weights (Probabilities)",
                         xlab="Edge Weights (Probabilities)", ylab="Frequency")
                }, error = function(e) {
                    plot(1, type="n", main="Histogram Error", sub=e$message)
                })
            }
            TRUE
        },
        .showBuildModelFrequencies=function(image, ...) {
            plotData <- self$results$buildModelContent$state
            
            if(!is.null(plotData) && self$options$buildModel_show_frequencies)  {
                tryCatch({
                    p <- plot_frequencies(x=plotData)
                    if(!is.null(p)) {
                        print(p)
                    }
                }, error = function(e) {
                    w <- c(plotData$weights)
                    brks <- seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20)
                    hist(x=plotData, breaks=brks, main="Frequencies Plot",
                         xlab="Edge Weights", ylab="Frequency")
                })
            }   
            TRUE
        },
        .showBuildModelMosaic=function(image, ...) {
            plotData <- self$results$buildModelContent$state

            if(!is.null(plotData) && self$options$buildModel_show_mosaic && self$options$buildModel_type == "frequency")  {
                tryCatch({
                    p <- plot_mosaic(x=plotData, digits=self$options$buildModel_digits)
                    print(p)
                }, error = function(e) {
                    plot(1, type="n", main="Mosaic Plot Error", sub=e$message)
                })
            }
            TRUE
        },
        .showSequencesPlot=function(image, ...) {
            plotData <- self$results$sequences_plot$state

            if(is.null(plotData) || !self$options$sequences_show_plot) {
                return(FALSE)
            }

            tryCatch({
                p <- tna::plot_sequences(
                    x = plotData,
                    type = self$options$sequences_type,
                    scale = self$options$sequences_scale,
                    geom = self$options$sequences_geom,
                    include_na = self$options$sequences_include_na,
                    tick = self$options$sequences_tick
                )
                print(p)
                TRUE
            }, error = function(e) {
                self$results$errorText$setContent(paste("Sequence plot error:", e$message))
                self$results$errorText$setVisible(TRUE)
                FALSE
            })
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
            }
            TRUE
        },
        .showCentralityStabilityPlot=function(image, ...) {
            plotData <- self$results$centrality_stability_plot$state

            if(is.null(plotData) || !self$options$centrality_stability_show_plot) {
                return(FALSE)
            }

            tryCatch({
                p <- plot(plotData)
                print(p)
                TRUE
            }, error = function(e) {
                self$results$errorText$setContent(paste("Centrality stability plot error:", e$message))
                self$results$errorText$setVisible(TRUE)
                FALSE
            })
        },
        .showEdgeBetweennessPlot=function(image, ...) {
            plotData <- self$results$edgeBetweenness_plot$state
            
            if(!is.null(plotData) && self$options$edgeBetweenness_show_plot)  {
                tryCatch({
                    plot(
                        x=plotData,
                        cut=self$options$edgeBetweenness_plot_cut,
                        minimum=self$options$edgeBetweenness_plot_min_value,
                        edge.label.cex=self$options$edgeBetweenness_plot_edge_label_size,
                        node.width=self$options$edgeBetweenness_plot_node_size,
                        label.cex=self$options$edgeBetweenness_plot_node_label_size,
                        layout=self$options$edgeBetweenness_plot_layout,
                        bg="transparent"
                    )
                }, error = function(e) {
                    plot(1, type="n", main="Edge Betweenness Plot Error", sub=e$message)
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
                    methods <- self$options$community_methods
                    plot(x=plotData, method=methods, bg="transparent")
                }, error = function(e) {
                    plot(1, type="n", main="Community Plot Error", sub=e$message)
                })
            }
            TRUE
        },
        .showBootstrapPlot=function(image, ...) {
            plotData <- self$results$bootstrap_plot$state
            if(!is.null(plotData) && self$options$bootstrap_show_plot)  {
                tryCatch({
                    plot(x=plotData, cut = 0.01)
                }, error = function(e) {
                    plot(1, type="n", main="Bootstrap Plot Error", sub=e$message)
                })
            }
            TRUE
        },
        .showCliquesMultiPlot=function(image, ...) {
            plotData <- self$results$cliques_multiple_plot$state
            if (!is.null(plotData) && self$options$cliques_show_plot) {
                tryCatch({
                    n_cliques <- lengths(plotData[1])
                    if (n_cliques == 0) return(FALSE)
                    ncol <- ceiling(sqrt(n_cliques))
                    nrow <- ceiling(n_cliques / ncol)
                    par(mfrow = c(nrow, ncol), mar = c(2, 2, 3, 1))
                    for (i in seq_len(n_cliques)) {
                        plot(x=plotData, ask=FALSE, first=i, n=1,
                            cut=self$options$cliques_plot_cut,
                            minimum=self$options$cliques_plot_min_value,
                            edge.label.cex=self$options$cliques_plot_edge_label_size,
                            node.width=self$options$cliques_plot_node_size,
                            label.cex=self$options$cliques_plot_node_label_size,
                            layout=self$options$cliques_plot_layout,
                            bg="transparent")
                    }
                }, error = function(e) {
                    plot(1, type="n", main="Cliques Plot Error", sub=e$message)
                })
                TRUE
            } else {
                FALSE
            }
        }
    )
)