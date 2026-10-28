# Results sections shared by TNA, GroupTNA and ClusterTNA.
#
# Each .jtna_run_*() fills one results Group (`section`) from a model. The
# expensive result is cached in that section's state so that cosmetic
# changes do not recompute it; tables are refilled from the cache on every
# run (jamovi only restores rows declared in the .r.yaml, not rows added
# here). `id` is NULL for a single network, or "group" / "cluster" for a
# grouped model, where it names the column identifying each network.
#
# Each .jtna_draw_*() draws one plot from cached state. They are called by
# the analyses' render functions.

.jtna_grouped <- function(x) inherits(x, "group_tna")

# ---- Model --------------------------------------------------------------

# Transition matrices and initial probabilities.
.jtna_run_matrix <- function(section, options, model, id = NULL) {
  if (!isTRUE(options$buildModel_show_matrix)) return(invisible())
  if (is.null(id)) {
    .jtna_matrix_table(section$matrix, model)
    .jtna_fill_table(section$inits, data.frame(state = model$labels,
                                               prob = as.numeric(model$inits)))
    return(invisible())
  }
  invisible(lapply(names(model), \(net) {
    section$matrices$addItem(key = net)
    .jtna_matrix_table(section$matrices$get(key = net), model[[net]])
  }))
  nets <- names(model)
  inits <- data.frame(state = model[[1]]$labels)
  inits[sprintf("net%d", seq_along(nets))] <- lapply(model, \(m) as.numeric(m$inits))
  invisible(lapply(seq_along(nets), \(k) {
    section$inits$addColumn(name = sprintf("net%d", k), title = nets[k], type = "number",
                             superTitle = tools::toTitleCase(id))
  }))
  .jtna_fill_table(section$inits, inits)
}

# One row per "from" state, one column per "to" state. State names come
# from the model's labels: tna's across-group scaling returns weight
# matrices without dimnames.
.jtna_matrix_table <- function(table, model) {
  states <- model$labels
  invisible(lapply(seq_along(states), \(k) {
    table$addColumn(name = sprintf("to%d", k), title = states[k], type = "number", superTitle = "To")
  }))
  df <- as.data.frame(unclass(model$weights), stringsAsFactors = FALSE)
  names(df) <- sprintf("to%d", seq_along(states))
  .jtna_fill_table(table, cbind(from = states, df, stringsAsFactors = FALSE))
}

# A single network's mosaic needs integer weights (tna: frequency models).
.jtna_check_mosaic <- function(section, options) {
  if (isTRUE(options$buildModel_show_mosaic) && !identical(options$buildModel_type, "frequency")) {
    .jtna_notice(options, section,
      "The mosaic plot needs a Frequency model. Set Type to Frequency to draw it.",
      name = "mosaic", type = jmvcore::NoticeType$WARNING)
  }
  invisible()
}

# ---- Network analysis -------------------------------------------------------

.jtna_run_centrality <- function(section, options, model, id = NULL) {
  table <- section$table
  measures <- .jtna_measures(options)
  if (length(measures) == 0L) {
    table$setNote("measures", "Select at least one centrality measure.")
    return(invisible())
  }
  cent <- table$state
  if (is.null(cent)) {
    cent <- tna::centralities(x = model, loops = options$centrality_loops,
                              normalize = options$centrality_normalize,
                              invert = options$centrality_invert, measures = measures)
    table$setState(cent)
  }
  .jtna_fill_table(table, .jtna_centrality_df(cent, measures, id))
}

.jtna_run_community <- function(section, options, model, id = NULL) {
  coms <- section$plot$state
  if (is.null(coms)) {
    coms <- .jtna_with_seed(.jtna_seed(options), \() {
      tna::communities(x = model, methods = options$community_methods,
                       gamma = options$community_gamma)
    })
    section$plot$setState(coms)
  }
  .jtna_fill_table(section$table, .jtna_community_df(coms, options$community_methods, id))
}

.jtna_run_cliques <- function(section, options, model) {
  if (is.null(section$plot$state)) {
    section$plot$setState(tna::cliques(x = model, size = options$cliques_size,
                                       threshold = .jtna_cliques_threshold(options),
                                       sum_weights = options$cliques_sum_weights))
  }
  invisible()
}

# ---- Confirmatory testing ---------------------------------------------------

.jtna_run_bootstrap <- function(section, options, model, id = NULL) {
  bs <- section$plot$state
  if (is.null(bs)) {
    bs <- .jtna_with_seed(.jtna_seed(options), \() tna::bootstrap(
      x = model, iter = options$bootstrap_iteration, level = options$bootstrap_level,
      method = options$bootstrap_method, threshold = options$bootstrap_threshold,
      consistency_range = c(options$bootstrap_range_low, options$bootstrap_range_up)))
    section$plot$setState(bs)
  }
  table <- section$table
  significant_only <- isTRUE(options$bootstrap_table_significant_only)
  df <- .jtna_bootstrap_df(bs, significant_only, id)
  if (nrow(df) == 0L) {
    table$setNote("empty", if (significant_only)
      "No edges were significant at the chosen level." else "No edges to show.")
  }
  df <- .jtna_limit_rows(df, table, options$bootstrap_table_max_rows,
                         options$bootstrap_table_show_all, "edges")
  .jtna_fill_table(table, df)
}

# `unscaled` returns the model to permute: tna does not permute grouped
# models with globally scaled weights, and scaling is a display transform.
.jtna_run_permutation <- function(section, options, model, id, unscaled) {
  perm <- section$plot$state
  if (is.null(perm)) {
    perm <- .jtna_with_seed(.jtna_seed(options), \() tna::permutation_test(
      x = if (length(.jtna_scaling(options)) > 0L) unscaled() else model,
      iter = options$permutation_iter, paired = options$permutation_paired,
      level = options$permutation_level, adjust = options$permutation_adjust))
    section$plot$setState(perm)
  }
  table <- section$table
  df <- .jtna_permutation_df(perm, id)
  if (is.null(df)) {
    table$setNote("empty", "No edges to compare.")
    return(invisible())
  }
  df <- .jtna_limit_rows(df, table, options$permutation_table_max_rows,
                         options$permutation_table_show_all, "edges")
  .jtna_fill_table(table, df)
}

# ---- Comparisons ------------------------------------------------------------

# Compare networks `i` and `j` (names of the model's networks).
.jtna_run_compare <- function(section, options, model, i, j, id) {
  if (identical(i, j)) {
    .jtna_notice(options, section,
      sprintf("Choose two different %ss to compare.", id), name = "same",
      type = jmvcore::NoticeType$WARNING)
    section$summary$setVisible(FALSE)
    section$network$setVisible(FALSE)
    section$plot$setVisible(FALSE)
    return(invisible())
  }
  # tna builds a network from each scaled weight matrix for the network
  # properties; z-score and log scaling can make weights negative, which
  # those path-based properties cannot use.
  signed <- options$compare_scaling %in% c("zscore", "log")
  state <- section$plot$state
  if (is.null(state)) {
    state <- list(
      result = tna::compare(x = model, i = match(i, names(model)), j = match(j, names(model)),
                            scaling = options$compare_scaling, network = !signed),
      i = i, j = j)
    section$plot$setState(state)
  }
  heading <- sprintf("%s vs %s", i, j)
  section$summary$setTitle(sprintf("Summary Metrics: %s", heading))
  .jtna_fill_table(section$summary, data.frame(
    metric = as.character(state$result$summary_metrics$metric),
    value = as.numeric(state$result$summary_metrics$value)))

  network <- section$network
  network$setTitle(sprintf("Network Properties: %s", heading))
  network$getColumn("net_i")$setTitle(i)
  network$getColumn("net_j")$setTitle(j)
  if (signed) {
    network$setNote("signed", paste("Network properties are not computed with z-score or log scaling,",
                                     "which can make weights negative."))
    return(invisible())
  }
  nm <- as.data.frame(state$result$network_metrics)
  .jtna_fill_table(network, data.frame(metric = as.character(nm$metric),
                                       net_i = as.numeric(nm[[2]]), net_j = as.numeric(nm[[3]])))
}

.jtna_run_compare_sequences <- function(section, options, model, id) {
  table <- section$table
  cs <- section$plot$state
  if (is.null(cs)) {
    cs <- .jtna_compare_sequences_df(model, options)
    section$plot$setState(cs)
  }
  freq_cols <- grep("^freq_", names(cs), value = TRUE)
  prop_cols <- grep("^prop_", names(cs), value = TRUE)
  invisible(lapply(freq_cols, \(fc) {
    table$addColumn(name = fc, title = sub("^freq_", "", fc), type = "integer", superTitle = "Frequency")
  }))
  invisible(lapply(prop_cols, \(pc) {
    table$addColumn(name = pc, title = sub("^prop_", "", pc), type = "number", superTitle = "Proportion")
  }))
  # tna reports an effect size (older versions: a test statistic).
  stat_cols <- intersect(c(effect_size = "effect_size", statistic = "statistic"), names(cs))
  invisible(lapply(stat_cols, \(sc) {
    table$addColumn(name = sc, title = if (sc == "effect_size") "Effect size" else "Statistic",
                    type = "number")
  }))
  table$addColumn(name = "p_value", title = "p", type = "number")
  rows <- as.data.frame(cs)
  # Current results count states before decoding action labels; retain a
  # fallback for cached sequence comparisons from older module versions.
  if (!"length" %in% names(rows)) {
    rows$length <- lengths(strsplit(as.character(rows$pattern), "->", fixed = TRUE))
  }
  if (nrow(rows) == 0L) table$setNote("empty", "No patterns reached the minimum frequency.")
  .jtna_fill_table(table, rows)
}

# ---- Sequences ----------------------------------------------------------------

# `prepared` is a function so the data are only prepared when the table has
# to be computed. `outcome` (a function of the prepared data) gives the
# per-sequence group used for the per-group counts.
.jtna_run_patterns <- function(section, options, prepared, outcome = NULL) {
  table <- section$table
  patterns <- table$state
  if (is.null(patterns)) {
    p <- prepared()
    patterns <- .jtna_patterns_df(p, options, if (!is.null(outcome)) outcome(p))
    table$setState(patterns)
  }
  .jtna_add_count_columns(table, patterns)
  if (!is.null(attr(patterns, "jtna_note"))) table$setNote("clipped", attr(patterns, "jtna_note"))
  if (nrow(patterns) == 0L) {
    table$setNote("empty", "No patterns were found with the current settings.")
  }
  patterns <- .jtna_limit_rows(patterns, table, options$pattern_table_max_rows,
                               options$pattern_table_show_all, "patterns")
  .jtna_fill_table(table, patterns)
}

# `labels` (a function of the prepared data) returns the identifying columns
# (actor, session, group or cluster), one value per sequence.
.jtna_run_indices <- function(section, options, prepared, labels) {
  table <- section$table
  indices <- table$state
  if (is.null(indices)) {
    p <- prepared()
    indices <- .jtna_indices_df(p, options, labels(p))
    table$setState(indices)
  }
  table$getColumn("actor")$setVisible("actor" %in% names(indices))
  table$getColumn("integrative_potential")$setVisible("integrative_potential" %in% names(indices))
  indices <- .jtna_limit_rows(indices, table, options$indices_table_max_rows,
                              options$indices_table_show_all, "sequences")
  .jtna_fill_table(table, indices)
}

# Actor and session of each sequence, from the prepared data's metadata.
.jtna_sequence_labels <- function(prepared, options) {
  actor <- options$buildModel_variables_long_actor
  labels <- list(session = as.integer(.jtna_session_values(prepared, ".session_nr")))
  if (!is.null(actor)) labels <- c(list(actor = as.character(.jtna_session_values(prepared, actor))), labels)
  labels
}

# ---- Plots ----------------------------------------------------------------------

.jtna_draw_model <- function(model, options) {
  args <- c(.jtna_network_args(options, "buildModel_plot"), background = "transparent")
  if (!.jtna_grouped(model)) return(do.call(cograph::splot, c(list(x = model), args)))
  op <- .jtna_grid(length(model))
  on.exit(graphics::par(op), add = TRUE)
  .jtna_plot_networks(model, args)
}

.jtna_draw_histogram <- function(model) {
  one <- function(m, title) {
    w <- c(m$weights)
    graphics::hist(x = m, breaks = seq(0, max(1, max(w, na.rm = TRUE)) + 0.01, length.out = 20),
                   main = title, xlab = "Edge weight", ylab = "Frequency")
  }
  if (!.jtna_grouped(model)) return(one(model, "Histogram of Edge Weights"))
  op <- .jtna_grid(length(model))
  on.exit(graphics::par(op), add = TRUE)
  invisible(lapply(names(model), \(net) one(model[[net]], net)))
}

.jtna_draw_cliques <- function(cliques, options) {
  args <- c(.jtna_network_args(options, "cliques_plot"), background = "transparent")
  counts <- if (inherits(cliques, "tna_cliques")) length(cliques$weights)
            else vapply(cliques, \(g) length(g$weights), integer(1))
  if (sum(counts) == 0L) stop("no cliques of this size reach the threshold.", call. = FALSE)
  op <- .jtna_grid(sum(counts))
  on.exit(graphics::par(op), add = TRUE)
  if (inherits(cliques, "tna_cliques")) {
    return(invisible(lapply(seq_len(counts), \(k) {
      do.call(plot, c(list(x = cliques, ask = FALSE, first = k, n = 1), args))
    })))
  }
  do.call(plot, c(list(x = cliques, ask = FALSE), args))
}

.jtna_draw_bootstrap <- function(bs, options) {
  args <- c(.jtna_network_args(options, "bootstrap_plot"), background = "transparent")
  if (inherits(bs, "tna_bootstrap")) return(do.call(cograph::splot, c(list(x = bs), args)))
  op <- .jtna_grid(length(bs))
  on.exit(graphics::par(op), add = TRUE)
  .jtna_plot_networks(bs, args)
}

.jtna_draw_community <- function(coms, options) {
  if (!inherits(coms, "tna_communities")) {
    op <- .jtna_grid(length(coms))
    on.exit(graphics::par(op), add = TRUE)
  }
  plot(x = coms, method = options$community_methods,
       layout = options$community_plot_layout, background = "transparent")
}

.jtna_draw_permutation <- function(perm, options) {
  op <- .jtna_grid(length(perm))
  on.exit(graphics::par(op), add = TRUE)
  plot(x = perm, cut = 0.01, layout = options$permutation_plot_layout)
}

# A sequence index or state distribution plot. Text options left empty fall
# back to tna's defaults; options that only apply to the other plot type are
# not passed.
.jtna_draw_sequences <- function(model, options, type = c("index", "distribution")) {
  type <- match.arg(type)
  text <- function(name) {
    value <- options[[name]]
    if (length(value) == 1L && nzchar(trimws(value))) value
  }
  grouped <- .jtna_grouped(model)
  args <- list(
    x = model, type = type, show_n = options$sequences_show_n, tick = options$sequences_tick,
    na_color = text("sequences_na_color"), border = text("sequences_border"),
    legend_title = text("sequences_legend_title"), xlab = text("sequences_xlab"),
    title = text(sprintf("sequences_%s_title", type)),
    ylab = text(sprintf("sequences_%s_ylab", type)),
    ncol = if (grouped) options$sequences_ncol)
  if (type == "index") {
    args$sort_by <- .jtna_sort_by(model, options$sequences_sort)
  } else {
    args <- c(args, list(scale = options$sequences_scale, geom = options$sequences_geom,
                         include_na = options$sequences_include_na))
  }
  print(do.call(tna::plot_sequences, Filter(Negate(is.null), args)))
}

# The time-point columns to sort an index plot by: none, the first, or all.
.jtna_sort_by <- function(model, sort = c("none", "first", "all")) {
  sort <- match.arg(sort)
  # The sequence data are a matrix; tna names its columns via as.data.frame().
  data <- if (.jtna_grouped(model)) model[[1]]$data else model$data
  cols <- names(as.data.frame(data))
  switch(sort, none = NULL, first = cols[1], all = cols)
}

# Size both sequence plots as set in the panel.
.jtna_run_sequences <- function(section, options) {
  section$index$setSize(options$sequences_width, options$sequences_height)
  section$distribution$setSize(options$sequences_width, options$sequences_height)
  invisible()
}

.jtna_draw_compare_sequences <- function(cs) {
  p <- plot(cs) + ggplot2::theme(
    text = ggplot2::element_text(size = 14), axis.text = ggplot2::element_text(size = 12),
    legend.text = ggplot2::element_text(size = 12), strip.text = ggplot2::element_text(size = 12))
  print(p)
}

.jtna_draw_compare <- function(state, options) {
  print(plot(x = state$result, type = options$compare_plot_type, name_x = state$i, name_y = state$j))
}

# The difference network for every pair of networks.
.jtna_draw_network_diff <- function(model, options) {
  pairs <- utils::combn(seq_along(model), 2, simplify = FALSE)
  op <- .jtna_grid(length(pairs))
  on.exit(graphics::par(op), add = TRUE)
  args <- .jtna_network_args(options, "compare_network_diff_plot")
  invisible(lapply(pairs, \(p) {
    title <- sprintf("%s vs %s", names(model)[p[1]], names(model)[p[2]])
    do.call(tna::plot_compare, c(list(x = model, i = p[1], j = p[2], title = title), args))
  }))
}
