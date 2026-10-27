# Shared helpers for the TNA, GroupTNA and ClusterTNA analyses.
#
# Everything the three analyses have in common lives here so that it is
# written, and fixed, once: data preparation, input validation, option
# translation, table filling, section-level error reporting and plotting.

# ---- Options --------------------------------------------------------------

# The variables assigned in the panel. Unassigned roles are dropped, so the
# result can be passed straight to tna::prepare_data().
.jtna_vars <- function(options) {
  roles <- c(action = "action", actor = "actor", time = "time", order = "order", group = "group")
  vars <- lapply(roles, \(r) .jtna_option(options, paste0("buildModel_variables_long_", r)))
  Filter(\(v) length(v) == 1L && nzchar(v), vars)
}

# An option's value, or NULL when this analysis does not define it (the
# generated options object errors on unknown names).
.jtna_option <- function(options, name) {
  if (options$has(name)) options[[name]] else NULL
}

# A grouped model over the prepared sequences, one network per value of
# `groups` (one value per sequence). Scaling is applied within each group:
# tna's grouped analyses (centralities, communities, bootstrap, comparisons)
# reject, or silently mislabel, models scaled across groups.
.jtna_group_model <- function(prepared, groups, options, scaling = .jtna_scaling(options)) {
  tna::group_model(
    x = prepared,
    group = groups,
    type = options$buildModel_type,
    scaling = scaling,
    groupwise = length(scaling) > 0L,
    params = .jtna_params(options))
}

# Say how grouped weights were scaled, when they were.
.jtna_scaling_notice <- function(section, options, id) {
  if (length(.jtna_scaling(options)) == 0L) return(invisible())
  .jtna_notice(options, section,
    sprintf("Edge weights are scaled within each %s; tna does not analyse grouped models scaled across %ss.", id, id),
    name = "scaling", type = jmvcore::NoticeType$INFO)
}

# A Level option's value, or `default` when nothing is selected.
.jtna_level <- function(value, default) {
  if (length(value) == 1L && !is.na(value) && nzchar(value)) value else default
}

# tna's `scaling` argument: the panel's "No scaling" maps to character(0).
.jtna_scaling <- function(options) {
  scaling <- options$buildModel_scaling
  if (is.null(scaling) || identical(scaling, "noScaling")) character(0L) else scaling
}

# tna's `params` argument: only the attention model takes one (lambda).
.jtna_params <- function(options) {
  if (identical(options$buildModel_type, "attention")) {
    list(lambda = options$buildModel_lambda)
  } else {
    list()
  }
}

.jtna_centrality_measures <- c(
  "OutStrength", "InStrength", "ClosenessIn", "ClosenessOut", "Closeness",
  "Betweenness", "BetweennessRSP", "Diffusion", "Clustering"
)

# The centrality measures ticked in the panel, in table order.
.jtna_measures <- function(options) {
  ticked <- vapply(
    .jtna_centrality_measures,
    \(m) isTRUE(options[[paste0("centrality_", m)]]),
    logical(1)
  )
  .jtna_centrality_measures[ticked]
}

# The stability measures ticked in the panel; all three when none is ticked.
.jtna_stability_measures <- function(options) {
  candidates <- c("InStrength", "OutStrength", "Betweenness")
  ticked <- vapply(
    candidates,
    \(m) isTRUE(options[[paste0("centrality_stability_", m)]]),
    logical(1)
  )
  if (any(ticked)) candidates[ticked] else candidates
}

# A clique threshold of 0 means "pick a sensible default for the size".
.jtna_cliques_threshold <- function(options) {
  threshold <- as.numeric(options$cliques_threshold)
  if (threshold > 0) return(threshold)
  if (as.numeric(options$cliques_size) <= 2) 0.1 else 0.01
}

# ---- Data preparation and validation --------------------------------------

# Stop the analysis with a message that jamovi shows in place of the
# results. The class lets callers and tests catch bad input precisely.
.jtna_reject <- function(message) {
  stop(errorCondition(message, class = "jtna_bad_input", call = NULL))
}

# Reject input that cannot produce a network, with a message that says why.
.jtna_validate <- function(data, options) {
  vars <- .jtna_vars(options)
  action <- as.character(data[[vars$action]])
  present <- !is.na(action) & nzchar(trimws(action))
  if (!any(present)) {
    .jtna_reject("The Action variable has no non-missing values.")
  }
  if (length(unique(action[present])) < 2L) {
    .jtna_reject("JTNA needs at least two distinct actions in the Action variable.")
  }
  if (!is.null(vars$group)) {
    group <- as.character(data[[vars$group]])
    group <- unique(group[!is.na(group) & nzchar(trimws(group))])
    if (length(group) < 2L) {
      .jtna_reject("The Group variable needs at least two groups to compare.")
    }
  }
  invisible(TRUE)
}

# Run tna::prepare_data() once, with every argument the user set, so that
# the model, pattern discovery and sequence indices all describe the same
# sequences. Time is handed to prepare_data as-is (text) or as Unix seconds
# (numeric); converting text with a bare as.POSIXct() drops the time of day
# from ISO-8601 stamps such as "2025-01-01T08:27:07Z".
.jtna_prepare <- function(data, options) {
  vars <- .jtna_vars(options)
  .jtna_validate(data, options)

  df <- data[unlist(vars, use.names = FALSE)]
  text_roles <- intersect(c("action", "actor", "group"), names(vars))
  df[unlist(vars[text_roles])] <- lapply(df[unlist(vars[text_roles])], as.character)

  if (!is.null(vars$order)) {
    # Order is a sequence position: keep it numeric so events sort 1, 2, 10
    # rather than "1", "10", "2". as.numeric() warns on text values, which is
    # exactly the case tested here (they stay as they are).
    order_num <- suppressWarnings(as.numeric(as.character(df[[vars$order]])))
    if (!anyNA(order_num)) df[[vars$order]] <- order_num
  }

  unix_time <- FALSE
  if (!is.null(vars$time)) {
    unix_time <- is.numeric(df[[vars$time]])
    if (!unix_time) df[[vars$time]] <- as.character(df[[vars$time]])
  }

  args <- list(
    data = df,
    action = vars$action,
    actor = vars$actor,
    time = vars$time,
    order = vars$order,
    time_threshold = options$buildModel_threshold,
    is_unix_time = if (unix_time) TRUE
  )
  args <- Filter(Negate(is.null), args)

  tryCatch(
    do.call(tna::prepare_data, args),
    error = function(e) {
      message <- conditionMessage(e)
      if (!is.null(vars$time) && grepl("time|date|posix|format", message, ignore.case = TRUE)) {
        .jtna_reject(sprintf(
          paste("The Time variable could not be read as date-times. Use a format such as",
                "2025-01-01 08:30:00, or Unix time in seconds. (%s)"),
          message))
      }
      .jtna_reject(sprintf("The data could not be prepared for TNA: %s", message))
    }
  )
}

# One value per sequence of `column` from the prepared data. prepare_data's
# meta_data has one row per session, in the same order as sequence_data, so
# this stays aligned when a time threshold splits an actor into sessions.
.jtna_session_values <- function(prepared, column) {
  meta <- prepared$meta_data
  stopifnot(
    "prepared data must have one meta_data row per sequence" =
      nrow(meta) == nrow(prepared$sequence_data)
  )
  if (!column %in% names(meta)) {
    stop(sprintf("the prepared data have no `%s` column; JTNA needs tna 1.3.1 or later (installed: %s).",
                 column, utils::packageVersion("tna")), call. = FALSE)
  }
  meta[[column]]
}

# Sequences as a character data frame, the form codyna expects.
.jtna_sequences <- function(prepared) {
  as.data.frame(lapply(prepared$sequence_data, as.character), stringsAsFactors = FALSE)
}

# ---- Section reporting ------------------------------------------------------

# Show a message at the top of a results section, through jamovi's notice
# system rather than as free text.
.jtna_notice <- function(options, section, message, name = "notice",
                         type = jmvcore::NoticeType$ERROR) {
  # Prefixed so a notice never shadows the element it sits next to:
  # Group$get() returns the first item with a given name.
  notice <- jmvcore::Notice$new(options = options, name = paste0("notice_", name), type = type)
  notice$setContent(message)
  section$insert(1, notice)
  invisible(notice)
}

# Hide every element of the named sections. A jamovi Group is visible when
# any of its items is, so the items themselves are hidden.
.jtna_hide_sections <- function(results, sections) {
  invisible(lapply(sections, \(s) lapply(results$get(s)$items, \(item) item$setVisible(FALSE))))
}

# Run one section. A failure is reported on that section and the analysis
# moves on to the next one, instead of skipping everything after it.
.jtna_section <- function(options, section, label, fun) {
  tryCatch(
    fun(),
    error = function(e) {
      .jtna_notice(
        options, section,
        sprintf("The %s could not be computed: %s", label, conditionMessage(e)),
        name = "error"
      )
      NULL
    }
  )
}

# ---- Tables -------------------------------------------------------------------

# The columns of a result as a plain data frame. codyna attaches its full
# match data as attributes (tens of MB on real data), which would otherwise
# be saved into the .omv file with the table.
.jtna_plain <- function(df) {
  data.frame(as.list(df), check.names = FALSE, stringsAsFactors = FALSE)
}

# Add one row per data frame row. Only columns the table declares are sent.
.jtna_fill_table <- function(table, df) {
  if (is.null(df) || nrow(df) == 0L) return(invisible(table))
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df[] <- lapply(df, \(col) if (is.factor(col)) as.character(col) else col)
  invisible(lapply(seq_len(nrow(df)), \(i) {
    table$addRow(rowKey = i, values = as.list(df[i, , drop = FALSE]))
  }))
  invisible(table)
}

# Keep the first `max_rows` rows unless the user asked for all of them, and
# say on the table how many were left out.
.jtna_limit_rows <- function(df, table, max_rows, show_all, noun) {
  total <- nrow(df)
  if (isTRUE(show_all) || total <= max_rows) return(df)
  table$setNote("limit", sprintf("Showing the first %d of %d %s.", max_rows, total, noun))
  df[seq_len(max_rows), , drop = FALSE]
}

# Centrality results for one or more networks; `id` names the column that
# identifies the network ("group" or "cluster") for grouped models.
.jtna_centrality_df <- function(cent, measures, id = NULL) {
  df <- as.data.frame(cent, stringsAsFactors = FALSE)
  keep <- c(if (!is.null(id)) "group", "state", intersect(measures, names(df)))
  df <- df[keep]
  if (!is.null(id)) names(df)[1] <- id
  df
}

# Community assignments (one row per state, plus the network for grouped
# models) for the single method selected in the panel.
.jtna_community_df <- function(coms, method, id = NULL) {
  one <- function(x, net = NULL) {
    a <- as.data.frame(x$assignments, stringsAsFactors = FALSE)
    df <- data.frame(state = as.character(a$state), community = as.integer(a[[method]]),
                     stringsAsFactors = FALSE)
    if (!is.null(net)) df <- cbind(stats::setNames(data.frame(net), id), df)
    df
  }
  if (is.null(id)) return(one(coms))
  do.call(rbind, lapply(names(coms), \(net) one(coms[[net]], net)))
}

# Bootstrap edges: significant ones first, then by p-value. `id` adds the
# network column for grouped models.
.jtna_bootstrap_df <- function(bs, significant_only, id = NULL) {
  one <- function(summary, net = NULL) {
    s <- as.data.frame(summary, stringsAsFactors = FALSE)
    s <- s[order(-s$sig, s$p_value, s$from, s$to), , drop = FALSE]
    if (isTRUE(significant_only)) s <- s[s$sig, , drop = FALSE]
    df <- data.frame(
      from = as.character(s$from), to = as.character(s$to), weight = s$weight,
      p_value = s$p_value, cr_lower = s$cr_lower, cr_upper = s$cr_upper,
      ci_lower = s$ci_lower, ci_upper = s$ci_upper,
      significant = ifelse(s$sig, "Yes", "No"),
      stringsAsFactors = FALSE
    )
    if (!is.null(net)) df <- cbind(stats::setNames(data.frame(rep(net, nrow(df))), id), df)
    df
  }
  if (is.null(id)) return(one(bs$summary))
  do.call(rbind, lapply(names(bs), \(net) one(bs[[net]]$summary, net)))
}

# Permutation-test edges for every pair of networks, smallest p-value first.
.jtna_permutation_df <- function(perm, id) {
  rows <- lapply(names(perm), \(pair) {
    s <- as.data.frame(perm[[pair]]$edges$stats, stringsAsFactors = FALSE)
    if (nrow(s) == 0L) return(NULL)
    s <- s[order(s$p_value, -abs(s$diff_true), s$edge_name), , drop = FALSE]
    data.frame(
      comparison = pair, edge_name = trimws(as.character(s$edge_name)),
      diff_true = as.numeric(s$diff_true), effect_size = as.numeric(s$effect_size),
      p_value = as.numeric(s$p_value), stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  if (!is.null(df)) names(df)[1] <- paste0(id, "_comparison")
  df
}

# Edge betweenness as one row per edge with a positive value, largest first.
.jtna_edge_betweenness_df <- function(weights) {
  df <- as.data.frame(as.table(weights), stringsAsFactors = FALSE)
  names(df) <- c("from", "to", "value")
  df <- df[df$value > 0, , drop = FALSE]
  df[order(-df$value, df$from, df$to), , drop = FALSE]
}

# Sequence comparison: patterns without missing values, most frequent first,
# capped at 20 rows.
.jtna_compare_sequences_df <- function(model, options) {
  sub_min <- options$compare_sequences_sub_min
  sub_max <- options$compare_sequences_sub_max
  if (sub_min > sub_max) {
    stop("the minimum pattern length is larger than the maximum.", call. = FALSE)
  }
  cs <- tna::compare_sequences(
    x = model,
    sub = seq.int(sub_min, sub_max),
    min_freq = options$compare_sequences_min_freq,
    correction = options$compare_sequences_correction
  )
  cs <- as.data.frame(cs[!grepl("*", cs$pattern, fixed = TRUE), , drop = FALSE])
  freq_cols <- grep("^freq_", names(cs), value = TRUE)
  total <- rowSums(cs[freq_cols])
  cs <- cs[order(-total, cs$pattern), , drop = FALSE]
  cs[seq_len(min(20L, nrow(cs))), , drop = FALSE]
}

# Pattern discovery on the prepared sequences. `outcome` (one value per
# sequence) adds the per-group counts and the chi-squared test.
.jtna_patterns_df <- function(prepared, options, outcome = NULL) {
  if (options$pattern_len_min > options$pattern_len_max) {
    stop("the minimum length is larger than the maximum length.", call. = FALSE)
  }
  if (options$pattern_gap_min > options$pattern_gap_max) {
    stop("the minimum gap is larger than the maximum gap.", call. = FALSE)
  }
  args <- list(
    data = .jtna_sequences(prepared),
    type = options$pattern_type,
    len = seq.int(options$pattern_len_min, options$pattern_len_max),
    gap = seq.int(options$pattern_gap_min, options$pattern_gap_max),
    min_support = options$pattern_min_support,
    min_freq = options$pattern_min_count,
    outcome = outcome
  )
  filters <- c(start = "pattern_starts_with", end = "pattern_ends_with", contain = "pattern_contains")
  chosen <- lapply(filters, \(f) {
    value <- options[[f]]
    if (isTRUE(options[[paste0(f, "_use")]]) && length(value) == 1L && nzchar(value)) value
  })
  args <- c(Filter(Negate(is.null), args), Filter(Negate(is.null), chosen))
  .jtna_plain(do.call(codyna::discover_patterns, args))
}

# Add one integer column per outcome count ("count_<group>") to the patterns
# table, titled after the group.
.jtna_add_count_columns <- function(table, patterns) {
  counts <- grep("^count_", names(patterns), value = TRUE)
  invisible(lapply(counts, \(col) {
    table$addColumn(name = col, title = sub("^count_", "", col), type = "integer",
                    superTitle = "Count by group")
  }))
}

# Sequence indices, one row per sequence, labelled from the prepared data's
# own per-session metadata. `labels` maps table columns to label vectors.
.jtna_indices_df <- function(prepared, options, labels = list()) {
  args <- list(data = .jtna_sequences(prepared), omega = options$indices_omega)
  favorable <- options$indices_favorable
  if (length(favorable) == 1L && nzchar(favorable)) args$favorable <- favorable
  indices <- .jtna_plain(do.call(codyna::sequence_indices, args))
  id <- data.frame(sequence_id = seq_len(nrow(indices)))
  cbind(id, as.data.frame(labels, stringsAsFactors = FALSE), indices)
}

# ---- Plots ----------------------------------------------------------------------

# Network-plot settings from the panel, for the options sharing `prefix`
# (for example "buildModel_plot" or "bootstrap_plot").
.jtna_network_args <- function(options, prefix) {
  get <- \(name) options[[paste0(prefix, "_", name)]]
  list(
    cut = get("cut"),
    minimum = get("min_value"),
    edge.label.cex = get("edge_label_size"),
    node_size = get("node_size"),
    label.cex = get("node_label_size"),
    layout = get("layout")
  )
}

# Arrange `n` panels in a near-square grid. Returns the previous settings so
# the caller can restore them with on.exit().
.jtna_grid <- function(n) {
  cols <- ceiling(sqrt(n))
  graphics::par(mfrow = c(ceiling(n / cols), cols))
}

# One network plot per element of a grouped object (a group_tna model or a
# grouped bootstrap), each titled with its group.
.jtna_plot_networks <- function(x, args) {
  invisible(lapply(names(x), \(net) {
    do.call(cograph::splot, c(list(x = x[[net]], title = net), args))
  }))
}

# Draw a plot, or a frame that explains why it could not be drawn. The
# message stays inside this image, so it also appears on export.
.jtna_draw <- function(title, fun) {
  tryCatch(
    {
      fun()
      TRUE
    },
    error = function(e) {
      graphics::plot.new()
      graphics::title(main = sprintf("%s could not be drawn", title))
      graphics::mtext(conditionMessage(e), side = 1, line = -2, cex = 0.9)
      warning(warningCondition(sprintf("%s could not be drawn: %s", title, conditionMessage(e)),
                               class = "jtna_plot_failed", call = NULL))
      TRUE
    }
  )
}

# ---- Instructions -------------------------------------------------------------

.jtna_instructions <- function(kind = c("single", "group", "cluster")) {
  kind <- match.arg(kind)
  roles <- switch(kind,
    single = c(
      "<b>Action</b> (required): the actions, states or events. Each distinct value becomes a node.",
      "<b>Actor</b> (optional): who performed each event. Sequences are built per actor.",
      paste("<b>Time</b> or <b>Order</b> (optional): orders the events.",
            "With Time, a gap longer than the session gap starts a new session.")
    ),
    group = c(
      "<b>Action</b> (required): the actions, states or events.",
      "<b>Actor</b> (required): who performed each event.",
      "<b>Group</b> (required): a separate network is built for each group.",
      "<b>Time</b> or <b>Order</b> (optional): orders the events."
    ),
    cluster = c(
      "<b>Action</b> (required): the actions, states or events.",
      "<b>Actor</b> (required): who performed each event. Sequences are clustered on their transitions.",
      "<b>Time</b> or <b>Order</b> (optional): orders the events.",
      "Set the <b>number of clusters</b>, then tick <b>Run clustering</b>. Clustering can take a while on large data."
    )
  )
  links <- switch(kind,
    single = '<a href="https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html" target="_blank">TNA chapter</a>',
    group = '<a href="https://sonsoles.me/posts/tna-group" target="_blank">Group TNA tutorial</a>',
    cluster = '<a href="https://sonsoles.me/posts/tna-clustering" target="_blank">Clustering tutorial</a>'
  )
  paste0(
    "<p>Data should be in long format, with one row per event.</p><ul>",
    paste0("<li>", roles, "</li>", collapse = ""),
    "</ul><p>Learn more: ", links,
    ' | <a href="https://sonsoles.me/posts/tna-tutorial" target="_blank">TNA tutorial</a></p>'
  )
}
