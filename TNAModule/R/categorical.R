# Compatibility corrections for categorical states passed to codyna.
# Keep its extraction algorithms, but use unambiguous state identifiers:
# codyna otherwise treats empty labels as missing and '*'/'->' as syntax.
#' Encode categorical states without changing their ordering
#' @param sequences A data frame of character sequence columns.
#' @return Encoded data, original labels, and corresponding tokens.
#' @keywords internal
.jtna_encode_states <- function(sequences) {
  stopifnot(is.data.frame(sequences))
  labels <- sort(unique(unlist(sequences, use.names = FALSE)))
  labels <- labels[!is.na(labels)]
  tokens <- sprintf("state%0*d", nchar(length(labels)), seq_along(labels))
  data <- as.data.frame(lapply(sequences, function(column) {
    tokens[match(column, labels)]
  }), stringsAsFactors = FALSE)
  list(data = data, labels = labels, tokens = tokens)
}

#' Restore state labels while retaining gapped-pattern wildcards
#' @param patterns Encoded pattern strings.
#' @param encoding The result of `.jtna_encode_states()`.
#' @return Display strings with quoted labels when required for clarity.
#' @keywords internal
.jtna_decode_patterns <- function(patterns, encoding) {
  stopifnot(is.character(patterns), is.list(encoding))
  quote_labels <- any(!nzchar(encoding$labels) |
                        grepl("->", encoding$labels, fixed = TRUE) |
                        grepl("*", encoding$labels, fixed = TRUE))
  labels <- if (quote_labels) encodeString(encoding$labels, quote = '"') else encoding$labels
  vapply(strsplit(patterns, "->", fixed = TRUE), function(parts) {
    matched <- match(parts, encoding$tokens)
    parts[!is.na(matched)] <- labels[matched[!is.na(matched)]]
    paste(parts, collapse = "->")
  }, character(1))
}

# Association is a group x (pattern present/absent) test. codyna 1.0.0
# instead tests uniform pattern counts, which confounds group sizes with
# prevalence. Derive counts from its occurrence matrices, preserving the
# extracted patterns and sequence membership, including one-sequence groups.
#' Compute pattern-presence association with outcome groups
#' @param patterns A plain codyna pattern data frame with encoded labels.
#' @param occurrences The occurrence matrices attached by codyna.
#' @param outcome One nonmissing group label per sequence.
#' @return Patterns with group counts and Pearson chi-square results.
#' @keywords internal
.jtna_pattern_association <- function(patterns, occurrences, outcome) {
  stopifnot(is.data.frame(patterns), is.list(occurrences), !anyNA(outcome))
  groups <- unique(as.character(outcome))
  sizes <- vapply(groups, function(group) sum(outcome == group), integer(1))
  counts <- lapply(seq_len(nrow(patterns)), function(i) {
    source <- occurrences[[which(vapply(occurrences, function(x) {
      identical(as.numeric(x$length), as.numeric(patterns$length[i]))
    }, logical(1)))[1L]]]
    present <- source$patterns[, match(patterns$pattern[i], source$unique)] > 0L
    stopifnot(length(present) == length(outcome))
    vapply(groups, function(group) sum(present[outcome == group]), integer(1))
  })
  counts <- if (length(counts)) do.call(rbind, counts) else matrix(integer(), 0L, length(groups))
  colnames(counts) <- paste0("count_", groups)
  statistics <- lapply(seq_len(nrow(counts)), function(i) {
    if (length(groups) < 2L) return(c(chisq = NA_real_, p_value = NA_real_))
    observed <- cbind(counts[i, ], sizes - counts[i, ])
    expected <- outer(sizes, colSums(observed)) / sum(sizes)
    usable <- expected > 0
    statistic <- sum((observed[usable] - expected[usable])^2 / expected[usable])
    c(chisq = statistic, p_value = stats::pchisq(statistic, length(groups) - 1L, lower.tail = FALSE))
  })
  statistics <- if (length(statistics)) do.call(rbind, statistics) else
    matrix(numeric(), 0L, 2L, dimnames = list(NULL, c("chisq", "p_value")))
  cbind(patterns, as.data.frame(counts, check.names = FALSE), as.data.frame(statistics))
}
