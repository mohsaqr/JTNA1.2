# Independent reference implementations: base R counting (and igraph on the
# weight matrix), used to check JTNA's numbers without going through tna's
# or codyna's own code paths.

# Sequences as a character matrix, one row per sequence.
ref_sequences <- function(prepared) {
  m <- as.matrix(as.data.frame(lapply(prepared$sequence_data, as.character), stringsAsFactors = FALSE))
  dimnames(m) <- NULL
  m
}

# Transition counts: consecutive pairs of observed states, summed over sequences.
ref_counts <- function(seqs, states) {
  from <- c(seqs[, -ncol(seqs), drop = FALSE])
  to <- c(seqs[, -1L, drop = FALSE])
  keep <- !is.na(from) & !is.na(to)
  unclass(table(factor(from[keep], states), factor(to[keep], states)))
}

# Relative transition probabilities: counts divided by their row totals.
ref_relative <- function(counts) {
  totals <- rowSums(counts)
  sweep(counts, 1L, ifelse(totals > 0, totals, 1), "/")
}

# Initial probabilities: the share of sequences starting in each state.
ref_inits <- function(seqs, states) {
  as.numeric(table(factor(seqs[, 1L], states))) / nrow(seqs)
}

# Strengths without self-loops, and igraph betweenness/closeness on 1/weight.
ref_centralities <- function(weights) {
  diag(weights) <- 0
  g <- igraph::graph_from_adjacency_matrix(weights, mode = "directed", weighted = TRUE)
  dist <- 1 / igraph::E(g)$weight
  data.frame(
    state = rownames(weights),
    OutStrength = rowSums(weights),
    InStrength = colSums(weights),
    Betweenness = as.numeric(igraph::betweenness(g, weights = dist)),
    Closeness = as.numeric(igraph::closeness(g, mode = "all", weights = dist)),
    stringsAsFactors = FALSE
  )
}

# Bigram occurrences: count adjacent state tuples, never a joined string.
# Internal missing events break adjacency. `pattern` is a display convenience;
# `from` and `to` retain tuple identity even when labels themselves contain ->.
ref_bigrams <- function(seqs) {
  seqs <- as.matrix(seqs)
  empty <- data.frame(pattern = character(), frequency = integer(), count = integer(),
                       from = character(), to = character(), stringsAsFactors = FALSE)
  if (ncol(seqs) < 2L || nrow(seqs) == 0L) return(empty)
  occurrences <- data.frame(
    from = as.vector(seqs[, -ncol(seqs), drop = FALSE]),
    to = as.vector(seqs[, -1L, drop = FALSE]),
    sequence = rep(seq_len(nrow(seqs)), times = ncol(seqs) - 1L),
    stringsAsFactors = FALSE)
  occurrences <- occurrences[!is.na(occurrences$from) & !is.na(occurrences$to), , drop = FALSE]
  if (nrow(occurrences) == 0L) return(empty)
  tuples <- unique(occurrences[c("from", "to")])
  matching <- lapply(seq_len(nrow(tuples)), \(i) {
    occurrences$from == tuples$from[i] & occurrences$to == tuples$to[i]
  })
  data.frame(pattern = paste(tuples$from, tuples$to, sep = "->"),
             frequency = vapply(matching, sum, integer(1)),
             count = vapply(matching, \(hit) length(unique(occurrences$sequence[hit])), integer(1)),
             from = tuples$from, to = tuples$to, stringsAsFactors = FALSE)
}

# Simple per-sequence indices from their definitions (codyna's
# normalisation of entropy: by the log of the number of distinct values in
# the whole sequence matrix, missing included).
ref_indices <- function(seqs) {
  n_values <- length(unique(c(seqs)))
  rows <- lapply(seq_len(nrow(seqs)), \(i) {
    s <- seqs[i, !is.na(seqs[i, ])]
    p <- as.numeric(table(s)) / length(s)
    tab <- table(s)
    dominant <- names(tab)[which.max(tab)]
    runs <- rle(s)
    self <- sum(s[-1L] == s[-length(s)])
    data.frame(
      valid_n = length(s),
      unique_states = length(unique(s)),
      longitudinal_entropy = -sum(p * log(p)) / log(n_values),
      simpson_diversity = 1 - sum(p^2),
      self_loop_tendency = self / (length(s) - 1L),
      transition_rate = (length(s) - 1L - self) / (length(s) - 1L),
      first_state = s[1L],
      last_state = s[length(s)],
      dominant_state = dominant,
      dominant_proportion = max(tab) / length(s),
      dominant_max_spell = max(runs$lengths[runs$values == dominant]),
      max_spell_duration = max(runs$lengths),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# A random long-format data set for Monte Carlo checks. Every draw is
# controlled by `seed`.
mc_data <- function(seed) {
  set.seed(seed)
  n_states <- sample(3:8, 1)
  states <- sprintf("s%d", seq_len(n_states))
  n_actor <- sample(6:40, 1)
  lengths <- sample(2:25, n_actor, replace = TRUE)
  gaps <- lapply(lengths, \(n) cumsum(sample(c(20, 60, 300, 2000), n, replace = TRUE,
                                            prob = c(.4, .3, .2, .1))))
  data.frame(
    actor = rep(sprintf("a%02d", seq_len(n_actor)), lengths),
    time = format(as.POSIXct("2026-03-01 08:00:00", tz = "UTC") + unlist(gaps),
                  "%Y-%m-%d %H:%M:%S"),
    action = sample(states, sum(lengths), replace = TRUE),
    group = rep(sample(rep(c("g1", "g2"), length.out = n_actor)), lengths),
    stringsAsFactors = FALSE
  )
}
