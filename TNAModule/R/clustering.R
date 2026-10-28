#' Cluster prepared sequences while retaining every observed action label
#'
#' Long-format inputs use NA for missing events. Literal empty strings, stars
#' and percent signs are therefore states, as they are in the network model.
#' The bundled tna encoder has 62 symbols; detect overflow before its distance
#' calculation produces an opaque missing-value error.
#' @param sequences A data frame with one row per prepared sequence.
#' @param options The ClusterTNA analysis options.
#' @return A tna_clustering object from tna::cluster_sequences().
.jtna_cluster_sequences <- function(sequences, options) {
  stopifnot(is.data.frame(sequences))
  values <- as.vector(as.matrix(sequences))
  observed <- values[!is.na(values)]
  if (length(observed) == 0L) {
    .jtna_reject("Clustering needs at least one non-missing action.")
  }
  # Match tna 1.3.1's seq2chr allocation, including the slot used by NA in
  # unique(values) and its additional missing-event symbol.
  symbols <- max(match(observed, unique(values))) + as.integer(anyNA(values))
  if (symbols > 62L) {
    .jtna_reject(sprintf(paste(
      "The installed tna clustering encoder cannot represent this dataset",
      "(%d distinct actions%s): it needs %d symbols, but supports 62.",
      "Reduce the number of action categories or use TNA/Group TNA without clustering."),
      length(unique(observed)), if (anyNA(values)) ", with missing or padded events" else "",
      symbols))
  }
  .jtna_with_seed(.jtna_seed(options), \() tna::cluster_sequences(
    data = sequences,
    k = options$clustering_k,
    dissimilarity = options$clustering_dissimilarity,
    method = options$clustering_method,
    na_syms = character(0L),
    # Disabled controls retain their value when the distance changes.
    weighted = identical(options$clustering_dissimilarity, "hamming") &&
      isTRUE(options$clustering_weighted),
    lambda = options$clustering_lambda))
}
