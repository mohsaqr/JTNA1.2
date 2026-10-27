# Load the module from source so the analyses run through the real jamovi
# classes (R6 + jmvcore), headless.
if (!exists("TNAClass", mode = "environment") && requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(testthat::test_path("..", ".."), quiet = TRUE, export_all = TRUE)
}

# Run an analysis with every option at its default except the ones given.
# The generated wrappers have no default for variable and level options, so
# the unassigned ones are passed as NULL.
jtna_run <- function(analysis, ...) {
  fn <- get(analysis, mode = "function")
  args <- list(...)
  required <- names(Filter(\(x) identical(x, quote(expr = )), formals(fn)))
  unset <- setdiff(required, names(args))
  args[unset] <- list(NULL)
  do.call(fn, args)
}

# The bundled example data, with its time column left as text.
jtna_data <- function() {
  utils::read.csv(testthat::test_path("..", "..", "data", "Regulation_long.csv"),
                  stringsAsFactors = FALSE)
}

# A small long-format data set: `n_actor` actors, `per` events each.
jtna_small <- function(seed = 1, n_actor = 20, per = 15,
                       actions = c("plan", "monitor", "evaluate", "read", "write")) {
  set.seed(seed)
  gaps <- unlist(lapply(seq_len(n_actor), \(a) cumsum(sample(c(30, 60, 120, 3600), per,
    replace = TRUE, prob = c(.4, .3, .2, .1)))))
  data.frame(
    actor  = rep(sprintf("S%02d", seq_len(n_actor)), each = per),
    time   = format(as.POSIXct("2026-01-01 09:00:00", tz = "UTC") + gaps, "%Y-%m-%d %H:%M:%S"),
    action = sample(actions, n_actor * per, replace = TRUE),
    group  = rep(rep(c("A", "B"), length.out = n_actor), each = per),
    stringsAsFactors = FALSE
  )
}

# The data frame behind a results table.
jtna_table <- function(table) table$asDF

# Notices inserted into a results group, as a character vector of contents.
jtna_notices <- function(group) {
  items <- group$items
  is_notice <- vapply(items, \(i) inherits(i, "Notice"), logical(1))
  # asString() wraps lines; collapse the wrapping so messages can be matched.
  vapply(items[is_notice], \(n) gsub("\\s+", " ", trimws(n$asString())), character(1))
}
