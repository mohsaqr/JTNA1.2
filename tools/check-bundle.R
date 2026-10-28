# Run the JTNA test suite against what jamovi will actually load: the
# packages bundled into the .jmo (TNAModule/build/R*), then jamovi's own R
# library, then the rest. Run after jmvtools::install("TNAModule"):
#
#   Rscript tools/check-bundle.R [path/to/jamovi/R/library]
#
# On macOS the jamovi library defaults to the app's bundled jmv library.

args <- commandArgs(trailingOnly = TRUE)
jamovi_lib <- if (length(args) >= 1L) args[[1]] else
  "/Applications/jamovi.app/Contents/Resources/modules/jmv/R"

module <- normalizePath(file.path(dirname(sub("--file=", "", grep("^--file=", commandArgs(), value = TRUE))),
                                  "..", "TNAModule"))
build_lib <- Sys.glob(file.path(module, "build", "R*"))
stopifnot(
  "no build library: run jmvtools::install(\"TNAModule\") first" = length(build_lib) == 1L,
  "jamovi R library not found" = dir.exists(jamovi_lib)
)

.libPaths(c(build_lib, jamovi_lib, .libPaths()))
bundled <- vapply(c("tna", "codyna", "cograph", "igraph"), \(p) {
  sprintf("%s %s (%s)", p, as.character(utils::packageVersion(p)), dirname(find.package(p)))
}, character(1))
message("Testing against:\n  ", paste(bundled, collapse = "\n  "))

Sys.setenv(NOT_CRAN = "true")
results <- testthat::test_dir(file.path(module, "tests", "testthat"), reporter = "summary",
                              stop_on_failure = FALSE)
summary_df <- as.data.frame(results)
failed <- sum(summary_df$failed) + sum(summary_df$error)
message(sprintf("%d expectations, %d failed", sum(summary_df$nb), failed))
quit(status = if (failed > 0) 1L else 0L)
