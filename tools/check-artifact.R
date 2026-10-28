# Smoke-test the compiled package and dependency metadata inside a .jmo.
# Run from the repository root: Rscript tools/check-artifact.R [path/to/module.jmo]
args <- commandArgs(trailingOnly = TRUE)
version <- unname(read.dcf("TNAModule/DESCRIPTION", fields = "Version")[1L])
artifact <- if (length(args)) args[[1]] else sprintf("TNAModule/JTNA_%s.jmo", version)
stopifnot(file.exists(artifact))
extracted <- tempfile("jtna-artifact-")
dir.create(extracted)
extracted <- normalizePath(extracted)
utils::unzip(artifact, exdir = extracted)
artifact_lib <- file.path(extracted, "JTNA", "R")
jamovi_lib <- "/Applications/jamovi.app/Contents/Resources/modules/jmv/R"
.libPaths(c(artifact_lib, if (dir.exists(jamovi_lib)) jamovi_lib, .libPaths()))
library(JTNA)
stopifnot(startsWith(find.package("JTNA"), extracted))
stopifnot(identical(as.character(utils::packageVersion("JTNA")), version))
metadata <- yaml::read_yaml(file.path(extracted, "JTNA", "jamovi.yaml"))
stopifnot(identical(as.character(metadata$version), version))
stopifnot(identical(formals(JTNA::TNA)$bootstrap_threshold_use, FALSE),
          formals(JTNA::TNA)$centrality_stability_iteration ==
            eval(formals(getFromNamespace("estimate_cs.tna", "tna"))$iter),
          formals(JTNA::GroupTNA)$compare_sequences_min_freq ==
            eval(formals(getFromNamespace("compare_sequences.group_tna", "tna"))$min_freq))
pins <- c(tna = "a7638bfa90ddf85e752199a3a01f1598d8f62311",
          codyna = "845534f0b55ca1ddbd01237b02c4c03a9891619a",
          cograph = "39f2aa200b9fb31c9647a7dbab26a18aed77166a")
invisible(lapply(names(pins), function(package) {
  stopifnot(startsWith(find.package(package), extracted))
  installed <- read.dcf(file.path(find.package(package), "DESCRIPTION"), fields = "RemoteSha")[1L]
  stopifnot(identical(unname(installed), unname(pins[[package]])))
}))

# Generated exported functions require explicit NULL for unset variable roles.
run <- function(analysis, ...) {
  fn <- getExportedValue("JTNA", analysis)
  options <- list(...)
  required <- names(Filter(function(x) identical(x, quote(expr = )), formals(fn)))
  options[setdiff(required, names(options))] <- list(NULL)
  do.call(fn, options)
}
data <- data.frame(actor = rep(seq_len(100), each = 4),
  action = rep(c("A", "B", "C", "D", "A", "C", "B", "D"), 50),
  group = rep(c(rep("large", 90), rep("small", 10)), each = 4))
result <- run("GroupTNA", data = data,
  buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
  buildModel_variables_long_group = "group", pattern_show_table = TRUE,
  pattern_len_min = 2, pattern_len_max = 2, pattern_min_count = 1,
  bootstrap_show_table = TRUE, bootstrap_iteration = 10,
  random_seed_use = TRUE, random_seed = 7)
patterns <- result$patterns$table$asDF
stopifnot(patterns$p_value[patterns$pattern == "A->B"] == 1,
          patterns$chisq[patterns$pattern == "A->B"] == 0,
          grepl(paste0("JTNA ", version), result$title, fixed = TRUE),
          !is.null(result$bootstrap$cache$state))

data <- data.frame(actor = rep(1:4, each = 4), action = rep(c("A", "", "A", ""), 4),
                   group = rep(c("G1", "G2"), each = 8))
result <- run("GroupTNA", data = data,
  buildModel_variables_long_action = "action", buildModel_variables_long_actor = "actor",
  buildModel_variables_long_group = "group", compare_sequences_show_table = TRUE,
  compare_sequences_sub_min = 2, compare_sequences_sub_max = 2,
  compare_sequences_min_freq = 1)
comparison <- result$compareSequences$table$asDF
stopifnot(nrow(comparison) == 2L, all(comparison$length == 2L),
          setequal(comparison$pattern, c('"A"->""', '""->"A"')))
cat(sprintf("Compiled JTNA %s: artifact metadata, pinned dependencies, association, cache and empty-state comparison checks passed.\n", version))
