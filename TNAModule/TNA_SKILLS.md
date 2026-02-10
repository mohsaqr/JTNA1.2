# TNA Package Skills Reference

This document describes how to use the `tna` R package functions. All functions return **ready-to-use outputs** requiring no manual loops or post-processing.

---

## Skill 1: Data Preparation

Prepare long-format data for TNA analysis.

```r
library(tna)

# Required: data frame with at least an action column
# Optional: actor, time, order columns

args <- list(
  data = myData,
  action = "action_column",
  actor = "actor_column",      # Optional: separates sequences by person
  time = "time_column",        # Optional: for chronological ordering
  order = "order_column",      # Optional: alternative to time
  time_threshold = 30          # Optional: max gap between events (minutes)
)

# Remove NULL arguments
args <- args[!sapply(args, is.null)]

# Prepare data
preparedData <- do.call(tna::prepare_data, args)

# Output structure:
# preparedData$long_data      - Long format with .session_id added
# preparedData$sequence_data  - Wide format (one row per actor)
```

---

## Skill 2: Build Single Model

Create a single TNA model (aggregated across all actors).

```r
# Types: "relative" (probabilities), "frequency" (counts), "attention" (time-weighted)
# Scaling: character(0) for none, or "minmax", "max", "rank"

model <- tna::build_model(
  x = preparedData,
  type = "relative",
  scaling = character(0)  # No scaling
)

# For attention type, add lambda parameter:
model <- tna::build_model(
  x = preparedData,
  type = "attention",
  scaling = "minmax",
  lambda = 0.5
)

# Output: TNA model object
# - Can be plotted directly: plot(model)
# - Can be passed to all analysis functions
```

---

## Skill 3: Build Group Model

Create multiple TNA networks by group variable. **One function call creates all groups.**

```r
# Get unique group per session
groupVector <- preparedData$long_data[
  !duplicated(preparedData$long_data$.session_id),
  "group_column"
]

model <- tna::group_model(
  x = preparedData,
  group = groupVector,
  type = "relative",
  scaling = character(0)
)

# Output: Grouped TNA model object (list of networks)
# - length(model) gives number of groups
# - names(model) gives group names
# - All analysis functions work identically on group models
```

---

## Skill 4: Cluster Sequences

Automatically cluster actors based on behavioral similarity, then build group model.

```r
# Step 1: Cluster sequences
clusters <- tna::cluster_sequences(
  data = preparedData$sequence_data,
  k = 3,                          # Number of clusters
  dissimilarity = "manhattan",    # Distance metric
  method = "pam"                  # Clustering method
)

# Step 2: Build group model from clusters
model <- tna::group_model(clusters, type = "relative")

# Output: Same as group_model - clustered TNA model
```

---

## Skill 5: Calculate Centralities

Get centrality measures for all nodes. **Returns ready data frame.**

```r
centralities_df <- tna::centralities(
  x = model,
  loops = TRUE,           # Include self-loops
  normalize = TRUE,       # Normalize values
  measures = c("OutStrength", "InStrength", "Betweenness", "Closeness")
)

# Available measures:
# "OutStrength", "InStrength", "ClosenessIn", "ClosenessOut",
# "Closeness", "Betweenness", "BetweennessRSP", "Diffusion", "Clustering"

# OUTPUT FOR SINGLE MODEL:
# | state   | OutStrength | InStrength | Betweenness | Closeness |
# |---------|-------------|------------|-------------|-----------|
# | StateA  | 0.45        | 0.32       | 12          | 0.67      |
# | StateB  | 0.28        | 0.51       | 8           | 0.54      |

# OUTPUT FOR GROUP MODEL (adds group column automatically):
# | group | state   | OutStrength | InStrength | Betweenness | Closeness |
# |-------|---------|-------------|------------|-------------|-----------|
# | A     | StateA  | 0.45        | 0.32       | 12          | 0.67      |
# | A     | StateB  | 0.28        | 0.51       | 8           | 0.54      |
# | B     | StateA  | 0.38        | 0.41       | 10          | 0.62      |

# Plot centralities:
plot(centralities_df)
```

---

## Skill 6: Centrality Stability (CS-Coefficient)

Estimate stability of centrality measures via case-dropping bootstrap.

```r
cs_result <- tna::estimate_cs(
  x = model,
  loops = TRUE,
  normalize = TRUE,
  measures = c("InStrength", "OutStrength", "Betweenness"),
  iter = 1000,           # Bootstrap iterations
  threshold = 0.7,       # Correlation threshold
  certainty = 0.95,      # Confidence level
  progressbar = FALSE
)

# Output: Named list with CS-coefficient per measure
# cs_result$InStrength$cs_coefficient  -> e.g., 0.75
# cs_result$OutStrength$cs_coefficient -> e.g., 0.68

# Plot stability curves:
plot(cs_result)
```

---

## Skill 7: Bootstrap Analysis

Test edge significance via bootstrap. **Returns ready data frame.**

```r
bs <- tna::bootstrap(
  x = model,
  iter = 1000,                    # Bootstrap iterations
  level = 0.95,                   # Confidence level
  method = "percentile",          # "percentile" or "bca"
  threshold = 0,                  # Minimum edge weight
  consistency_range = c(0.025, 0.975)  # Consistency range
)

# OUTPUT FOR SINGLE MODEL:
# bs$summary is a ready data frame:
# | from   | to     | weight | p_value | cr_lower | cr_upper | ci_lower | ci_upper | sig   |
# |--------|--------|--------|---------|----------|----------|----------|----------|-------|
# | StateA | StateB | 0.35   | 0.001   | 0.28     | 0.42     | 0.30     | 0.40     | TRUE  |
# | StateB | StateA | 0.12   | 0.234   | 0.05     | 0.19     | 0.08     | 0.16     | FALSE |

# OUTPUT FOR GROUP MODEL:
# bs is a named list, one entry per group:
# bs[["GroupA"]]$summary  -> data frame for Group A
# bs[["GroupB"]]$summary  -> data frame for Group B

# Access group results:
for (group_name in names(bs)) {
  summary_df <- bs[[group_name]]$summary
  # Process summary_df...
}

# Plot bootstrap results:
plot(bs, cut = 0.01)
```

---

## Skill 8: Community Detection

Detect communities/clusters within the network. **Returns ready data frame.**

```r
coms <- tna::communities(
  x = model,
  methods = "spinglass",   # Detection method
  gamma = 1                # Resolution parameter
)

# Available methods:
# "spinglass", "walktrap", "fast_greedy", "label_prop",
# "infomap", "edge_betweenness", "leading_eigen"

# Output structure:
# coms$assignments is a ready data frame:
# | state   | spinglass |
# |---------|-----------|
# | StateA  | 1         |
# | StateB  | 1         |
# | StateC  | 2         |

# Plot communities:
plot(coms, method = "spinglass")
```

---

## Skill 9: Clique Analysis

Find densely connected subgroups.

```r
cliques <- tna::cliques(
  x = model,
  size = 3,          # Minimum clique size
  threshold = 0.1    # Minimum edge weight
)

# Output: Cliques object
# Can be plotted with multiple cliques shown:
plot(cliques, first = 1, n = 1)  # Show 1st clique
plot(cliques, first = 2, n = 1)  # Show 2nd clique
```

---

## Skill 10: Permutation Test (Group Models Only)

Test for significant differences between groups. **Returns ready data frames.**

```r
permTest <- tna::permutation_test(
  x = model,           # Must be a group model
  iter = 1000,         # Permutation iterations
  paired = FALSE,      # Paired test?
  level = 0.95         # Confidence level
)

# Output: Named list with one entry per group comparison
# For 3 groups (A, B, C), you get: "A vs B", "A vs C", "B vs C"

# Access comparison results:
for (comparison in names(permTest)) {
  stats_df <- permTest[[comparison]]$edges$stats
  # stats_df columns: edge_name, diff_true, effect_size, p_value
}

# Example output for permTest[["A vs B"]]$edges$stats:
# | edge_name      | diff_true | effect_size | p_value |
# |----------------|-----------|-------------|---------|
# | StateA->StateB | 0.15      | 0.45        | 0.023   |
# | StateB->StateC | -0.08     | 0.22        | 0.156   |

# Plot permutation results:
plot(permTest)
```

---

## Skill 11: Compare Sequences (Group Models Only)

Compare sequential patterns between groups. **Returns ready data frame.**

```r
compSeq <- tna::compare_sequences(
  x = model,                    # Must be a group model
  sub = 2:4,                    # Pattern lengths to search
  min_freq = 5,                 # Minimum frequency
  correction = "bonferroni"     # P-value correction
)

# Output: Ready data frame with pattern comparisons
# | pattern | freq_A | freq_B | prop_A | prop_B | statistic | p_value |
# |---------|--------|--------|--------|--------|-----------|---------|
# | A-B-C   | 45     | 23     | 0.12   | 0.06   | 8.45      | 0.004   |
# | B-C-A   | 32     | 41     | 0.08   | 0.11   | 3.21      | 0.073   |

# Filter out patterns with NA (*):
compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

# Plot comparison:
plot(compSeq)
```

---

## Skill 12: Sequence Analysis Plot

Visualize sequences as index or distribution plots.

```r
plot_result <- tna::plot_sequences(
  x = model,
  type = "index",        # "index" or "distribution"
  scale = "absolute",    # "absolute" or "relative"
  geom = "bar",          # "bar" or "area"
  include_na = FALSE,    # Include NA values?
  tick = 10              # Tick interval on x-axis
)

print(plot_result)  # ggplot object
```

---

## Skill 13: Edge Betweenness Network

Analyze edge betweenness centrality.

```r
eb_network <- tna::betweenness_network(x = model)

# Output structure:
# eb_network$weights  -> Matrix of edge betweenness values

# Convert to edge list:
edge_df <- as.data.frame(as.table(eb_network$weights))
names(edge_df) <- c("from", "to", "value")
edge_df <- edge_df[edge_df$value > 0, ]

# Plot:
plot(eb_network)
```

---

## Skill 14: Frequency Plot

Plot state frequencies.

```r
freq_plot <- tna::plot_frequencies(x = model)
print(freq_plot)  # ggplot object
```

---

## Skill 15: Mosaic Plot

Visualize transition matrix as mosaic (frequency type only).

```r
mosaic_plot <- tna::plot_mosaic(x = model, digits = 2)
print(mosaic_plot)  # ggplot object
```

---

## Skill 16: Network Plot

Plot the TNA network with customization.

```r
plot(
  x = model,
  cut = 0.1,                    # Minimum edge weight to show
  minimum = 0,                  # Minimum node size
  edge.label.cex = 0.8,         # Edge label size
  node.width = 1,               # Node size
  label.cex = 1,                # Node label size
  layout = "spring",            # Layout algorithm
  bg = "transparent"            # Background color
)

# Available layouts:
# "circle", "spring", "kamada.kawai", "fruchterman.reingold",
# "davidson.harel", "gem", "graphopt", "grid", "mds",
# "lgl", "star", "tree", "sugiyama", "drl"
```

---

## Key Principle: No Loops Needed

The TNA package handles all internal complexity. You call **one function** and get **complete results**:

```r
# WRONG - Don't do manual loops:
for (group in groups) {
  subset_data <- data[data$group == group, ]
  model <- build_model(subset_data)
  cent <- centralities(model)
  # ... manually combine results
}

# RIGHT - Single call handles everything:
model <- tna::group_model(data, group = groupColumn)
cent <- tna::centralities(model)  # Returns combined results for ALL groups
```

The only loops you write are for **populating UI tables** by iterating through the ready-made data frame rows.
