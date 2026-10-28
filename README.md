# JTNA - Jamovi Transition Network Analysis Module

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version: 1.9.4](https://img.shields.io/badge/Version-1.9.4-blue.svg)](https://github.com/mohsaqr/JTNA1.2/releases)
[![tests](https://github.com/mohsaqr/JTNA1.2/actions/workflows/tests.yml/badge.svg)](https://github.com/mohsaqr/JTNA1.2/actions/workflows/tests.yml)
[![Jamovi](https://img.shields.io/badge/Jamovi-Compatible-green.svg)](https://www.jamovi.org/)

A comprehensive **jamovi module** for performing **Transition Network Analysis (TNA)** to study relational dynamics and behavioral patterns in sequential data. JTNA provides an intuitive point-and-click interface for advanced network analysis techniques -- no coding required.

JTNA is a front end to the [tna](https://sonsoles.me/tna/) and [codyna](https://github.com/santikka/codyna) R packages. It uses their model and sequence-analysis algorithms, with corrections for verified label-handling and statistical defects described below. Inferential defaults follow tna, and unsupported globally scaled analyses report tna's refusal.

## What's New in 1.9.4

- **Correct pattern association tests:** group and cluster pattern tables test sequence-level pattern presence/absence against group membership, accounting for unequal group sizes. They no longer use codyna 1.0.0's equal-count goodness-of-fit test. Re-run earlier pattern/group significance results.
- **Literal states stay distinct:** empty labels, `*`, `%`, and labels containing `->` retain their identity in clustering, patterns, and indices. Pattern filters select exact states; literal asterisk states have the correct lift. Blank table labels are quoted so they are distinguishable from missing values.
- **Correct labels and complete output:** scaled grouped centralities retain their state names, blank-named groups retain their matrices, and grouped clique plots show every computed clique.
- **Cosmetic changes preserve statistics:** changing bootstrap, permutation, or community plot appearance no longer reruns the numerical analysis.
- **Defaults aligned:** clique threshold zero is honored literally; sequence-comparison minimum frequency is 5; stability uses 1,000 iterations. Threshold bootstrap uses tna's automatic weight quantile unless **Set threshold** is selected. Review that setting when reopening analyses created with earlier versions.
- **Scaled permutation behavior aligned:** globally scaled models now receive tna's unsupported-analysis message. The previous silent unscaled rebuild has been removed.
- **Clear clustering limit:** the bundled tna encoder supports 62 symbols, including symbols allocated for missing/padded events. Data exceeding its capacity receive an actionable message.

The compatibility corrections preserve the selected algorithms and model weights. Pattern/group p-values intentionally differ from codyna's erroneous equal-count statistic; label encoding and clustering's literal-state handling intentionally correct upstream interpretation of ordinary long-format values.

## What's New in 1.9.3

- **Mosaic plot in Group and Cluster TNA** for every model type, as in tna (states by group). It used to appear only with Type set to Frequency, which tna requires only for a single network.
- **Version stamp**: every analysis shows the module version in its title, for example *Transition Network Analysis (JTNA 1.9.3)*.

## What's New in 1.9.2

The first release since 1.8.4. It includes 1.9.0 and 1.9.1, which fix every finding of the jamovi library's audit of 1.7.1 and a further code review.

**Results that were wrong before**
- **Group labels**: Group TNA could show a network under another group's name when the data did not start with the alphabetically first group (also in 1.8). Re-run group analyses made with earlier versions.
- **Cluster TNA honours Type, Scaling and Lambda**: it used to build a relative model whatever was chosen.
- **Actors in more than one group** (for example before/after) keep separate sequences in each group instead of being merged into one.
- **Paired permutation tests** require the same actors in both groups; they used to pair unrelated rows whenever the group sizes matched.
- **The sequence comparison correction** now takes effect, and comparisons stay in the right direction with 10 or more clusters.
- **Event order** is kept for numeric Order values and across daylight-saving changes; ISO time stamps (`2025-01-01T08:27:07Z`) keep their time of day.

**Defaults and behaviour follow tna**
- **Edge betweenness is directed** again (tna's default), as before 1.6.6.
- **Grouped models with scaling are scaled across groups**, as in tna. With tna 1.3.1, bootstrap, permutation, sequence comparison, community detection and network comparison are not available for such models; those sections show tna's message and the rest of the analysis runs.
- **Include missing values** in sequence plots is off by default, as in tna.
- Labels such as `*` or `A->B` are ordinary states, as in tna.

**New options**
- **Reproducible results on request**: an optional seed for bootstrap, permutation, stability, community detection, sequence comparison and clustering.
- **Multiple-testing correction** for permutation tests (off by default, as in tna).
- More clustering distances and linkage methods, weighted Hamming, inverted weights for path-based centralities, summed-weight cliques.
- **Sequence plots revamped**: show the index and distribution plots together, and control sorting, titles, axis and legend labels, colours, tick interval, facet columns and size.

**Easier to use**
- **One failing section no longer stops the rest**: a problem (for example community detection on a disconnected network) is reported on that section, and every other section still runs.
- **Clear messages** for data that cannot be analysed (no transitions, sequences too short for patterns or indices) instead of R errors. Pattern discovery adapts its length and gap to short sequences and says so.
- **Every panel control works**, the mosaic and frequencies plots draw, and the transition matrix and initial probabilities are proper tables you can copy into Excel or Word.
- **The same sessions everywhere**: the model, pattern discovery and sequence indices use one prepared data set, so the session gap applies to all of them. Missing Group values are left out and counted.

**Tested**: the suites exercise actual analysis classes, seeded upstream comparisons, independent calculations for selected statistics, and an optional option sweep (see [Testing](#testing)).

## What's New in 1.8

- **Pattern discovery in groups & clusters** — Group/Cluster TNA pattern tables now show a per-group breakdown with χ² and p-value (previously dropped whenever a Time threshold split actors into sessions).
- **Scaled models could permute through an unscaled rebuild** — this 1.8 behavior was removed in 1.9.4 to preserve the displayed model and respect tna's scaling restriction.
- **Correct event ordering** — the Order column is treated numerically, so sequences longer than 9 events are no longer mis-sorted; Cluster TNA now honours Order and the session Threshold too.
- **Attention models** build directly with a heads-up that they can be slower on long sequences.
- Default **edge label size** is now **0.7** for cleaner plots.

See the [Releases](https://github.com/mohsaqr/JTNA1.2/releases) page for full release notes.

## Analysis Modules

| Module | Description |
|--------|-------------|
| **TNA** | Individual transition network analysis with network plots, centrality, bootstrap, community detection, cliques, pattern discovery, and sequence indices |
| **Group TNA** | Multi-group comparison analysis with pairwise group comparison, permutation tests, network difference plots, and subsequence comparison |
| **Cluster TNA** | Person-centered analysis that clusters sequences by their dissimilarity, then builds and analyzes a separate TNA model for each cluster |

## Features

### Model Building
- Build transition networks from sequential behavioral data in **long format**
- Support for **relative**, **frequency**, and **attention** model types
- Configurable scaling (MinMax, Max, Rank), session threshold, and attention **lambda**
- Transition matrix, initial probabilities, network plot, histogram, frequency plot, and mosaic plot outputs

### Centrality Analysis
- Calculate **OutStrength**, **InStrength**, **Closeness** (In/Out), **Betweenness**, **BetweennessRSP**, **Diffusion**, and **Clustering** centrality measures
- Optional loops, normalisation, and inverted weights for path-based measures
- Centrality table and centrality plot
- **Centrality stability analysis**: case-dropping bootstrap with configurable iterations, threshold, and certainty level to assess robustness of centrality rankings

### Edge Betweenness
- Directed edge betweenness of the transition network (tna's default)
- Table and plot outputs with customizable layout options

### Community Detection
- Detect communities using **Spinglass**, **Walktrap**, **Fast Greedy**, **Label Propagation**, **Infomap**, **Edge Betweenness**, or **Leading Eigenvector** methods
- Community assignment table and community plot
- Configurable gamma parameter

### Cliques Analysis
- Identify cliques of configurable size and threshold, optionally using summed weights
- **Combined grid plot** displaying all cliques in a single visualization

### Bootstrap Validation
- Bootstrap confidence intervals with configurable iterations, level, and method (stability/threshold)
- Results table with edge weights, p-values, confidence ranges, and significance indicators
- **Filter to show only significant edges**, configurable maximum rows, and show-all toggle

### Sequence Plots
- **Index** and **distribution** plots, shown separately or together
- **Bar** and **area** geometries with proportion or count scaling
- Sorting, titles, axis and legend labels, colours, tick interval, facet columns, plot size, and missing-value handling

### Pattern Discovery (via codyna)
- Discover recurring sequential patterns using the [codyna](https://github.com/santikka/codyna) package
- Support for **N-gram**, **gapped**, and **repeated** pattern types
- Filter by pattern length range, gap range, minimum support, and minimum frequency
- Constrain patterns by starting, ending, or containing specific states (checkbox-gated level pickers)
- In **Group TNA** / **Cluster TNA**, patterns include per-group/cluster sequence counts and an unadjusted Pearson χ² test of pattern presence/absence by group, without continuity correction

### Sequence Indices
- Compute 20+ per-sequence indices via [codyna](https://github.com/santikka/codyna) — complexity, longitudinal entropy, Simpson diversity, spell durations, transition rate/complexity, self-loop tendency, and initial/dominant/emergent-state metrics
- Define favorable states and omega parameter for fine-tuning
- Sortable table with pagination

### Clustering (Cluster TNA)
- Distances: **Hamming** (optionally weighted), **OSA**, **Levenshtein**, **Damerau-Levenshtein**, **LCS**, **Jaccard**, **Jaro-Winkler**, **q-gram**, and **cosine**
- Methods: **PAM** and hierarchical clustering (**Ward D/D2**, complete, average, single, McQuitty, median, centroid)

### Group Comparison (Group TNA & Cluster TNA)
- **Pairwise comparison** of any two groups or clusters
- **Summary metrics table** with density, reciprocity, and global network properties
- **Network comparison table** with edge-level differences, correlation, and distance
- **Network difference plot** visualizing which edges differ between groups
- **Bar chart comparison** with multiple plot types and scaling options

### Subsequence Comparison (Group TNA & Cluster TNA)
- Test whether specific subsequences occur at different rates across groups or clusters
- Configurable subsequence length, minimum frequency, and p-value correction method (Bonferroni by default, as in tna)
- Results table with effect sizes, and comparison plot

### Permutation Testing (Group TNA & Cluster TNA)
- Assess statistical significance of transition differences between groups or clusters
- Configurable iterations, paired option, significance level, and multiple-testing correction
- Results table with pagination controls

### Reproducibility
- Optional **random seed** (off by default, as in tna) for every resampling or randomised step: bootstrap, permutation, stability, community detection, sequence comparison and clustering

### Bundled Sample Dataset
- **Regulation_long** dataset included for immediate testing
- Contains Actor, Group, Time, and Action columns with self-regulated learning data

## Installation

### From a release (recommended)
1. Download the latest `.jmo` file from the [Releases](https://github.com/mohsaqr/JTNA1.2/releases) page
2. Open **jamovi**
3. Go to **Modules** (+ icon) > **Sideload** > select the `.jmo` file
4. Restart jamovi

### Development Installation
```r
install.packages('jmvtools', repos=c('https://repo.jamovi.org', 'https://cran.r-project.org'))
library(jmvtools)
options(jamovi_home="/path/to/your/jamovi")
setwd("path/to/TNAModule")
jmvtools::install()
```

tna, codyna and cograph are installed from the commits pinned in `Remotes` in `TNAModule/DESCRIPTION`, because jamovi's package snapshot has older versions. After changing a dependency, delete `TNAModule/build/` before running `jmvtools::install()` again, or the old packages are reused.

## Testing

```r
# From TNAModule/: regression, equivalence and integration suites
Sys.setenv(NOT_CRAN = "true")
testthat::test_dir("tests/testthat")

# Every value of every option in each analysis (about 10 minutes)
Sys.setenv(NOT_CRAN = "true", JTNA_EXTENSIVE = "true")
testthat::test_file("tests/testthat/test-option-sweep.R")
```

```sh
# From the repository root, after jmvtools::install(): the suite against the
# packages shipped in the .jmo and jamovi's own R library
Rscript tools/check-bundle.R
```

The suite checks results tables against direct, seeded upstream calls, with independent label/edge matching. Base-R and igraph references cover transition counts, initial probabilities, selected centralities, bigrams, selected sequence indices, edge betweenness, and pattern/group contingency tests. A 100-data-set Monte Carlo check covers relative-model counts and invariants; further invariants run on 30 data sets. These are complementary checks, not independent proofs of every upstream algorithm.

Regression tests include literal-state relabeling, unequal group sizes, zero thresholds, complete clique rendering, and cache restoration without changing RNG state. The optional sweep exercises one option at a time; full saved-file persistence and the installed jamovi GUI lifecycle remain separate validation tasks. CI is configured for Linux, macOS and Windows.

## Quick Start

1. Open jamovi and load your data in **long format** (one row per event)
2. Go to **JTNA** in the menu bar and select your analysis type
3. Assign your **Action** column (required) and optionally **Actor**, **Time**/**Order**, and **Group** columns
4. Results appear automatically -- use the checkboxes in the options panel to show/hide specific outputs

Or try the bundled dataset: **Open** > **Data Library** > **Regulation_long**

## Data Format

Data should be in **long format** with one row per event/action:

| Column | Required | Description |
|--------|----------|-------------|
| **Action** | Yes | The actions, states, or events (each unique value becomes a node) |
| **Actor** | Depends | Identifies individuals (required for Group TNA and Cluster TNA) |
| **Time** | No | Timestamp for ordering events and splitting them into sessions (text date-times, ISO 8601, or Unix seconds) |
| **Order** | No | Alternative to Time -- numeric ordering of events |
| **Group** | Group TNA only | Column defining groups for comparison; rows with a missing group are left out and counted |

## Documentation & Tutorials

- [TNA Chapter](https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html) -- Introduction to Transition Network Analysis
- [Updated TNA Tutorial](https://sonsoles.me/posts/tna-tutorial) -- Step-by-step TNA tutorial
- [Group TNA Tutorial](https://sonsoles.me/posts/tna-group) -- Multi-group analysis
- [Cluster TNA Tutorial](https://sonsoles.me/posts/tna-clustering) -- Clustering with TNA
- [Frequency-based TNA](https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html) -- Advanced TNA techniques
- [TNA Clusters Chapter](https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html) -- Pattern discovery methods
- [tna R package](https://sonsoles.me/tna/) -- Underlying R package documentation

## Citation

If you use JTNA in your research, please cite:

```bibtex
@inproceedings{saqr2025transition,
  title={Transition Network Analysis: A Novel Framework for Modeling, Visualizing, and Identifying the Temporal Patterns of Learners and Learning Processes},
  author={Saqr, Mohammed and L{\'o}pez-Pernas, Sonsoles and Tikka, Santtu and others},
  booktitle={Proceedings of LAK '25},
  year={2025},
  doi={10.1145/3706468.3706513}
}
```

Additional references:
- Tikka, S., Lopez-Pernas, S., & Saqr, M. (2025). *tna: An R Package for Transition Network Analysis.* Applied Psychological Measurement. [doi:10.1177/01466216251348840](https://doi.org/10.1177/01466216251348840)
- Girault, D., Saqr, M., Lopez-Pernas, S., & Tikka, S. (2025). *JTNA: A jamovi module for Transition Network Analysis.* [GitHub](https://github.com/sonsoleslp/JTNA)

## Authors

- **Dylan Girault** -- Lead Developer
- **Mohammed Saqr** -- Development & Research
- **Santtu Tikka** -- Statistical Methods
- **Sonsoles Lopez-Pernas** -- Research & Development

**Maintainer**: Mohammed Saqr (saqr@saqr.me)

## License

MIT License -- see [LICENSE](TNAModule/LICENSE) for details.

## Bug Reports

[GitHub Issues](https://github.com/mohsaqr/JTNA1.2/issues)
