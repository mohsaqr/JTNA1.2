# JTNA - Jamovi Transition Network Analysis Module

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Version: 1.6.1](https://img.shields.io/badge/Version-1.6.1-blue.svg)]()
[![Jamovi](https://img.shields.io/badge/Jamovi-Compatible-green.svg)](https://www.jamovi.org/)

A comprehensive **jamovi module** for performing **Transition Network Analysis (TNA)** to study relational dynamics and behavioral patterns in sequential data. JTNA provides an intuitive point-and-click interface for advanced network analysis techniques -- no coding required.

## Analysis Modules

| Module | Description |
|--------|-------------|
| **TNA** | Individual transition network analysis with network plots, centrality, bootstrap, community detection, cliques, pattern discovery, and sequence indices |
| **Group TNA** | Multi-group comparison analysis with pairwise group comparison, permutation tests, network difference plots, and subsequence comparison |
| **Cluster TNA** | Person-centered analysis that automatically clusters sequences based on transition patterns, then builds and analyzes a separate TNA model for each cluster |

## Features

### Model Building
- Build transition networks from sequential behavioral data in **long format**
- Support for **relative**, **frequency**, **co-occurrence**, and **attention** model types
- Configurable scaling (MinMax, Max, Rank) and threshold parameters
- **Attention model** with lambda decay parameter for temporal weighting
- Transition matrix, network plot, histogram, frequency plot, and mosaic plot outputs

### Centrality Analysis
- Calculate **OutStrength**, **InStrength**, **Closeness** (In/Out), **Betweenness**, **BetweennessRSP**, **Diffusion**, and **Clustering** centrality measures
- Centrality table and centrality plot
- **Centrality stability analysis**: subset bootstrap with configurable iterations, threshold, and certainty level to assess robustness of centrality rankings

### Edge Betweenness
- Compute and visualize edge betweenness for the transition network
- Table and plot outputs with customizable layout options

### Community Detection
- Detect communities using **Spinglass**, **Walktrap**, **Fast Greedy**, **Label Propagation**, **Infomap**, **Edge Betweenness**, or **Leading Eigenvector** methods
- Community assignment table and community plot
- Configurable gamma parameter

### Cliques Analysis
- Identify cliques of configurable size and threshold
- **Combined grid plot** displaying all cliques in a single visualization

### Bootstrap Validation
- Bootstrap confidence intervals with configurable iterations, level, and method (stability/threshold)
- Results table with edge weights, p-values, confidence ranges, and significance indicators
- **Filter to show only significant edges**, configurable maximum rows, and show-all toggle

### Sequence Analysis
- Sequence visualization with **index** and **distribution** plot types
- **Bar** and **area** geometries with proportion or count scaling
- Configurable tick intervals and missing value handling

### Pattern Discovery (via codyna)
- Discover recurring sequential patterns using the [codyna](https://github.com/santikka/codyna) package
- Support for **ordered**, **unordered**, and **custom** pattern types
- Filter by pattern length range, gap range, minimum support, and minimum count
- Constrain patterns by starting, ending, or containing specific states

### Sequence Indices
- Compute per-sequence **complexity**, **entropy**, and **turbulence** indices
- Define favorable states and omega parameter for fine-tuning
- Sortable table with pagination

### Group Comparison (Group TNA & Cluster TNA)
- **Pairwise comparison** of any two groups or clusters
- **Summary metrics table** with density, reciprocity, and global network properties
- **Network comparison table** with edge-level differences, correlation, and distance
- **Network difference plot** visualizing which edges differ between groups
- **Bar chart comparison** with multiple plot types and scaling options

### Subsequence Comparison (Group TNA & Cluster TNA)
- Test whether specific subsequences occur at different rates across groups or clusters
- Configurable subsequence length, minimum frequency, and p-value correction method
- Results table and comparison plot

### Permutation Testing (Group TNA & Cluster TNA)
- Assess statistical significance of transition differences between groups or clusters
- Configurable iterations, paired option, and significance level
- Results table with pagination controls

### Bundled Sample Dataset
- **Regulation_long** dataset included for immediate testing
- Contains Actor, Group, Time, and Action columns with self-regulated learning data

## Installation

### From jamovi Library (Recommended)
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
| **Time** | No | Timestamp for ordering events chronologically |
| **Order** | No | Alternative to Time -- integer ordering of events |
| **Group** | Group TNA only | Column defining groups for comparison |

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
