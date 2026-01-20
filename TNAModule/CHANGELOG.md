# JTNA Module - Features and Changelog

## Overview

JTNA (Jamovi Transition Network Analysis) is a comprehensive module for analyzing sequential behavioral data using network analysis techniques. The module provides tools for building, visualizing, and statistically analyzing transition networks from longitudinal data.

---

## Modules

### 1. TNA (Transition Network Analysis)

Single-subject or aggregated transition network analysis.

**Data Input:**
- Action (required)
- Actor (optional)
- Time (optional)
- Order (optional)

**Network Types:**
- Relative (transition probabilities)
- Frequency (raw transition counts)
- Attention (time-weighted transitions with decay)

**Features:**
- **Model Building**: Create transition matrices with configurable scaling (None, MinMax, Max, Rank)
- **Visualization**: Network plots with customizable layouts (Circle, Spring, Kamada-Kawai, etc.)
- **Histogram**: Distribution of edge weights
- **Frequencies Plot**: State frequency visualization
- **Mosaic Plot**: For frequency-type networks

**Analysis Options:**
- **Centrality Analysis**: OutStrength, InStrength, Closeness (In/Out), Betweenness, BetweennessRSP, Clustering, Diffusion
- **Edge Betweenness**: Identify critical edges in the network
- **Community Detection**: Spinglass, Walktrap, Fast Greedy, Label Prop, Infomap, Edge Betweenness, Leading Eigen
- **Clique Analysis**: Find densely connected subgroups
- **Bootstrap Analysis**: Statistical validation with stability and threshold methods

---

### 2. GroupTNA (Group Transition Network Analysis)

Compare transition networks across predefined groups.

**Data Input:**
- Action (required)
- Actor (required)
- Time (optional)
- Order (optional)
- Group (required)

**Network Types:**
- Relative
- Frequency
- Attention

**Features:**
- All TNA features applied per group
- **Permutation Test**: Statistical comparison between groups
- **Sequence Analysis**: Visualize action sequences by group
- **Compare Sequences**: Statistical comparison of subsequence patterns across groups

---

### 3. ClusterTNA (Cluster-based TNA)

Automatically cluster actors based on behavioral similarity, then analyze networks per cluster.

**Data Input:**
- Action (required)
- Actor (required)
- Time (optional)
- Order (optional)

**Clustering:**
- PAM (Partitioning Around Medoids) algorithm
- Configurable number of clusters (k)
- Manhattan distance on sequence data

**Features:**
- All GroupTNA features with automatic clustering
- Silhouette-based cluster quality assessment
- Compare sequences across discovered clusters

---

### 4. CoOccurrenceTNA (Co-occurrence Network Analysis)

Analyze co-occurrence patterns (undirected relationships) rather than transitions.

**Data Input:**
- Action (required)
- Actor (optional)
- Time (optional)
- Order (optional)

**Features:**
- **Co-occurrence Networks**: Capture which actions occur together within sessions
- All visualization and analysis options from TNA
- Suitable for analyzing item co-occurrence, topic co-occurrence, etc.

---

### 5. GroupCoOccurrenceTNA (Group Co-occurrence Network Analysis)

Compare co-occurrence networks across predefined groups.

**Data Input:**
- Action (required)
- Actor (required)
- Time (optional)
- Order (optional)
- Group (required)

**Features:**
- All CoOccurrenceTNA features applied per group
- **Permutation Test**: Statistical comparison of co-occurrence patterns between groups
- Multi-group visualization with automatic layout

---

## Common Features Across All Modules

### Visualization Settings
- Network plot with configurable:
  - Cut value (minimum edge weight to display)
  - Minimum value threshold
  - Edge label size
  - Node size
  - Node label size
  - Layout algorithm (14 options)

### Centrality Measures
- Out Strength
- In Strength
- Closeness (overall, in, out)
- Betweenness
- Betweenness RSP (Randomized Shortest Paths)
- Clustering coefficient
- Diffusion centrality

### Community Detection Methods
- Spinglass
- Walktrap
- Fast Greedy
- Label Propagation
- Infomap
- Edge Betweenness
- Leading Eigenvector

### Bootstrap Analysis
- Configurable iterations (up to 10,000)
- Significance level setting
- Methods: Stability, Threshold
- Confidence intervals and p-values for edges

### Permutation Test (Group modules)
- Compare networks between groups
- Configurable iterations
- Paired test option
- Effect size calculation

---

## Changelog

### Version 1.4.0

#### New Modules
- **CoOccurrenceTNA**: Dedicated module for co-occurrence network analysis
- **GroupCoOccurrenceTNA**: Group comparison for co-occurrence networks

#### Improvements
- Reordered data input parameters consistently across all modules: Action, Actor, Time, Order
- Removed co-occurrence type from GroupTNA (now has dedicated module)
- Fixed multi-group plot display in GroupCoOccurrenceTNA
- Improved plot function patterns for consistent multi-group visualization

#### Technical Changes
- Standardized plot functions to match ClusterTNA's working pattern
- Added helper functions for clique plotting
- Simplified null checks in render functions

### Version 1.3.0
- Added ClusterTNA module with automatic PAM clustering
- Added Compare Sequences feature for pattern comparison
- Added Sequence Analysis visualization

### Previous Versions
- Initial TNA and GroupTNA modules
- Centrality, community detection, clique analysis
- Bootstrap and permutation testing

---

## Data Format

All modules expect data in **long format** with one row per action/event:

| Actor | Action | Time | Order | Group |
|-------|--------|------|-------|-------|
| user1 | A | 2024-01-01 10:00 | 1 | treatment |
| user1 | B | 2024-01-01 10:05 | 2 | treatment |
| user1 | A | 2024-01-01 10:10 | 3 | treatment |
| user2 | C | 2024-01-01 11:00 | 1 | control |
| ... | ... | ... | ... | ... |

---

## Dependencies

- R package: `tna` (Transition Network Analysis)
- jamovi >= 2.0

---

## Authors

JTNA Module developed for jamovi statistical software.
