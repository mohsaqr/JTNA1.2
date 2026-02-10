# What's New in JTNA v1.6.0

> Compared to v1.4.0 (previous release on main)

---

## New Module: Cluster Transition Network Analysis

A brand-new **ClusterTNA** module that automatically groups sequences into clusters and builds separate TNA models for each cluster.

- **Clustering options** — Choose the number of clusters (*k*), dissimilarity measure, and clustering method
- **Per-cluster analysis** — Each cluster gets its own transition matrix, network plot, centrality table, community detection, cliques, and bootstrap analysis
- **Cluster comparison** — Compare any two clusters side-by-side with network difference plots, summary statistics, and bar charts
- **Sequence comparison** — Compare subsequence frequencies across clusters with statistical testing and p-value correction
- **Permutation test** — Test whether transition differences between clusters are statistically significant
- **Sequence indices** — Compute per-sequence complexity and entropy indices for each cluster

---

## New Features in TNA

### Centrality Stability Analysis
Assess the robustness of centrality measures using subset bootstrapping:
- Drop a fraction of sequences and recompute centralities across many iterations
- Stability table and correlation plot for InStrength, OutStrength, and Betweenness
- Configurable number of iterations, threshold, and certainty level

### Pattern Discovery (via codyna)
Discover recurring sequential patterns in the data:
- Support for ordered and unordered pattern types, or define a custom pattern
- Filter by pattern length, gap range, minimum support, and minimum count
- Constrain patterns by starting, ending, or containing specific states
- Full results table with pagination controls

### Sequence Indices
Compute per-sequence summary statistics:
- Complexity, entropy, and turbulence indices for each sequence
- Define favorable states and omega parameter for fine-tuning
- Sortable table with pagination

### Bootstrap Table Enhancements
- Filter to show only significant edges
- Configurable maximum rows and "show all" toggle
- Pagination for large result tables

### Lambda Parameter for Model Building
- New lambda parameter for co-occurrence model construction, providing finer control over temporal weighting

---

## New Features in Group TNA

### Group Comparison
Compare any two groups directly within the module:
- Select groups *i* and *j* for pairwise comparison
- **Summary table** with transition matrix differences
- **Network difference plot** showing which edges differ between groups, with full plot customization (cut, layout, node size, edge labels)
- **Bar chart** comparison with multiple plot types and scaling options

### Sequence Comparison
- Compare subsequence frequencies across groups
- Configurable subsequence length, minimum frequency, and p-value correction method
- Results table and comparison plot

### Permutation Test Enhancements
- Results table with pagination controls (`max_rows`, `show_all`)

### Sequence Indices
- Per-sequence complexity and entropy indices, computed within each group
- Favorable states and omega parameter

### Bootstrap Table Enhancements
- Same filtering and pagination improvements as TNA (significant-only, max rows, show all)

### Lambda Parameter
- Same co-occurrence lambda parameter as TNA

---

## Improvements Across All Modules

### Sample Dataset
- **Regulation_long** dataset bundled with the module for immediate testing
- Contains Actor, Group, Time, and Action columns with regulation strategies (cohesion, consensus, discuss, synthesis, etc.)

### Cliques Grid Plot
- All cliques now displayed in a single combined grid plot instead of individual separate plots
- Automatic grid layout based on number of cliques found

### Robust Error Handling
- All plot rendering methods wrapped in `tryCatch` with informative error messages
- Graceful fallback when plots cannot be rendered

### References
- Full academic citations included: Saqr et al. (2025), Tikka et al. (2025), and Girault et al. (2025)

---

## Summary

| Feature | v1.4.0 | v1.6.0 |
|---|---|---|
| Modules | TNA, Group TNA | TNA, Group TNA, **Cluster TNA** |
| Centrality stability | -- | InStrength, OutStrength, Betweenness |
| Pattern discovery | -- | Ordered, unordered, custom patterns |
| Sequence indices | -- | Complexity, entropy, turbulence |
| Group/cluster comparison | -- | Network diff, bar charts, summary |
| Sequence comparison | -- | Subsequence frequency testing |
| Sample dataset | -- | Regulation_long |
| Bootstrap table filtering | -- | Significant-only, pagination |
| Cliques display | 6 separate plots | Single grid plot |
