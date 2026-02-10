# JTNA v1.6 — A Major Upgrade for Transition Network Analysis in jamovi

We are excited to announce **JTNA v1.6**, a major release that significantly expands the capabilities of the JTNA module for [jamovi](https://www.jamovi.org/). This update introduces a new analysis module, powerful new analytical features, and a range of quality-of-life improvements — all accessible through jamovi's intuitive point-and-click interface with no coding required.

---

## New Module: Cluster TNA

JTNA v1.6 introduces a brand-new **Cluster Transition Network Analysis** module that brings person-centered analysis to TNA. This module automatically groups individual sequences into clusters based on their transition patterns, then builds and analyzes a separate TNA model for each cluster.

**Key capabilities:**

- **Automatic clustering** of sequences using configurable number of clusters, dissimilarity measures, and clustering methods
- **Per-cluster network models** — each cluster gets its own transition matrix, network plot, centrality table, community detection, and cliques analysis
- **Between-cluster comparison** — compare any two clusters side-by-side with network difference plots, summary statistics, and visual bar charts
- **Subsequence comparison** — test whether specific subsequences differ in frequency across clusters, with p-value correction
- **Permutation testing** — assess whether transition differences between clusters are statistically significant
- **Bootstrap validation** — evaluate the reliability of each cluster's transition network

---

## New Features in TNA

### Centrality Stability Analysis

How robust are your centrality results? The new **centrality stability analysis** answers this question by systematically dropping a fraction of sequences and recomputing centralities across many iterations. The results are presented as:

- A **stability table** showing correlation stability coefficients for each measure
- A **correlation plot** visualizing how centrality rankings hold up under data subsampling
- Configurable for **InStrength**, **OutStrength**, and **Betweenness** centrality

### Pattern Discovery

Powered by the [`codyna`](https://github.com/santikka/codyna) package, the new **pattern discovery** feature identifies recurring sequential patterns in your data:

- Discover **ordered** or **unordered** patterns, or define a **custom pattern** to search for
- Control pattern **length** and **gap** ranges to find patterns of varying complexity
- Set **minimum support** and **count** thresholds to filter meaningful patterns
- Constrain results to patterns that **start with**, **end with**, or **contain** specific states

### Sequence Indices

Compute per-sequence summary statistics that capture the complexity and structure of individual sequences:

- **Complexity**, **entropy**, and **turbulence** indices for each sequence
- Define **favorable states** and fine-tune with the **omega** parameter
- Full sortable table with pagination

### Attention Model Support

A new **attention** model type is now available, with a configurable **lambda** (decay rate) parameter that controls temporal weighting — giving more recent transitions greater influence in the model.

---

## New Features in Group TNA

### Group Comparison

Directly compare any two groups within the Group TNA module:

- Select groups for **pairwise comparison**
- View a **summary table** of transition matrix differences
- Visualize differences with a **network difference plot** showing which edges differ between groups
- Compare groups with **bar charts** using multiple plot types and scaling options

### Subsequence Comparison

Test whether specific subsequences occur at different rates across groups:

- Configure **subsequence length**, **minimum frequency**, and **p-value correction** method
- View results in a **comparison table** and **visual plot**

### Permutation Testing Enhancements

Permutation test results now include **pagination controls** for easier navigation of large result tables.

---

## Improvements Across All Modules

### Bundled Sample Dataset

JTNA v1.6 ships with the **Regulation_long** dataset, ready for immediate use. This dataset contains self-regulated learning data with Actor, Group, Time, and Action columns — perfect for exploring TNA features without preparing your own data first.

### Cliques Grid Plot

Cliques are now displayed in a **single combined grid plot** instead of multiple separate plots, making it easier to compare and interpret all cliques at a glance.

### Bootstrap Table Filtering

Bootstrap results tables now support:

- Filtering to show **only significant edges**
- Configurable **maximum rows** and **show all** toggle
- Smooth **pagination** for large result sets

### Robust Error Handling

All plot rendering methods are now wrapped with comprehensive error handling. When a plot cannot be rendered, you get a clear, informative error message instead of a crash.

### Updated References

Full academic citations are now included in the module output:

- Saqr, M., López-Pernas, S., et al. (2025). *Transition network analysis: A novel framework for modeling, visualizing, and identifying the temporal patterns of learners and learning processes.* LAK '25.
- Tikka, S., López-Pernas, S., & Saqr, M. (2025). *tna: An R Package for Transition Network Analysis.* Applied Psychological Measurement.
- Girault, D., Saqr, M., López-Pernas, S., & Tikka, S. (2025). *JTNA: A jamovi module for Transition Network Analysis.*

---

## At a Glance

| | v1.4 | v1.6 |
|---|---|---|
| **Modules** | TNA, Group TNA | TNA, Group TNA, Cluster TNA |
| **Centrality stability** | -- | Subset bootstrap with correlation plots |
| **Pattern discovery** | -- | Ordered, unordered, and custom patterns |
| **Sequence indices** | -- | Complexity, entropy, turbulence |
| **Group/cluster comparison** | -- | Network diff plots, bar charts, summaries |
| **Subsequence comparison** | -- | Frequency testing with p-value correction |
| **Attention model** | -- | With configurable lambda decay |
| **Sample dataset** | -- | Regulation_long (bundled) |
| **Bootstrap filtering** | Basic table | Significant-only filter, pagination |
| **Cliques display** | Separate plots | Combined grid plot |

---

## Getting Started

1. Download and install [jamovi](https://www.jamovi.org/)
2. Install the JTNA module from the jamovi library (or install the `.jmo` file manually)
3. Open the bundled **Regulation_long** dataset or load your own data
4. Navigate to the **JTNA** menu and select your analysis type

For more information on TNA, visit the [tna package website](https://sonsoles.me/tna/) or consult the references above.

---

*JTNA is developed by Dylan Girault, Mohammed Saqr, Santtu Tikka, and Sonsoles López-Pernas.*
