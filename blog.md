# Transition Network Analysis in Jamovi: JTNA

Transition Network Analysis (TNA) is a method that combines Stochastic Process Mining with probabilistic graph representation. It models, visualizes, and analyzes patterns in sequential data, capturing both relational and temporal aspects of behavioral dynamics.

TNA computes metrics at the graph, node, and edge levels, and identifies recurring structures such as dyads, triads, communities, and clusters. Critically, it incorporates statistical validation — bootstrapping, permutation testing, and case-dropping — to assess the robustness of findings. This makes it possible to attach a p-value and effect size to each individual edge in a network, a capability that has no precedent in conventional process modeling. TNA can also compare patterns across subgroups with statistical significance at every edge.

TNA is available as an R package with tutorials, a no-install Shiny web app at [sonsoleslp.shinyapps.io/tna-app](https://sonsoleslp.shinyapps.io/tna-app), and the JTNA plugin for jamovi described here. For the latest developments, visit [sites.uef.fi/learning-analytics/tna](https://sites.uef.fi/learning-analytics/tna).

---

## What JTNA Does

### Network Analysis

- **Multiple model types:** Build transition, frequency, and attention-weighted networks
- **Centrality measures:** Betweenness, diffusion, closeness, and degree centrality computed automatically
- **Statistical validation:** Bootstrap and permutation tests assess the stability of each transition
- **Structure detection:** Identify cliques, communities, and recurring behavioral patterns

### Sequence Analysis

- **Behavioral trajectories:** Visualize individual sequences as bar or area charts
- **Customizable output:** Control scaling, colors, and themes for publication-ready figures

### Group Comparisons

- **Cross-group analysis:** Compare TNA patterns between groups (e.g., control vs. experimental)
- **Side-by-side plots:** Comparative visualizations of network structures and sequences
- **Group-specific metrics:** Separate network statistics per group for precise comparisons

### Cluster TNA

- **Data-driven grouping:** Discover behavioral profiles directly from sequence data — no pre-existing group variable required
- **Six dissimilarity algorithms:** Hamming, OSA, Levenshtein, LCS, Jaccard, Jaro-Winkler
- **Multiple clustering methods:** PAM, Ward's D2, complete, average, and single linkage

---

## Getting Started

1. Download and install jamovi from [jamovi.org](https://jamovi.org)
2. Open jamovi, go to the **Modules** tab, and search for **JTNA** in the library
3. Install the module and load your data

---

## Tutorials

| Tutorial | Link |
|---|---|
| Comprehensive TNA tutorial | [sonsoles.me/posts/tna-tutorial](https://sonsoles.me/posts/tna-tutorial/) |
| Group analysis and comparison | [sonsoles.me/posts/tna-group](https://sonsoles.me/posts/tna-group/) |
| Clustering and profile discovery | [sonsoles.me/posts/tna-clustering](https://sonsoles.me/posts/tna-clustering/) |
| Model comparison guide | [sonsoles.me/posts/tna-compare](https://sonsoles.me/posts/tna-compare/) |
| Full function reference | [sonsoles.me/tna/tna.html](https://sonsoles.me/tna/tna.html) |

## R Package Vignettes

| Vignette | Link |
|---|---|
| Getting started with tna | [tna.html](https://sonsoles.me/tna/articles/tna.html) |
| Main tna functions showcase | [complete\_tutorial.html](https://sonsoles.me/tna/articles/complete_tutorial.html) |
| Preparing data for tna | [prepare\_data.html](https://sonsoles.me/tna/articles/prepare_data.html) |
| Frequency-based TNA | [ftna.html](https://sonsoles.me/tna/articles/ftna.html) |
| Attention TNA | [atna.html](https://sonsoles.me/tna/articles/atna.html) |
| Cliques and communities | [communities\_and\_cliques.html](https://sonsoles.me/tna/articles/communities_and_cliques.html) |
| Grouped sequence data | [grouped\_sequences.html](https://sonsoles.me/tna/articles/grouped_sequences.html) |
