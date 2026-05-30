# What's New in JTNA v1.6.1

JTNA (jamovi Transition Network Analysis) is a plugin for jamovi that lets researchers analyze sequential behavioral data as networks — visualizing how people move between states over time. Version 1.4.0 included two modules: TNA for individual-level analysis, and Group TNA for comparing pre-defined groups. Version 1.6.1 adds three substantial new capabilities.

---

## 1. Cluster TNA: Groups Discovered from the Data

The new Cluster TNA module removes the requirement to have a pre-existing group variable. Instead of bringing labeled groups to the analysis, you let the sequences define the groups.

The module computes pairwise dissimilarities between participants' sequences, clusters participants based on those dissimilarities, then builds a separate transition network for each cluster. The result is a set of networks representing behavioral profiles discovered directly from the data.

### Six Dissimilarity Algorithms

You can choose how sequence similarity is measured:

| Algorithm | Description |
|---|---|
| **Hamming** | Position-wise differences between fixed-length sequences |
| **Optimal String Alignment (OSA)** | Edit distance with restricted transpositions |
| **Levenshtein (LV)** | Insertions, deletions, and substitutions |
| **Longest Common Subsequence (LCS)** | Shared ordering without requiring contiguity |
| **Jaccard** | Set-based overlap, ignoring order |
| **Jaro-Winkler (JW)** | Edit distance weighted toward early positions |

Clustering method options are: PAM, Ward's D2, complete linkage, average linkage, and single linkage.

---

## 2. Sequence Pattern Mining

Pattern Discovery, powered by the `codyna` package, identifies subsequences that recur across participants — complementing the transition probabilities shown in the network with concrete behavioral sequences and their frequencies.

Three pattern types are supported:

- **N-grams** — consecutive subsequences of a given length
- **Gapped patterns** — non-consecutive subsequences, allowing gaps between elements
- **Repeated patterns** — subsequences that appear multiple times within one person's trace

Parameters include minimum and maximum pattern length, gap range, minimum support threshold, and filters by starting state, ending state, or containing state. Output is a table of patterns with count, proportion, and support.

---

## 3. Centrality Stability Analysis

Centrality measures identify the most prominent nodes in a network, but their reliability depends on the sample. Centrality Stability (CS) analysis tests that reliability by repeatedly dropping random subsets of participants, recomputing centrality, and measuring how much the rankings change.

The output is a CS-coefficient per measure, ranging from 0 to 1. Values above 0.7 indicate stable rankings; values below 0.5 suggest the rankings are sensitive to sample composition. A stability plot accompanies the coefficient table.

This analysis is available in TNA, Group TNA, and Cluster TNA, with configurable iterations, threshold, and certainty level.

---

## 4. Sequence Indices

Sequence Indices compute per-person summary statistics that describe the structure of each person's behavioral sequence:

- **Longitudinal entropy** — unpredictability across the sequence
- **Simpson diversity** — spread across distinct states
- **Mean spell duration** — average time spent in each state
- **Self-loop tendency** — rate of consecutive same-state repetition
- **Transition rate** — frequency of state changes
- **Complexity index** — composite measure of behavioral variety

The output is a participant-level table usable in downstream analyses. Available in all three modules.

---

## 5. The Attention Model Type

A new model type builds transition networks using temporal weighting: recent transitions contribute more than distant ones, controlled by a decay parameter **λ**. Larger λ values increase the weight of recent behavior. The `co-occurrence` model type was removed from TNA and Group TNA.

---

## 6. Fourteen Network Layout Algorithms

Layout options expanded from 2 (circle, spring) to 14, including Kamada-Kawai, Fruchterman-Reingold, GEM, Sugiyama hierarchical, DRL, and others. Different layouts can surface different structural properties of the same network.

---

## Other Changes

- Group TNA adds **Compare Sequences** — statistical comparison of subsequence frequencies across groups with multiple correction options
- Community detection now outputs a table alongside the plot
- Bootstrap results tables support pagination and filtering to significant edges only
- A sample dataset (`Regulation_long.csv`) is included
- References updated to four proper academic citations (JTNA, TNA R package, TNA LAK paper, CODYNA)

---

JTNA is free and installable from the jamovi library. Tutorials and documentation: [lamethods.org](https://lamethods.org). Source code: [github.com/mohsaqr/JTNA1.2](https://github.com/mohsaqr/JTNA1.2)

*Cite: Girault et al. (2025); Saqr et al. (2025 LAK); Tikka et al. (Applied Psychological Measurement, 2025).*
