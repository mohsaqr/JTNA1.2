---
title: "JTNA Tutorial: Transition Network Analysis in jamovi"
subtitle: "A point-and-click walkthrough of JTNA v1.7.1, grounded in the reference TNA tutorial."
---

**JTNA v1.7.1** — a step-by-step, point-and-click walkthrough of the JTNA jamovi module.

::: {.scope}
**Scope.** Every conceptual explanation in this tutorial is drawn from the reference R-package tutorial: Saqr, M., & López-Pernas, S. (2026). *An Updated Comprehensive Tutorial on Transition Network Analysis (TNA)* — [sonsoleslp.github.io/posts/tna-tutorial](https://sonsoleslp.github.io/posts/tna-tutorial/). JTNA is a jamovi wrapper for the `tna` R package (Tikka, López-Pernas, & Saqr, 2025), so the behaviour of each analysis option is the behaviour described in that tutorial. Where JTNA exposes features that fall outside that tutorial's scope (for example Group TNA comparisons or sequence clustering), we describe what each control does at the widget level and point you to the dedicated companion tutorials — [tna-group](https://sonsoles.me/posts/tna-group/) and [tna-clustering](https://sonsoles.me/posts/tna-clustering/) — rather than attempting to re-explain those methods here.
:::

::: {.tip}
**How this tutorial is laid out.** Each top-level section is collapsible — click its heading, or use the **Expand / Collapse all** pill at the bottom-right, to hide sections you don't need. The floating table of contents on the left keeps you oriented; clicking a TOC entry scrolls to and opens the target section automatically.
:::

---

## Table of Contents

1. [What is Transition Network Analysis?](#1-what-is-transition-network-analysis)
2. [Installation and Setup](#2-installation-and-setup)
3. [The example dataset](#3-the-example-dataset)
4. [Part 1 — TNA module](#4-part-1--tna-module)
5. [Part 2 — Group TNA](#5-part-2--group-tna)
6. [Part 3 — Cluster TNA](#6-part-3--cluster-tna)
7. [Complete workflow at a glance](#7-complete-workflow-at-a-glance)
8. [Troubleshooting and tips](#8-troubleshooting-and-tips)
9. [References and citation](#9-references-and-citation)

---

## 1. What is Transition Network Analysis?

Transition Network Analysis (TNA) is a framework for modelling sequential processes as weighted directed networks. It combines the temporal resolution of stochastic process mining with the structural analytic capacity of graph theory (Saqr, López-Pernas, Törmänen, et al., 2025). The framework supports several model types — first-order Markov models, frequency-based transition models, co-occurrence models, and attention-based models — each suited to different data characteristics and research questions.

What distinguishes TNA from earlier approaches to sequential and process data is that **every level of analysis is subject to confirmatory testing**:

- **Edge significance** is established through bootstrapping.
- **Centrality stability** is quantified through case-dropping with correlation stability coefficients.
- **Group differences** are evaluated through permutation testing that produces edge-level p-values, effect sizes, and corrections for multiple comparisons.

In the network, **nodes** are the unique states or actions, **arrows** are the transitions from one state to another (thicker arrows = higher transition probability), and the **coloured rim** around each node shows the initial probability of starting in that state.

The framework is implemented in the `tna` R package (Tikka, López-Pernas, & Saqr, 2025), with companion interfaces in jamovi (this module, JTNA) and a Shiny web application.

`[SCREENSHOT 1.1 — An annotated example transition network highlighting nodes, directed arrows, and the initial-probability rim.]`

---

## 2. Installation and Setup

### 2.1 Install jamovi

Download jamovi from [jamovi.org](https://www.jamovi.org/) and install it for your operating system.

`[SCREENSHOT 2.1 — The jamovi.org download page.]`

### 2.2 Install JTNA

**Option A — jamovi library (recommended):**

1. Open jamovi.
2. Click the **Modules** tab (the **+** icon, top-right).
3. Click **jamovi library**.
4. Search for **JTNA** and click **Install**.

**Option B — sideload the `.jmo` file:**

1. Download `JTNA_1.7.1.jmo` from the JTNA GitHub repository.
2. In jamovi, click **Modules → Sideload**.
3. Open the downloaded `.jmo` file.

`[SCREENSHOT 2.2 — The Modules panel after JTNA has been installed.]`

### 2.3 Load the example dataset

1. Click the hamburger menu (top-left).
2. **Open → Data Library**.
3. Under the JTNA section, pick **Regulation_long**.

`[SCREENSHOT 2.3 — jamovi's Data Library with the Regulation_long entry selected.]`

---

## 3. The example dataset

Throughout this tutorial we use the bundled **Regulation_long** dataset, a long-format event log of coded collaborative-regulation behaviours.

| Property | Value |
|---|---|
| Rows | 13,767 events |
| Actors | 2,000 individuals |
| Actions (9 states) | adapt, cohesion, consensus, coregulate, discuss, emotion, monitor, plan, synthesis |
| Groups (3) | A, B, C |
| Columns | `Actor`, `Achievers` (High/Low), `Group` (A/B/C), `Time_Posix`, `Action`, `Time_Unix` |

Each row is one event: one actor performed one action at one time. The data is already in **long format** — exactly the shape JTNA (and the underlying `prepare_data()` function) expects.

`[SCREENSHOT 3.1 — jamovi's spreadsheet view showing the six columns of Regulation_long.]`

---

## 4. Part 1 — TNA module

Open **Analyses → JTNA → TNA**.

The left-hand options panel is organised top-to-bottom into the following sections, which this tutorial walks through in order:

1. **Variables** (Action / Actor / Time / Order)
2. **Basic Options** (Type, Matrix, Scaling, Threshold)
3. `━━ Network Analysis ━━` — Visualization Settings, Centrality Analysis, Edge Betweenness, Community Detection, Clique Analysis
4. `━━ Confirmatory Testing ━━` — Bootstrap Analysis, Centrality Stability
5. `━━ Sequence Analysis ━━` — Sequence Analysis, Pattern Discovery, Sequence Indices

### 4.1 Assigning variables

The variable supplier at the top has four target slots.

| Slot | Column to drag in | Notes |
|---|---|---|
| **Action** | `Action` | Required. Each unique value becomes a node in the network. |
| **Actor** | `Actor` | Optional but strongly recommended — it tells JTNA to build one sequence per person. Without it the function treats the whole data frame as one long sequence, chaining actor A's last event directly into actor B's first event, which makes no substantive sense. |
| **Time** | `Time_Posix` | Optional. Within each actor, events are sorted by timestamp; also, two consecutive events from the same actor more than 15 minutes apart (the default) are treated as belonging to different sequences. |
| **Order** | *(leave empty)* | Optional. Use this *instead of* Time when you only have a step counter, not a timestamp. If both are supplied, Time sorts first and Order breaks ties. |

Any column in the data that is not assigned to one of these slots (for example `Achievers`, `Group`) is preserved as metadata and is available later as a grouping variable in Group TNA.

Drag **Action** → Action, **Actor** → Actor, **Time_Posix** → Time.

`[SCREENSHOT 4.1 — TNA options panel with Action/Actor/Time_Posix assigned.]`

As soon as Action is assigned, JTNA builds the model and starts producing output on the right.

### 4.2 Basic Options

#### Type (model type)

| Option | Description |
|---|---|
| **Relative** *(default)* | First-order Markov transition-probability model. Rows of the transition matrix sum to 1; each cell `[i, j]` is the probability of transitioning from state `i` to state `j`. This is the model type that the reference tutorial uses throughout. |
| **Frequency** | Frequency-based transition model (raw counts). Listed in the reference tutorial as one of the supported model types. |
| **Attention** | Attention-based model. Listed in the reference tutorial as a supported model type; the decay rate is controlled by the **Lambda** field (only enabled when Attention is selected). |

::: {.note}
The reference tutorial works end-to-end with the Relative (first-order Markov) model. For the other model types, consult the `tna` package documentation.
:::

#### Matrix *(checkbox)*

When ticked, JTNA shows the full transition matrix — the same object the reference tutorial inspects with `round(model$weights, 3)`. Each cell `[i, j]` is the probability of transitioning from state `i` to state `j`, and rows sum to 1.

`[SCREENSHOT 4.2 — The 9×9 transition matrix produced for Regulation_long.]`

#### Scaling

Four options: **No Scaling** *(default)*, MinMax, Max, Rank. The reference tutorial does not apply scaling when building the model, so we recommend leaving this at **No Scaling**. See the `tna` R-package documentation if you need to apply a non-default scaling.

#### Parameters → Threshold (default: 900)

This is the session-splitting threshold **in seconds** — the `time_threshold` argument of `prepare_data()` in the reference tutorial. Its default (900 = 15 minutes) means that if two consecutive events from the same actor are more than 15 minutes apart, JTNA treats them as belonging to different sequences. The reference tutorial explains why this matters:

> "a transition from the last event before a break to the first event after a break is not a real transition — there is a gap in between where the process stopped."

Adjust this to match your data: a chat corpus might use 2 minutes (120), a weekly longitudinal study might use one day (86400).

`[SCREENSHOT 4.3 — Basic Options section showing Type, Matrix, Scaling, and Threshold.]`

---

### 4.3 Visualization Settings

#### Display Options

| Checkbox | What it shows | Reference-tutorial equivalent |
|---|---|---|
| **TNA plot** *(on by default)* | The main transition-network plot. | `plot(model, minimum = ..., cut = ...)` |
| **Histogram** | Distribution of all transition probabilities. Useful for choosing a pruning threshold. | `hist(model)` |
| **Frequencies plot** | Bar chart of how often each state appears in the data (raw frequency, not transition probability). | `plot_frequencies(model)` |
| **Mosaic plot** | Mosaic-plot view of transition frequencies. *Not covered in the reference tutorial — use only if you specifically want a mosaic visualisation.* |

`[SCREENSHOT 4.4 — Transition network, histogram and frequencies plot for Regulation_long.]`

#### Plot Parameters

The two parameters the reference tutorial explains explicitly are **Cut value** and **Minimum value**. They map onto `plot(model, minimum = , cut = )` as follows:

| Parameter | Default | Meaning (from reference tutorial) |
|---|---|---|
| **Minimum value** | 0.05 | "Edges with weight below this value are **hidden entirely** but are retained in all analysis." |
| **Cut value** | 0.1 | "Edges with weight below this value appear **faded/thinner**." |
| **Edge label size** | 1 | Size of the numeric label on each arrow (cosmetic). |
| **Node size** | 8 | Size of each node (cosmetic). |
| **Node label size** | 1 | Size of the state-name labels (cosmetic). |
| **Layout** | Circle | Node-placement algorithm. 14 options are available; the reference tutorial uses the default. |
| **Digits** | 1 | Decimal places for mosaic-plot labels. |

::: {.important}
**Important (straight from the reference tutorial):** lowering the **Minimum value** does not change any analysis — it only affects what appears on the plot. All edges remain in the model and are used by every downstream statistic.
:::

---

### 4.4 Centrality Analysis

Centrality measures reduce each state to a single number capturing its structural importance in the network. Different measures answer different questions: which states are the most common destinations (InStrength), the most common origins (OutStrength), the key bridges between other states (Betweenness), or the most influential in spreading activation (Diffusion).

#### Display Options
- **Table** — one row per state, one column per selected measure.
- **Plot** — one panel per selected measure.

#### Measures (reference-tutorial definitions, verbatim)

| Measure | Interpretation |
|---|---|
| **OutStrength** | Total outgoing weight. Frequent *origin* state. |
| **InStrength** | Total incoming weight. "Attractor" in the process. |
| **Closeness** | Overall connectedness. |
| **ClosenessIn / ClosenessOut** | Reachability from/to other states. |
| **Betweenness** | Bridge/bottleneck between other states. |
| **BetweennessRSP** | Robust betweenness via randomized shortest paths. |
| **Clustering** | Whether neighbours are also connected to each other. |
| **Diffusion** | Spread of activation through the network. |

#### Parameters
- **Include loops** — include self-loops when computing centrality (reference tutorial: `loops = TRUE`).
- **Normalize values** — rescale measures (reference tutorial: `normalize = TRUE`).

`[SCREENSHOT 4.5 — Centrality table and multi-panel centrality plot.]`

---

### 4.5 Edge Betweenness

While node centrality summarises the importance of individual states, **edge betweenness** identifies the most critical *transitions* — the connections that serve as bridges between different parts of the process. Transitions with high edge betweenness lie on many shortest paths and therefore play a key structural role.

- **Table** — edge betweenness value for every transition.
- **Plot** — a network where edge thickness reflects betweenness.

This corresponds to the reference tutorial's `betweenness_network(model)` → `plot(..., cut = 0.1)`.

The Plot Parameters (Cut, Minimum value, Edge label size, Node size, Node label size, Layout) behave exactly as in Section 4.3.

`[SCREENSHOT 4.6 — Edge-betweenness table and network plot.]`

---

### 4.6 Community Detection

Communities are groups of states that are **more densely connected to each other than to the rest of the network**. Unlike cliques (which require every pair to be mutually connected), community-detection algorithms use modularity-based criteria to reveal the modular structure and functional subsystems within the transition network.

In the reference tutorial, `communities(model)` runs seven algorithms and prints how many communities each one detected, together with the assignment of each state to a community under each algorithm.

#### Methods (reference-tutorial definitions)

| Method | Logic |
|---|---|
| **leading_eigen** | Leading eigenvector of the modularity matrix. |
| **walktrap** | Random walks; nodes visited together belong together. |
| **fast_greedy** | Greedy modularity optimisation. |
| **spinglass** | Spin-glass model for fine-grained structure. |
| **infomap** | Description-length minimisation for directed flow. |
| **label_prop**, **edge_betweenness** | Additional algorithms supported by the underlying `tna` package. |

::: {.note}
**JTNA default:** Spinglass. The reference R-tutorial uses **leading_eigen** by default. Either is fine — compare the assignments across several methods rather than relying on a single algorithm.
:::

#### Parameters
- **Gamma** — resolution parameter used by Spinglass (higher → more, smaller communities).

`[SCREENSHOT 4.7 — Community assignment table and coloured community plot.]`

---

### 4.7 Clique Analysis

A clique is a **fully connected subnetwork** where every state has strong mutual transitions to every other state in the group. From the reference tutorial:

> "Cliques reveal self-reinforcing behavioural loops — for example, a 'plan → monitor → adapt → plan' cycle where each state frequently transitions to each of the others."

And, on how to interpret them:

> "These are substantively interesting because they represent behavioural patterns that sustain themselves: once a learner enters a clique, the transition probabilities keep them cycling within it."

#### Parameters

| Parameter | Meaning (reference tutorial) |
|---|---|
| **Size** | Number of states in each clique (2 = dyads, 3 = triads, 4 = quads, …). |
| **Threshold** | Minimum transition probability that every edge in the clique must meet. |

**Practical guidance from the reference tutorial:** *lower the threshold as clique size increases*, because fully mutual connections become increasingly unlikely. The reference tutorial uses `threshold = 0.1` for dyads, `0.05` for triads, `0.03` for quads.

#### Display
- **Plot** — a panel of subnetwork plots, one per detected clique.

`[SCREENSHOT 4.8 — Dyad, triad and quad clique plots.]`

---

### 4.8 Bootstrap Analysis (Confirmatory Testing)

Quoting the reference tutorial directly:

> "The transition probabilities estimated by `tna()` are descriptive estimates derived from a particular sample of sequences. Without further validation, there is no basis for determining which of these estimates reflect stable properties of the underlying process and which are artifacts of sampling variability."
>
> "Bootstrapping addresses this directly by resampling sequences with replacement and reconstructing the transition matrix across a large number of iterations … Edges that do not consistently exceed a defined threshold or that deviate beyond an acceptable consistency range are identified as non-significant."
>
> "The bootstrap is therefore not an optional diagnostic — it is a confirmatory step that determines whether the network structure warrants substantive interpretation."

#### Parameters (match the reference tutorial's defaults)

| JTNA field | Default | Reference-tutorial argument |
|---|---|---|
| **Iteration** | 1000 | `iter = 1000` |
| **Level** | 0.05 | `level = 0.05` |
| **Method → Stability** *(default)* | — | `method = "stability"` |
| **Method → Threshold** | — | `method = "threshold"` |
| **Lower / Upper** (under Method Parameters, Stability) | 0.75 / 1.25 | `consistency_range = c(0.75, 1.25)` |
| **Threshold** (under Method Parameters, Threshold method) | 0.1 | edge-weight threshold |

#### Display
- **Plot** — network plot showing only edges that survived the bootstrap.
- **Table** — bootstrap summary, with one row per edge and the columns below.

#### Bootstrap-summary columns (reference tutorial)

| Column | Meaning |
|---|---|
| `from`, `to` | Source and target state. |
| `weight` | Original observed transition probability. |
| `p_value` | Proportion of inconsistent resamples. |
| `sig` | Whether the edge is significant at the chosen level. |
| `cr_lower`, `cr_upper` | Consistency-range bounds. |
| `ci_lower`, `ci_upper` | Bootstrap confidence-interval bounds. |

#### Interpreting the results (reference tutorial)

- **Significant edges (`sig = TRUE`)** — appeared consistently across bootstrap resamples. These transitions are stable properties of the data and warrant substantive interpretation.
- **Non-significant edges (`sig = FALSE`)** — unstable under resampling. Their presence in the original model may reflect sampling noise rather than a genuine transition pathway.
- **Narrow confidence intervals** mean the edge weight is precisely estimated; **wide confidence intervals** mean substantial uncertainty remains even if the edge reached significance.
- **Practical implication:** *report and interpret only the bootstrap-validated network. Conclusions drawn from non-validated edges risk being non-replicable.*

#### Table Options

- **Max rows** (default 20) / **Show all rows** — paging controls.
- **Significant only** — filter the table to `sig = TRUE` edges.

`[SCREENSHOT 4.9 — Bootstrap summary table (first 20 rows) and the bootstrap-validated network plot.]`

---

### 4.9 Centrality Stability (Confirmatory Testing)

The reference tutorial is blunt here:

> "Centrality measures are among the most frequently interpreted yet least frequently validated quantities in network-based analyses."

The **case-dropping bootstrap** systematically removes increasing proportions of sequences, recomputes centrality at each step, and quantifies how much the original ranking is preserved — via the **Correlation Stability (CS) coefficient** (Epskamp, Borsboom, & Fried, 2018). The CS coefficient is the maximum proportion of cases that can be dropped while maintaining a correlation of at least 0.7 with the original centrality ordering.

| CS coefficient | Interpretation |
|---|---|
| **≥ 0.7** | Excellent — centrality rankings are robust and can be interpreted with confidence. |
| **0.5 – 0.7** | Acceptable — rankings are reasonably stable but should be interpreted with some caution. |
| **< 0.5** | Poor — rankings are unreliable and should not serve as the basis for substantive conclusions. |

> "A CS coefficient below 0.5 indicates that the centrality ordering is not sufficiently stable … the appropriate response is to collect additional data or to refrain from interpreting centrality rankings altogether."

#### Display
- **Stability Table** — CS-coefficient for each selected measure.
- **Stability Plot** — the average correlation between full-sample and reduced-sample centrality rankings as the drop proportion increases.

#### Measures
InStrength, OutStrength, Betweenness (the three on by default).

#### Parameters
- **Iterations** (default 100).
- **Threshold** (default 0.7) — the correlation target used to compute the CS coefficient.
- **Certainty** (default 0.95) — confidence level for the CS estimate.

`[SCREENSHOT 4.10 — Centrality-stability table and stability plot.]`

---

### 4.10 Sequence Analysis

The sections above all work with the transition network, where each edge represents how often one state follows another across all sequences. **Sequence analysis works with the raw sequences directly, before they get collapsed into a transition matrix.** It provides a complementary perspective: the full temporal unfolding of individual sequences and the overall distribution of states across sequence positions.

This corresponds to the reference tutorial's `plot_sequences(prepared_data)`.

#### Plot Type
- **Index** *(default)* — each row is one individual sequence, colours represent states at each position. Reveals patterns in sequence length, composition, and ordering that the transition matrix alone cannot capture.
- **Distribution** — aggregates across all sequences, showing at each position the proportion (or count) of sequences in each state.

#### Scale
- **Proportion** *(default)* / **Count** — y-axis metric for the distribution plot.

#### Geometry
- **Bar** *(default)* / **Area** — stacked bars or stacked area chart.

#### Options
- **Include missing values** (default off in TNA, on in Group TNA / Cluster TNA).
- **Tick Interval** (default 5) — spacing of x-axis tick marks.

`[SCREENSHOT 4.11 — Index and distribution sequence plots.]`

---

### 4.11 Pattern Discovery *(beyond the scope of the reference tutorial)*

This section lets JTNA mine your sequences for recurring sub-sequences (n-grams, gapped patterns, repeated patterns) using the `codyna` package. **The reference TNA tutorial does not cover pattern discovery**, so we describe only what each control does at the widget level.

| Control | Effect |
|---|---|
| **Show patterns table** | Toggles the table on. |
| **Pattern Type** | N-gram (consecutive), Gapped (with gaps), Repeated (repeated within an individual). |
| **Min Length / Max Length** | Length range for N-gram and Repeated patterns. |
| **Min Gap / Max Gap** | Gap range, only used when Pattern Type = Gapped. |
| **Min Support / Min Count** | Minimum proportion of actors, and minimum absolute count, required for a pattern to be reported. |
| **Starts with / Ends with / Contains** | String filters applied to the pattern. |
| **Max rows / Show all rows** | Table paging. |

Refer to the `codyna` package documentation for the substantive interpretation of these measures.

`[SCREENSHOT 4.12 — Pattern Discovery controls and an example patterns table.]`

---

### 4.12 Sequence Indices *(beyond the scope of the reference tutorial)*

Computes per-actor summary statistics about each individual's sequence (for example entropy and diversity measures). **The reference TNA tutorial does not cover these indices**, so again we describe only the widget.

| Control | Effect |
|---|---|
| **Show indices table** | Toggles the table on. |
| **Favorable State** | Pick one action from the dropdown to compute indices related to time spent in that state. |
| **Omega** (default 1) | Weighting parameter for the favorable-state indices. |
| **Max rows / Show all rows** | Table paging. |

Refer to the package documentation for formulas and interpretation.

`[SCREENSHOT 4.13 — Sequence Indices table.]`

---

## 5. Part 2 — Group TNA

Open **Analyses → JTNA → Group TNA**.

Group TNA builds a **separate transition network for every level of a grouping variable** and adds comparison and permutation-testing tools. In the reference R-tutorial, group comparisons are covered in a dedicated companion tutorial ([tna-group](https://sonsoles.me/posts/tna-group/)). This section walks through the JTNA UI and grounds each shared feature in the reference tutorial; for the group-comparison-specific tools it describes the widgets only and points you to the companion tutorial for the conceptual treatment.

### 5.1 Assigning variables

Group TNA adds one new slot on top of the four TNA slots:

| Slot | Column |
|---|---|
| Action | `Action` |
| Actor | `Actor` |
| Time | `Time_Posix` |
| Order | *(leave empty)* |
| **Group** | `Group` |

Drag `Group` into the Group slot. JTNA will detect levels A, B, C and build one network per level.

`[SCREENSHOT 5.1 — Group TNA options panel with Group assigned.]`

### 5.2 Per-group outputs

The sections that follow are *identical in meaning* to the TNA module — they simply produce one result per group.

- **Visualization Settings** — Section 4.3 applies, per group.
- **Centrality Analysis** — Section 4.4 applies, per group.
- **Community Detection** — Section 4.6 applies, per group.
- **Clique Analysis** — Section 4.7 applies, per group.
- **Bootstrap Analysis** — Section 4.8 applies, per group. Each group's network is validated independently.
- **Sequence Analysis / Sequence Indices** — Sections 4.10 and 4.12 apply, per group.

::: {.note}
**Absent from Group TNA.** Edge Betweenness, Centrality Stability and Pattern Discovery are exposed only in the TNA module — they do not appear in Group TNA's panel.
:::

`[SCREENSHOT 5.2 — Three per-group TNA plots, one each for Group A, B and C.]`

### 5.3 Permutation Analysis *(group-comparison feature)*

Permutation testing assesses whether observed differences in transition probabilities between two groups exceed what would be expected under a random reassignment of group labels. This feature is described in detail in the **companion tutorial** ([tna-group](https://sonsoles.me/posts/tna-group/)) — we describe only the widget here.

| JTNA field | Default | Notes |
|---|---|---|
| **Iter** | 1000 | Number of random permutations. |
| **Level** | 0.05 | Significance threshold. |
| **Paired** | off | Tick when groups are naturally paired (e.g. pre/post on the same actors). |
| **Text / Plot** | off | Output toggles. |
| **Max rows / Show all rows** | 20 / off | Table paging. |

`[SCREENSHOT 5.3 — Permutation-test table with edge-level p-values.]`

### 5.4 Network Difference Plot *(group-comparison feature)*

Produces a single network plot colouring each edge by which group has the stronger transition. See the companion tutorial for interpretation. The Plot Parameters (Cut, Min value, Edge label size, Node size, Node label size, Layout) behave as described in Section 4.3.

`[SCREENSHOT 5.4 — Network difference plot comparing two groups.]`

### 5.5 Compare Network Properties *(group-comparison feature)*

Widget-level description only — consult the companion tutorial for methodology.

- **First Network / Second Network** — pick two group levels from the dropdowns.
- **Summary Metrics Table** — side-by-side global metrics for the two networks.
- **Network Properties Table** — edge-level comparison (weights, difference, correlation, distance).
- **Comparison Plot** + **Plot Type** — Heatmap *(default)*, Scatterplot, Weight Density.
- **Scaling Method** — None *(default)*, MinMax, Max, Rank, Z-Score, Robust, Log, Log1p, Softmax, Quantile. Controls how edge weights are scaled before the two networks are compared.

`[SCREENSHOT 5.5 — Summary metrics table, network-properties table and a heatmap comparison plot.]`

### 5.6 Compare Sequences *(group-comparison feature)*

Tests whether specific short action sub-sequences differ in frequency between groups. Widget-level description only.

| Control | Default | Notes |
|---|---|---|
| Sub Min / Sub Max | 2 / 4 | Range of sub-sequence lengths to test. |
| Minimum Frequency | 20 | Sub-sequences below this total count are ignored. |
| Correction Method | Bonferroni | Also available: Holm, Hochberg, BH (FDR), BY, None. |
| Max rows / Show all rows | 20 / off | Table paging. |

`[SCREENSHOT 5.6 — Compare-Sequences table with adjusted p-values.]`

---

## 6. Part 3 — Cluster TNA

Open **Analyses → JTNA → Cluster TNA**.

Cluster TNA is for situations where you **do not have a pre-defined grouping variable**. It clusters actors based on the similarity of their sequences, then builds a separate transition network for each discovered cluster. The underlying clustering methodology is covered in a separate companion tutorial ([tna-clustering](https://sonsoles.me/posts/tna-clustering/)) — as with Group TNA, this section walks through the JTNA UI and grounds shared features in the reference tutorial, while describing clustering-specific controls only at the widget level.

### 6.1 Assigning variables

The four standard slots (Action, Actor, Time, Order) — **no Group slot**, because clusters are discovered from the data.

`[SCREENSHOT 6.1 — Cluster TNA options panel.]`

### 6.2 Clustering Parameters

The first section in the Cluster TNA panel is **Clustering Parameters** (expanded by default). This is where Cluster TNA differs from the other two modules.

| Control | Default | Notes |
|---|---|---|
| **Distance Measure** | Hamming | Also: Optimal String Alignment (OSA), Levenshtein, Longest Common Substring (LCS), Jaccard, Jaro-Winkler. |
| **Clustering Method** | PAM | Also: Ward (D2), Complete, Average, Single. |
| **Number of Clusters (k)** | 2 | Integer, 2–20. |
| **Run Clustering** | off | **Must be ticked for clustering to run.** |

Clustering can be computationally expensive on large datasets, which is why the Run Clustering checkbox has to be ticked explicitly. See the clustering companion tutorial for guidance on picking a distance measure and *k*.

`[SCREENSHOT 6.2 — Clustering Parameters section with Run Clustering ticked.]`

### 6.3 Per-cluster outputs

Once clustering completes, everything below is produced per-cluster. These sections behave exactly as in the TNA module:

- **Visualization Settings** — Section 4.3.
- **Centrality Analysis** — Section 4.4.
- **Community Detection** — Section 4.6.
- **Clique Analysis** — Section 4.7.
- **Bootstrap Analysis** — Section 4.8.
- **Sequence Analysis / Sequence Indices** — Sections 4.10 and 4.12.

Cluster TNA exposes one additional **model type** not present in TNA: **Co-occurrence**. This is one of the model types mentioned in the reference tutorial's introduction (alongside first-order Markov, frequency and attention), suited to data where order is not the primary concern.

`[SCREENSHOT 6.3 — Two per-cluster network plots for k = 2.]`

### 6.4 Cluster comparisons

The comparison tools are the same as in Group TNA, with two UI differences:

- **First Network / Second Network** are integer inputs (cluster 1, 2, …), not dropdowns.
- **Scaling Method** offers a reduced list: None, MinMax, Max, Rank, Z-Score, Robust.

Everything else — **Permutation Analysis**, **Network Difference Plot**, **Compare Network Properties**, **Compare Sequences** — mirrors Sections 5.3–5.6. See the clustering companion tutorial for interpretation.

`[SCREENSHOT 6.4 — Cluster comparison: heatmap, network difference plot, permutation table.]`

---

## 7. Complete workflow at a glance

JTNA exposes the same end-to-end analysis pipeline that the reference R-tutorial describes:

| Step | Reference-tutorial R call | JTNA equivalent |
|---|---|---|
| Prepare data | `prepare_data(df, action=, actor=, time=, time_threshold=900)` | Variable slots (Action / Actor / Time / Order) + Parameters → Threshold (900). |
| Build model | `tna(prepared)` | Happens automatically once Action is assigned. Pick a Type. |
| Visualise | `plot(model, minimum=0.05, cut=0.1)` | Visualization Settings → TNA plot (Minimum value = 0.05, Cut value = 0.1). |
| Inspect distributions | `hist(model)`; `plot_frequencies(model)` | Visualization Settings → Histogram; Frequencies plot. |
| Cliques | `cliques(model, size=k, threshold=t)` | Clique Analysis (Size, Threshold). |
| Node centrality | `centralities(model)` | Centrality Analysis. |
| Edge centrality | `betweenness_network(model)` | Edge Betweenness. |
| Communities | `communities(model, methods=…)` | Community Detection (Methods). |
| Bootstrap | `bootstrap(model, iter=1000, level=0.05)` | Bootstrap Analysis (Iteration, Level, Method, Range). |
| Centrality stability | `estimate_centrality_stability(model)` | Centrality Stability. |
| Raw sequences | `plot_sequences(prepared)` | Sequence Analysis. |

For group-level and cluster-level comparisons (Permutation Analysis, Compare Network Properties, Compare Sequences, Network Difference Plot), consult the companion tutorials [tna-group](https://sonsoles.me/posts/tna-group/) and [tna-clustering](https://sonsoles.me/posts/tna-clustering/) — they ground those features in the same way this tutorial grounds the TNA module.

---

## 8. Troubleshooting and tips

### Where JTNA defaults differ from the reference tutorial

| Setting | JTNA default | Reference-tutorial default | Notes |
|---|---|---|---|
| Community-detection method | Spinglass | leading_eigen | Either is reasonable; the reference tutorial suggests running several methods and comparing. |
| `hist` / `plot_frequencies` visible by default | off | n/a (called on demand) | Tick the checkboxes under Visualization Settings when you need them. |

### General

- **No output appears.** Ensure the **Action** variable is assigned and that the data has at least two unique action values.
- **Greyed-out options.** Many controls are only enabled when a parent toggle is on (e.g. Plot Parameters require the relevant Plot / Table checkbox). Lambda is only enabled when Type = Attention. Gap fields are only enabled when Pattern Type = Gapped. Mosaic plot requires Type = Frequency.
- **Bootstrap is slow.** Reduce **Iteration** from 1000 to 100 while exploring, then raise it again for the analysis you report.
- **Pick a sensible Minimum value.** Tick **Histogram** first to see the distribution of edge weights, then pick a minimum that removes visual noise without discarding edges you actually care about. Remember: *Minimum value only affects the plot — every analytical result uses the full set of edges.*
- **Clustering is slow.** Large datasets with many actors take longer. Try reducing *k* or picking a faster distance measure (Hamming or Jaccard).

---

## 9. References and citation

### To cite JTNA

> Girault, D., Saqr, M., Tikka, S., & López-Pernas, S. (2025). *JTNA: Transition Network Analysis for jamovi* (Version 1.7.1) [Software]. <https://github.com/sonsoleslp/JTNA>

### To cite the TNA methodology and R package

- Saqr, M., López-Pernas, S., Törmänen, T., Kaliisa, R., Misiejuk, K., & Tikka, S. (2025). Transition Network Analysis: A Novel Framework for Modeling, Visualizing, and Identifying the Temporal Patterns of Learners and Learning Processes. In *Proceedings of the 15th International Learning Analytics and Knowledge Conference (LAK '25)* (pp. 351–361). ACM. <https://doi.org/10.1145/3706468.3706513>
- Tikka, S., López-Pernas, S., & Saqr, M. (2025). *tna: An R Package for Transition Network Analysis*. Applied Psychological Measurement. <https://doi.org/10.1177/01466216251348840>
- Saqr, M., López-Pernas, S., & Tikka, S. (2025). Mapping Relational Dynamics with Transition Network Analysis: A Primer and Tutorial. In Saqr & López-Pernas (Eds.), *Advanced Learning Analytics Methods*. Springer. <https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html>
- Saqr, M., López-Pernas, S., & Tikka, S. (2025). Capturing The Breadth and Dynamics of the Temporal Processes with Frequency Transition Network Analysis: A Primer and Tutorial. In Saqr & López-Pernas (Eds.), *Advanced Learning Analytics Methods*. Springer. <https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html>
- López-Pernas, S., Tikka, S., & Saqr, M. (2025). Mining Patterns and Clusters with Transition Network Analysis: A Heterogeneity Approach. In Saqr & López-Pernas (Eds.), *Advanced Learning Analytics Methods*. Springer. <https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html>

### Companion tutorials (used by this document)

- Saqr, M., & López-Pernas, S. (2026). *An Updated Comprehensive Tutorial on Transition Network Analysis (TNA).* <https://sonsoleslp.github.io/posts/tna-tutorial/>
- TNA Group Analysis: <https://sonsoles.me/posts/tna-group/>
- TNA Clustering: <https://sonsoles.me/posts/tna-clustering/>
- `prepare_data()` tutorial: <https://sonsoles.me/posts/tna-data/>

---

*Tutorial written for JTNA v1.7.1. If you are using a different version some options or defaults may differ.*
