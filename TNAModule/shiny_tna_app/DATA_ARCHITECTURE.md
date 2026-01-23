# Centralized Data Loading Architecture

## Overview

The TNA Shiny App uses a centralized data loading architecture where data is loaded **once** in a dedicated Data tab and shared across all analysis modules. This eliminates code duplication and provides a consistent user experience.

## Data Formats

The app supports two data formats:

### 1. Activity Sequence (TNA Modules)
Long-format data with one row per action/event.

| Column | Required | Description |
|--------|----------|-------------|
| Action | Yes | The action/activity/event/state |
| Actor | Optional | Person/entity performing the action |
| Group | Optional | Group membership for group analysis |
| Time | Optional | Timestamp of the action |
| Order | Optional | Sequence order within actor |

**Example:**
```csv
actor,action,group,time
student1,Reading,GroupA,2024-01-01 10:00:00
student1,Writing,GroupA,2024-01-01 10:05:00
student2,Reading,GroupB,2024-01-01 10:00:00
```

### 2. Edge List (SNA Module)
Network data with source-target pairs.

| Column | Required | Description |
|--------|----------|-------------|
| From | Yes | Source node |
| To | Yes | Target node |
| Weight | Optional | Edge weight/strength |
| Group | Optional | Group for separate networks |

**Example:**
```csv
from,to,weight,group
NodeA,NodeB,5,Group1
NodeB,NodeC,3,Group1
NodeA,NodeC,2,Group2
```

---

## Architecture

### Shared Data Store

All data is stored in a single `reactiveValues` object:

```r
shared_data <- reactiveValues(
  # Raw data
  activity_data = NULL,   # For TNA modules
  edge_data = NULL,       # For SNA module

  # Activity sequence column mappings
  action_col = NULL,
  actor_col = NULL,
  group_col = NULL,
  time_col = NULL,
  order_col = NULL,

  # Edge list column mappings
  from_col = NULL,
  to_col = NULL,
  weight_col = NULL,
  sna_group_col = NULL,

  # Flags
  has_group = FALSE,
  data_format = "activity_sequence"
)
```

### Data Flow

```
┌─────────────────────────────────────────────────────────────┐
│                        DATA TAB                              │
│  ┌─────────────┐    ┌──────────────┐    ┌───────────────┐  │
│  │ File Upload │ OR │ Sample Data  │ -> │ Column Mapping │  │
│  └─────────────┘    └──────────────┘    └───────────────┘  │
│                              │                               │
│                              ▼                               │
│                     ┌─────────────────┐                     │
│                     │  shared_data    │                     │
│                     └─────────────────┘                     │
└─────────────────────────────────────────────────────────────┘
                               │
         ┌─────────────────────┼─────────────────────┐
         ▼                     ▼                     ▼
    ┌─────────┐          ┌──────────┐          ┌─────────┐
    │   TNA   │          │ Group TNA│          │   SNA   │
    │ Module  │          │  Module  │          │ Module  │
    └─────────┘          └──────────┘          └─────────┘
```

---

## Coding Conventions

### 1. Module Data Reactive

Each module has a simple data reactive that pulls from shared_data:

```r
# Pattern for TNA-based modules
module_data <- reactive({
  req(shared_data$activity_data)
  # Add any module-specific requirements
  # req(shared_data$actor_col)  # if actor required
  # req(shared_data$has_group)  # if group required
  shared_data$activity_data
})

# Pattern for SNA module
sna_data <- reactive({
  req(shared_data$edge_data)
  shared_data$edge_data
})
```

### 2. Using Shared Column Mappings

Instead of module-specific inputs like `input$tna_action`, use shared mappings:

```r
# Before (old pattern - DON'T USE)
copyData[[input$tna_action]] <- as.character(copyData[[input$tna_action]])

# After (new pattern - USE THIS)
copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])
```

### 3. Data Status Indicators

Each module sidebar should have a status indicator:

**UI:**
```r
sidebar = sidebar(
  title = "Module Name",
  width = 350,

  # Data status indicator (always first)
  uiOutput("module_data_indicator"),
  hr(),

  # Module-specific controls...
)
```

**Server:**
```r
output$module_data_indicator <- renderUI({
  if (is.null(shared_data$activity_data)) {
    div(class = "alert alert-warning alert-sm py-2",
        icon("exclamation-triangle"), " Load data in Data tab")
  } else if (is.null(shared_data$required_col)) {
    div(class = "alert alert-warning alert-sm py-2",
        icon("exclamation-circle"), " Select required column in Data tab")
  } else {
    div(class = "alert alert-success alert-sm py-2",
        icon("check-circle"), sprintf(" Data: %d rows", nrow(shared_data$activity_data)))
  }
})
```

### 4. Plot Messages

Show helpful messages in plot area when requirements aren't met:

```r
output$module_network_plot <- renderPlot({
  # Check requirements in order
  if (is.null(shared_data$activity_data)) {
    par(mar = c(0, 0, 0, 0))
    plot.new()
    text(0.5, 0.5, "Load data in the Data tab to begin", cex = 1.2, col = "gray50")
    return()
  }

  if (is.null(shared_data$required_col)) {
    par(mar = c(0, 0, 0, 0))
    plot.new()
    text(0.5, 0.5, "Select required column in the Data tab", cex = 1.2, col = "gray50")
    return()
  }

  # Actual plot code...
})
```

### 5. Button-Triggered Analysis

For computationally intensive operations, use button triggers with reactiveVal:

```r
# Store computed results
module_model <- reactiveVal(NULL)

# Button handler
observeEvent(input$module_run, {
  req(shared_data$action_col)

  # Compute model...
  result <- tryCatch({
    # analysis code
  }, error = function(e) {
    showNotification(paste("Error:", e$message), type = "error")
    NULL
  })

  if (!is.null(result)) {
    module_model(result)
    showNotification("Analysis complete", type = "message")
  }
})

# Use in outputs
output$module_plot <- renderPlot({
  req(module_model())
  # plot code using module_model()
})
```

### 6. Smart Column Matching

The app auto-detects columns using pattern matching:

```r
find_matching_column <- function(col_names, expected_names) {
  col_lower <- tolower(col_names)
  expected_lower <- tolower(expected_names)
  for (exp in expected_lower) {
    match_idx <- which(col_lower == exp)
    if (length(match_idx) > 0) {
      return(col_names[match_idx[1]])
    }
  }
  return("")
}

# Usage
action_match <- find_matching_column(col_names,
  c("action", "activity", "event", "state"))
```

### 7. Standardized Plot Settings

All network plots should use consistent default settings:

| Setting | Default | Input ID Pattern |
|---------|---------|------------------|
| Cut value | 0.1 | `*_plot_cut` |
| Minimum value | 0.05 | `*_plot_min` |
| Edge label size | 1 | `*_edge_label_size` |
| Node size | 1 | `*_node_size` |
| Node label size | 1 | `*_node_label_size` |
| Layout | "circle" | `*_layout` |

```r
plot(model,
     cut = input$module_plot_cut,
     minimum = input$module_plot_min,
     edge.label.cex = input$module_edge_label_size,
     node.width = input$module_node_size,
     label.cex = input$module_node_label_size,
     layout = input$module_layout,
     bg = "white")
```

---

## Adding a New Module

### Step 1: Add UI Tab

```r
nav_panel(
  title = "New Module",
  icon = icon("chart-network"),
  layout_sidebar(
    sidebar = sidebar(
      title = "New Module Analysis",
      width = 350,

      # 1. Data indicator (required)
      uiOutput("newmod_data_indicator"),
      hr(),

      # 2. Run button (if needed)
      actionButton("newmod_run", "Run Analysis", class = "btn-primary mb-3"),
      hr(),

      # 3. Module-specific options
      # ...

      # 4. Visualization settings in accordion
      accordion(
        id = "newmod_accordion",
        open = FALSE,
        accordion_panel(
          title = "Visualization Settings",
          numericInput("newmod_plot_cut", "Cut value:", value = 0.1, ...),
          # ... standard plot settings
        )
      )
    ),

    # Main panel
    navset_pill(
      nav_panel(title = "Network", ...)
    )
  )
)
```

### Step 2: Add Server Logic

```r
# ==========================================================================
# New Module
# ==========================================================================

# Data reactive
newmod_data <- reactive({
  req(shared_data$activity_data)
  shared_data$activity_data
})

# Data indicator
output$newmod_data_indicator <- renderUI({
  # ... status indicator code
})

# Model storage (if button-triggered)
newmod_model <- reactiveVal(NULL)

# Run handler (if button-triggered)
observeEvent(input$newmod_run, {
  # ... analysis code
})

# OR reactive model (if auto-updating)
newmod_model <- reactive({
  req(shared_data$action_col)
  # ... build model
})

# Outputs
output$newmod_network_plot <- renderPlot({
  # ... plot code with helpful messages
})
```

---

## Module Requirements

| Module | Requires Actor | Requires Group | Data Format |
|--------|---------------|----------------|-------------|
| TNA | Optional | No | Activity Sequence |
| Group TNA | Yes | Yes | Activity Sequence |
| Cluster TNA | Yes | No | Activity Sequence |
| Co-occurrence | Optional | No | Activity Sequence |
| One-Hot | Optional | Optional | Activity Sequence |
| SNA | N/A | Optional | Edge List |

---

## File Structure

```
shiny_tna_app/
├── app.R                 # Main application
├── DATA_ARCHITECTURE.md  # This file
├── data/
│   ├── Regulation_long.csv
│   ├── sample_edgelist.csv
│   ├── sample_onehot.csv
│   └── sample_sequences.csv
├── www/
│   └── styles.css
└── R/                    # Helper functions (if needed)
```
