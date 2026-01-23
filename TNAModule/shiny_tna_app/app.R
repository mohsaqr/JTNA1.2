# TNA Shiny Application
# Converted from JTNA Jamovi Module
# Version 1.0.0

library(shiny)
library(bslib)
library(bsicons)
library(DT)
library(tna)
library(codyna)

# ============================================================================
# HELPER FUNCTIONS
# ============================================================================

# Layout choices for network plots
layout_choices <- c(
  "Circle" = "circle",
  "Spring" = "spring",
  "Kamada-Kawai" = "layout_with_kk",
  "Graphopt" = "layout_with_graphopt",
  "GEM" = "layout_with_gem",
  "DRL" = "layout_with_drl",
  "Star" = "layout_as_star",
  "Grid" = "layout_on_grid",
  "Nicely" = "layout_nicely",
  "Fruchterman-Reingold" = "layout_with_fr",
  "Davidson-Harel" = "layout_with_dh",
  "LGL" = "layout_with_lgl",
  "Sugiyama" = "layout_with_sugiyama",
  "Random" = "layout_randomly"
)

# Color themes for network plots
theme_choices <- c(
  "Colorblind" = "colorblind",
  "Classic" = "classic",
  "Gray" = "gray",
  "Hollywood" = "Hollywood",
  "Borkulo" = "Borkulo",
  "TeamFortress" = "TeamFortress",
  "Reddit" = "Reddit",
  "Leuven" = "Leuven",
  "Fried" = "Fried"
)

# Community detection methods
community_methods <- c(
  "Spinglass" = "spinglass",
  "Walktrap" = "walktrap",
  "Fast Greedy" = "fast_greedy",
  "Label Propagation" = "label_prop",
  "Infomap" = "infomap",
  "Edge Betweenness" = "edge_betweenness",
  "Leading Eigenvector" = "leading_eigen"
)

# Centrality measures
centrality_measures <- c(
  "OutStrength", "InStrength", "Betweenness", "BetweennessRSP",
  "Closeness", "ClosenessIn", "ClosenessOut", "Diffusion", "Clustering"
)

# ============================================================================
# UI
# ============================================================================

# Available themes
available_themes <- c(
  "Flatly" = "flatly",
  "Cosmo" = "cosmo",
  "Cerulean" = "cerulean",
  "Journal" = "journal",
  "Litera" = "litera",
  "Lumen" = "lumen",
  "Minty" = "minty",
  "Pulse" = "pulse",
  "Sandstone" = "sandstone",
  "Simplex" = "simplex",
  "Sketchy" = "sketchy",
  "Spacelab" = "spacelab",
  "United" = "united",
  "Yeti" = "yeti",
  "Zephyr" = "zephyr",
  "Cyborg (Dark)" = "cyborg",
  "Darkly (Dark)" = "darkly",
  "Slate (Dark)" = "slate",
  "Solar (Dark)" = "solar",
  "Superhero (Dark)" = "superhero",
  "Vapor (Dark)" = "vapor"
)

ui <- page_navbar(
  title = "TNA - Transition Network Analysis",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  # ---- Settings in navbar ----
  nav_spacer(),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  ),
  nav_item(
    selectInput(
      "app_theme",
      label = NULL,
      choices = available_themes,
      selected = "flatly",
      width = "140px"
    )
  ),

  # ---- Data Tab ----
  nav_panel(
    title = "Data",
    icon = icon("database"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Data Management",
        width = 350,

        # Data Management Accordion
        accordion(
          id = "data_management_accordion",
          open = c("load_data", "column_mapping"),

          accordion_panel(
            title = "Data Format",
            icon = bsicons::bs_icon("file-earmark-spreadsheet"),
            value = "data_format",
            radioButtons("data_format", label = NULL,
                         choices = c("Activity Sequence (TNA)" = "activity_sequence",
                                    "Edge List (SNA)" = "edge_list"),
                         selected = "activity_sequence")
          ),

          accordion_panel(
            title = "Load Data",
            icon = bsicons::bs_icon("cloud-arrow-up"),
            value = "load_data",
            fileInput("data_file", label = NULL, accept = ".csv",
                      placeholder = "Choose CSV file...", buttonLabel = "Browse"),
            actionButton("load_sample", "Load Sample Data",
                        class = "btn-outline-secondary w-100",
                        icon = icon("database"))
          ),

          accordion_panel(
            title = "Column Mapping",
            icon = bsicons::bs_icon("columns-gap"),
            value = "column_mapping",

            conditionalPanel(
              condition = "input.data_format == 'activity_sequence'",
              # Required field
              tags$label(
                class = "form-label",
                bsicons::bs_icon("asterisk", class = "text-danger"), " Action Column"
              ),
              selectInput("col_action", label = NULL, choices = NULL),

              # Optional fields
              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Actor Column ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_actor", label = NULL, choices = NULL),

              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Group Column ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_group", label = NULL, choices = NULL),

              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Time Column ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_time", label = NULL, choices = NULL),

              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Order Column ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_order", label = NULL, choices = NULL)
            ),

            conditionalPanel(
              condition = "input.data_format == 'edge_list'",
              # Required fields
              tags$label(
                class = "form-label",
                bsicons::bs_icon("asterisk", class = "text-danger"), " From (Source)"
              ),
              selectInput("col_from", label = NULL, choices = NULL),

              tags$label(
                class = "form-label",
                bsicons::bs_icon("asterisk", class = "text-danger"), " To (Target)"
              ),
              selectInput("col_to", label = NULL, choices = NULL),

              # Optional fields
              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Weight ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_weight", label = NULL, choices = NULL),

              tags$label(
                class = "form-label",
                bsicons::bs_icon("dash-circle", class = "text-secondary"), " Group ",
                tags$small(class = "text-muted", "(Optional)")
              ),
              selectInput("col_sna_group", label = NULL, choices = NULL)
            )
          )
        ),

        # Status indicator
        tags$div(class = "mt-3", uiOutput("data_status"))
      ),

      # Main panel - Conditional rendering
      uiOutput("data_main_panel")
    )
  ),

  # ---- TNA Tab ----
  nav_panel(
    title = "TNA",
    icon = icon("sitemap"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Transition Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("tna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("tna_run", "Run Analysis", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("tna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        checkboxInput("tna_show_plot", "Show TNA Plot", value = TRUE),
        checkboxInput("tna_show_matrix", "Show Matrix", value = TRUE),

        # Collapsible sections
        accordion(
          id = "tna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            radioButtons("tna_type", "Type:",
                         choices = c("Relative" = "relative",
                                    "Frequency" = "frequency",
                                    "Attention" = "attention"),
                         selected = "relative", inline = TRUE),
            conditionalPanel(
              condition = "input.tna_type == 'attention'",
              numericInput("tna_lambda", "Lambda (decay rate):", value = 1, min = 0.01, max = 10)
            ),
            radioButtons("tna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling"),
            numericInput("tna_threshold", "Time Threshold:", value = 900, min = 1)
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("tna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("tna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("tna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("tna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("tna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("tna_layout", "Layout:", choices = layout_choices),
            checkboxInput("tna_show_histogram", "Show Histogram", value = TRUE),
            checkboxInput("tna_show_frequencies", "Show Frequencies Plot", value = TRUE),
            conditionalPanel(
              condition = "input.tna_type == 'frequency'",
              checkboxInput("tna_show_mosaic", "Show Mosaic Plot", value = TRUE)
            )
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("tna_centrality_show_table", "Show Table", value = TRUE),
            checkboxInput("tna_centrality_show_plot", "Show Plot", value = TRUE),
            checkboxGroupInput("tna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("tna_centrality_loops", "Include loops", value = FALSE),
            checkboxInput("tna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Centrality Stability", icon = bsicons::bs_icon("bar-chart"),
            checkboxInput("tna_stability_show_table", "Stability Table", value = TRUE),
            checkboxInput("tna_stability_show_plot", "Stability Plot", value = TRUE),
            checkboxGroupInput("tna_stability_measures", "Measures:",
                              choices = c("InStrength", "OutStrength", "Betweenness"),
                              selected = c("InStrength", "OutStrength", "Betweenness")),
            numericInput("tna_stability_iterations", "Iterations:", value = 100, min = 10, max = 10000),
            numericInput("tna_stability_threshold", "Threshold:", value = 0.7, min = 0, max = 1, step = 0.05),
            numericInput("tna_stability_certainty", "Certainty:", value = 0.95, min = 0, max = 1, step = 0.01)
          ),

          accordion_panel(
            title = "Edge Betweenness", icon = bsicons::bs_icon("arrow-right"),
            checkboxInput("tna_edge_betweenness_show_table", "Show Table", value = TRUE),
            checkboxInput("tna_edge_betweenness_show_plot", "Show Plot", value = TRUE),
            selectInput("tna_edge_betweenness_layout", "Layout:", choices = layout_choices)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("tna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("tna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("tna_community_method", "Method:", choices = community_methods),
            numericInput("tna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("tna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("tna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("tna_bootstrap_iterations", "Iterations:", value = 1000, min = 0, max = 10000),
            numericInput("tna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01),
            radioButtons("tna_bootstrap_method", "Method:",
                        choices = c("Stability" = "stability", "Threshold" = "threshold"),
                        selected = "stability"),
            conditionalPanel(
              condition = "input.tna_bootstrap_method == 'stability'",
              numericInput("tna_bootstrap_range_low", "Lower range:", value = 0.75, min = 0, max = 10),
              numericInput("tna_bootstrap_range_up", "Upper range:", value = 1.25, min = 0, max = 10)
            ),
            conditionalPanel(
              condition = "input.tna_bootstrap_method == 'threshold'",
              numericInput("tna_bootstrap_threshold", "Threshold:", value = 0.1, min = 0, max = 1)
            ),
            checkboxInput("tna_bootstrap_significant_only", "Significant only", value = FALSE),
            numericInput("tna_bootstrap_max_rows", "Max rows:", value = 20, min = 1, max = 1000)
          ),

          accordion_panel(
            title = "Sequence Analysis", icon = bsicons::bs_icon("list-ol"),
            checkboxInput("tna_sequences_show_plot", "Show Sequence Plot", value = TRUE),
            radioButtons("tna_sequences_type", "Type:",
                        choices = c("Index" = "index", "Distribution" = "distribution"),
                        selected = "index", inline = TRUE),
            radioButtons("tna_sequences_scale", "Scale:",
                        choices = c("Proportion" = "proportion", "Count" = "count"),
                        selected = "proportion", inline = TRUE),
            radioButtons("tna_sequences_geom", "Geometry:",
                        choices = c("Bar" = "bar", "Area" = "area"),
                        selected = "bar", inline = TRUE),
            checkboxInput("tna_sequences_include_na", "Include missing values", value = FALSE),
            numericInput("tna_sequences_tick", "Tick interval:", value = 5, min = 1, max = 20)
          ),

          accordion_panel(
            title = "Pattern Discovery", icon = bsicons::bs_icon("search"),
            checkboxInput("tna_pattern_show_table", "Show Patterns Table", value = TRUE),
            radioButtons("tna_pattern_type", "Pattern Type:",
                        choices = c("N-gram" = "ngram", "Gapped" = "gapped",
                                   "Repeated" = "repeated", "Custom" = "custom"),
                        selected = "ngram"),
            conditionalPanel(
              condition = "input.tna_pattern_type == 'custom'",
              textInput("tna_pattern_custom", "Custom Pattern:", value = "")
            ),
            numericInput("tna_pattern_len_min", "Min Length:", value = 2, min = 2, max = 20),
            numericInput("tna_pattern_len_max", "Max Length:", value = 5, min = 2, max = 20),
            numericInput("tna_pattern_min_support", "Min Support:", value = 0.01, min = 0, max = 1),
            numericInput("tna_pattern_min_count", "Min Count:", value = 2, min = 1, max = 1000),
            numericInput("tna_pattern_max_rows", "Max rows:", value = 20, min = 1, max = 1000)
          ),

          accordion_panel(
            title = "Sequence Indices", icon = bsicons::bs_icon("hash"),
            checkboxInput("tna_indices_show_table", "Show Indices Table", value = TRUE),
            numericInput("tna_indices_omega", "Omega:", value = 1, min = 0.01, max = 10),
            numericInput("tna_indices_max_rows", "Max rows:", value = 50, min = 1, max = 10000)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "tna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("sitemap"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("TNA Network"),
              tags$span(
                downloadButton("tna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("tna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("tna_network_plot", width = "600px", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "tna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("tna_summary_table")
                ),
                accordion_panel(
                  title = "Transition Matrix",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("tna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Metrics",
          icon = icon("chart-bar"),
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Histogram"),
                tags$span(
                  downloadButton("tna_histogram_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("tna_histogram_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("tna_histogram_plot", width = "600px", height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Frequencies"),
                tags$span(
                  downloadButton("tna_frequencies_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("tna_frequencies_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("tna_frequencies_plot", width = "600px", height = "400px")
              )
            )
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Mosaic"),
              tags$span(
                downloadButton("tna_mosaic_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("tna_mosaic_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("tna_mosaic_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_menu(
          title = "Centrality",
          icon = icon("bullseye"),

          nav_panel(
            title = "Measures",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Centrality Plot"),
                tags$span(
                  downloadButton("tna_centrality_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("tna_centrality_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("tna_centrality_plot", width = "600px", height = "400px")
              )
            ),
            card(
              card_header("Centrality Table"),
              DT::dataTableOutput("tna_centrality_table")
            )
          ),

          nav_panel(
            title = "Stability",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Centrality Stability Plot"),
                tags$span(
                  downloadButton("tna_stability_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("tna_stability_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("tna_stability_plot", width = "600px", height = "400px")
              )
            ),
            card(
              card_header("Centrality Stability Table"),
              DT::dataTableOutput("tna_stability_table")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Plot"),
              tags$span(
                downloadButton("tna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("tna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("tna_community_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Community Table"),
            DT::dataTableOutput("tna_community_table")
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Plot"),
              tags$span(
                downloadButton("tna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("tna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("tna_bootstrap_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Bootstrap Table"),
            DT::dataTableOutput("tna_bootstrap_table")
          )
        ),

        nav_menu(
          title = "Sequences",
          icon = icon("stream"),

          nav_panel(
            title = "Sequence Plot",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Sequence Plot"),
                tags$span(
                  downloadButton("tna_sequences_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("tna_sequences_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("tna_sequences_plot", width = "600px", height = "400px")
              )
            )
          ),

          nav_panel(
            title = "Sequence Indices",
            card(
              card_header("Sequence Indices"),
              DT::dataTableOutput("tna_indices_table")
            )
          ),

          nav_panel(
            title = "Pattern Discovery",
            card(
              card_header("Pattern Discovery"),
              DT::dataTableOutput("tna_pattern_table")
            )
          )
        ),

        nav_panel(
          title = "Edge Betweenness",
          icon = icon("route"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Edge Betweenness Plot"),
              tags$span(
                downloadButton("tna_edgebetweenness_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("tna_edgebetweenness_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("tna_edge_betweenness_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Edge Betweenness Table"),
            DT::dataTableOutput("tna_edge_betweenness_table")
          )
        )
      )
    )
  ),

  # ---- Group TNA Tab ----
  nav_panel(
    title = "Group TNA",
    icon = icon("users"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Group Transition Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("gtna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("gtna_run", "Run Analysis", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("gtna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        checkboxInput("gtna_show_plot", "Show Plot", value = TRUE),
        checkboxInput("gtna_show_matrix", "Show Matrix", value = TRUE),

        accordion(
          id = "gtna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            radioButtons("gtna_type", "Type:",
                         choices = c("Relative" = "relative",
                                    "Frequency" = "frequency",
                                    "Attention" = "attention"),
                         selected = "relative", inline = TRUE),
            conditionalPanel(
              condition = "input.gtna_type == 'attention'",
              numericInput("gtna_lambda", "Lambda:", value = 1, min = 0.01, max = 10)
            ),
            radioButtons("gtna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling"),
            numericInput("gtna_threshold", "Time Threshold:", value = 900, min = 1)
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("gtna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("gtna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("gtna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("gtna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("gtna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("gtna_layout", "Layout:", choices = layout_choices)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("gtna_centrality_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_centrality_show_plot", "Show Plot", value = TRUE),
            checkboxGroupInput("gtna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("gtna_centrality_loops", "Include loops", value = FALSE),
            checkboxInput("gtna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Permutation Test", icon = bsicons::bs_icon("arrow-left-right"),
            checkboxInput("gtna_permutation_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_permutation_show_plot", "Show Plot", value = TRUE),
            numericInput("gtna_permutation_iter", "Iterations:", value = 1000, min = 1),
            checkboxInput("gtna_permutation_paired", "Paired", value = FALSE),
            numericInput("gtna_permutation_level", "Level:", value = 0.05, min = 0, max = 1)
          ),

          accordion_panel(
            title = "Sequence Analysis", icon = bsicons::bs_icon("list-ol"),
            checkboxInput("gtna_sequences_show_plot", "Show Sequence Plot", value = TRUE),
            radioButtons("gtna_sequences_type", "Type:",
                        choices = c("Index" = "index", "Distribution" = "distribution"),
                        selected = "index", inline = TRUE),
            radioButtons("gtna_sequences_scale", "Scale:",
                        choices = c("Proportion" = "proportion", "Count" = "count"),
                        selected = "proportion", inline = TRUE),
            radioButtons("gtna_sequences_geom", "Geometry:",
                        choices = c("Bar" = "bar", "Area" = "area"),
                        selected = "bar", inline = TRUE),
            checkboxInput("gtna_sequences_include_na", "Include missing values", value = FALSE),
            numericInput("gtna_sequences_tick", "Tick interval:", value = 5, min = 1, max = 20)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("gtna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("gtna_community_method", "Method:", choices = community_methods),
            numericInput("gtna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Clique Analysis", icon = bsicons::bs_icon("diagram-2"),
            checkboxInput("gtna_cliques_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_cliques_show_plot", "Show Plot", value = TRUE),
            numericInput("gtna_cliques_size", "Clique size:", value = 2, min = 2, max = 10),
            numericInput("gtna_cliques_threshold", "Threshold:", value = 0, min = 0, max = 1, step = 0.1)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("gtna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("gtna_bootstrap_iterations", "Iterations:", value = 1000, min = 100, max = 10000),
            numericInput("gtna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01),
            radioButtons("gtna_bootstrap_method", "Method:",
                        choices = c("Stability" = "stability", "Threshold" = "threshold"),
                        selected = "stability"),
            conditionalPanel(
              condition = "input.gtna_bootstrap_method == 'stability'",
              numericInput("gtna_bootstrap_range_low", "Lower range:", value = 0.75, min = 0, max = 10),
              numericInput("gtna_bootstrap_range_up", "Upper range:", value = 1.25, min = 0, max = 10)
            ),
            conditionalPanel(
              condition = "input.gtna_bootstrap_method == 'threshold'",
              numericInput("gtna_bootstrap_threshold", "Threshold:", value = 0.1, min = 0, max = 1)
            ),
            checkboxInput("gtna_bootstrap_significant_only", "Significant only", value = FALSE),
            numericInput("gtna_bootstrap_max_rows", "Max rows:", value = 20, min = 1, max = 1000)
          ),

          accordion_panel(
            title = "Compare Sequences", icon = bsicons::bs_icon("file-diff"),
            checkboxInput("gtna_compare_show_table", "Show Table", value = TRUE),
            checkboxInput("gtna_compare_show_plot", "Show Plot", value = TRUE),
            numericInput("gtna_compare_min_len", "Min Length:", value = 2, min = 2, max = 10),
            numericInput("gtna_compare_max_len", "Max Length:", value = 4, min = 2, max = 10),
            numericInput("gtna_compare_min_freq", "Min Frequency:", value = 20, min = 1, max = 100),
            selectInput("gtna_compare_correction", "Correction Method:",
                       choices = c("Bonferroni" = "bonferroni", "Holm" = "holm",
                                  "Hochberg" = "hochberg", "BH (FDR)" = "BH",
                                  "BY" = "BY", "None" = "none"))
          ),

          accordion_panel(
            title = "Sequence Indices", icon = bsicons::bs_icon("hash"),
            checkboxInput("gtna_indices_show_table", "Show Indices Table", value = TRUE),
            numericInput("gtna_indices_omega", "Omega:", value = 1, min = 0.01, max = 10),
            numericInput("gtna_indices_max_rows", "Max rows:", value = 50, min = 1, max = 10000)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "gtna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("project-diagram"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Group TNA Networks"),
              tags$span(
                downloadButton("gtna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_network_plot", width = "100%", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "gtna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("gtna_summary_table")
                ),
                accordion_panel(
                  title = "Transition Matrices",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("gtna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Table"),
            DT::dataTableOutput("gtna_centrality_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Centrality Plot"),
              tags$span(
                downloadButton("gtna_centrality_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_centrality_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_centrality_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Permutation",
          icon = icon("random"),
          card(
            card_header("Permutation Test Results"),
            DT::dataTableOutput("gtna_permutation_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Permutation Plot"),
              tags$span(
                downloadButton("gtna_permutation_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_permutation_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_permutation_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Sequences",
          icon = icon("stream"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Sequence Plot"),
              tags$span(
                downloadButton("gtna_sequences_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_sequences_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_sequences_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Detection Plot"),
              tags$span(
                downloadButton("gtna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_community_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Community Assignments"),
            DT::dataTableOutput("gtna_community_table")
          )
        ),

        nav_panel(
          title = "Cliques",
          icon = icon("circle-nodes"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Clique Analysis Plot"),
              tags$span(
                downloadButton("gtna_cliques_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_cliques_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_cliques_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Cliques"),
            verbatimTextOutput("gtna_cliques_text")
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Analysis Plot"),
              tags$span(
                downloadButton("gtna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_bootstrap_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Bootstrap Results"),
            DT::dataTableOutput("gtna_bootstrap_table")
          )
        ),

        nav_panel(
          title = "Compare",
          icon = icon("code-compare"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Compare Sequences Plot"),
              tags$span(
                downloadButton("gtna_compare_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gtna_compare_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gtna_compare_plot", width = "600px", height = "400px")
            )
          ),
          card(
            card_header("Sequence Comparison"),
            DT::dataTableOutput("gtna_compare_table")
          )
        ),

        nav_panel(
          title = "Indices",
          icon = icon("table"),
          card(
            card_header("Sequence Indices"),
            DT::dataTableOutput("gtna_indices_table")
          )
        )
      )
    )
  ),

  # ---- Cluster TNA Tab ----
  nav_panel(
    title = "Cluster TNA",
    icon = icon("object-group"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Cluster Transition Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("ctna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("ctna_run_clustering", "Run Clustering", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("ctna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        # Clustering Parameters
        numericInput("ctna_k", "Number of Clusters (k):", value = 2, min = 2, max = 20),

        selectInput("ctna_dissimilarity", "Distance Measure:",
                   choices = c("Hamming" = "hamming",
                              "Optimal String Alignment (OSA)" = "osa",
                              "Levenshtein" = "lv",
                              "Longest Common Substring (LCS)" = "lcs",
                              "Jaccard" = "jaccard",
                              "Jaro-Winkler" = "jw")),

        selectInput("ctna_method", "Clustering Method:",
                   choices = c("PAM" = "pam",
                              "Ward" = "ward.D2",
                              "Complete" = "complete",
                              "Average" = "average",
                              "Single" = "single")),

        checkboxInput("ctna_show_plot", "Show Cluster Plots", value = TRUE),

        accordion(
          id = "ctna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            radioButtons("ctna_type", "Type:",
                         choices = c("Relative" = "relative",
                                    "Frequency" = "frequency",
                                    "Attention" = "attention"),
                         selected = "relative", inline = TRUE),
            radioButtons("ctna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling")
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("ctna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("ctna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("ctna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("ctna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("ctna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("ctna_layout", "Layout:", choices = layout_choices)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("ctna_centrality_show_table", "Show Table", value = FALSE),
            checkboxGroupInput("ctna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness"))
          ),

          accordion_panel(
            title = "Permutation Test", icon = bsicons::bs_icon("arrow-left-right"),
            checkboxInput("ctna_permutation_show_table", "Show Table", value = FALSE),
            checkboxInput("ctna_permutation_show_plot", "Show Plot", value = FALSE),
            numericInput("ctna_permutation_iter", "Iterations:", value = 1000, min = 1),
            numericInput("ctna_permutation_level", "Level:", value = 0.05, min = 0, max = 1)
          ),

          accordion_panel(
            title = "Sequence Analysis", icon = bsicons::bs_icon("list-ol"),
            checkboxInput("ctna_sequences_show_plot", "Show Sequence Plot", value = TRUE),
            radioButtons("ctna_sequences_type", "Type:",
                        choices = c("Index" = "index", "Distribution" = "distribution"),
                        selected = "index", inline = TRUE),
            radioButtons("ctna_sequences_scale", "Scale:",
                        choices = c("Proportion" = "proportion", "Count" = "count"),
                        selected = "proportion", inline = TRUE),
            radioButtons("ctna_sequences_geom", "Geometry:",
                        choices = c("Bar" = "bar", "Area" = "area"),
                        selected = "bar", inline = TRUE),
            checkboxInput("ctna_sequences_include_na", "Include missing values", value = FALSE),
            numericInput("ctna_sequences_tick", "Tick interval:", value = 5, min = 1, max = 20)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("ctna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("ctna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("ctna_community_method", "Method:", choices = community_methods),
            numericInput("ctna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Clique Analysis", icon = bsicons::bs_icon("diagram-2"),
            checkboxInput("ctna_cliques_show_table", "Show Table", value = TRUE),
            checkboxInput("ctna_cliques_show_plot", "Show Plot", value = TRUE),
            numericInput("ctna_cliques_size", "Clique size:", value = 2, min = 2, max = 10),
            numericInput("ctna_cliques_threshold", "Threshold:", value = 0, min = 0, max = 1, step = 0.1)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("ctna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("ctna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("ctna_bootstrap_iterations", "Iterations:", value = 1000, min = 100, max = 10000),
            numericInput("ctna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01),
            radioButtons("ctna_bootstrap_method", "Method:",
                        choices = c("Stability" = "stability", "Threshold" = "threshold"),
                        selected = "stability"),
            conditionalPanel(
              condition = "input.ctna_bootstrap_method == 'stability'",
              numericInput("ctna_bootstrap_range_low", "Lower range:", value = 0.75, min = 0, max = 10),
              numericInput("ctna_bootstrap_range_up", "Upper range:", value = 1.25, min = 0, max = 10)
            ),
            conditionalPanel(
              condition = "input.ctna_bootstrap_method == 'threshold'",
              numericInput("ctna_bootstrap_threshold", "Threshold:", value = 0.1, min = 0, max = 1)
            ),
            checkboxInput("ctna_bootstrap_significant_only", "Significant only", value = FALSE),
            numericInput("ctna_bootstrap_max_rows", "Max rows:", value = 20, min = 1, max = 1000)
          ),

          accordion_panel(
            title = "Compare Sequences", icon = bsicons::bs_icon("file-diff"),
            checkboxInput("ctna_compare_show_table", "Show Table", value = TRUE),
            checkboxInput("ctna_compare_show_plot", "Show Plot", value = TRUE),
            numericInput("ctna_compare_min_len", "Min Length:", value = 2, min = 2, max = 10),
            numericInput("ctna_compare_max_len", "Max Length:", value = 4, min = 2, max = 10),
            numericInput("ctna_compare_min_freq", "Min Frequency:", value = 20, min = 1, max = 100),
            selectInput("ctna_compare_correction", "Correction Method:",
                       choices = c("Bonferroni" = "bonferroni", "Holm" = "holm",
                                  "Hochberg" = "hochberg", "BH (FDR)" = "BH",
                                  "BY" = "BY", "None" = "none"))
          ),

          accordion_panel(
            title = "Sequence Indices", icon = bsicons::bs_icon("hash"),
            checkboxInput("ctna_indices_show_table", "Show Indices Table", value = TRUE),
            numericInput("ctna_indices_omega", "Omega:", value = 1, min = 0.01, max = 10),
            numericInput("ctna_indices_max_rows", "Max rows:", value = 50, min = 1, max = 10000)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "ctna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("project-diagram"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Cluster TNA Networks"),
              tags$span(
                downloadButton("ctna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_network_plot", width = "100%", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "ctna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("ctna_summary_table")
                ),
                accordion_panel(
                  title = "Cluster Matrices",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("ctna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Clustering",
          icon = icon("object-group"),
          card(
            card_header("Cluster Assignments"),
            DT::dataTableOutput("ctna_cluster_assignment")
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Table"),
            DT::dataTableOutput("ctna_centrality_table")
          )
        ),

        nav_panel(
          title = "Permutation",
          icon = icon("random"),
          card(
            card_header("Permutation Test Results"),
            DT::dataTableOutput("ctna_permutation_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Permutation Plot"),
              tags$span(
                downloadButton("ctna_permutation_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_permutation_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_permutation_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Sequences",
          icon = icon("stream"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Sequence Plot"),
              tags$span(
                downloadButton("ctna_sequences_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_sequences_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_sequences_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Detection Plot"),
              tags$span(
                downloadButton("ctna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_community_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Community Assignments"),
            DT::dataTableOutput("ctna_community_table")
          )
        ),

        nav_panel(
          title = "Cliques",
          icon = icon("circle-nodes"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Clique Analysis Plot"),
              tags$span(
                downloadButton("ctna_cliques_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_cliques_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_cliques_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Cliques"),
            verbatimTextOutput("ctna_cliques_text")
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Analysis Plot"),
              tags$span(
                downloadButton("ctna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_bootstrap_plot", width = "600px", height = "600px")
            )
          ),
          card(
            card_header("Bootstrap Results"),
            DT::dataTableOutput("ctna_bootstrap_table")
          )
        ),

        nav_panel(
          title = "Compare",
          icon = icon("code-compare"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Compare Sequences Plot"),
              tags$span(
                downloadButton("ctna_compare_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ctna_compare_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ctna_compare_plot", width = "600px", height = "400px")
            )
          ),
          card(
            card_header("Sequence Comparison"),
            DT::dataTableOutput("ctna_compare_table")
          )
        ),

        nav_panel(
          title = "Indices",
          icon = icon("table"),
          card(
            card_header("Sequence Indices"),
            DT::dataTableOutput("ctna_indices_table")
          )
        )
      )
    )
  ),

  # ---- Co-occurrence TNA Tab ----
  nav_panel(
    title = "Co-occurrence",
    icon = icon("link"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Co-occurrence Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("cotna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("cotna_run", "Run Analysis", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("cotna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        checkboxInput("cotna_show_plot", "Show Plot", value = TRUE),
        checkboxInput("cotna_show_matrix", "Show Matrix", value = TRUE),

        accordion(
          id = "cotna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            radioButtons("cotna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling"),
            numericInput("cotna_threshold", "Time Threshold:", value = 900, min = 1)
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("cotna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("cotna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("cotna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("cotna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("cotna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("cotna_layout", "Layout:", choices = layout_choices),
            checkboxInput("cotna_show_histogram", "Show Histogram", value = TRUE),
            checkboxInput("cotna_show_frequencies", "Show Frequencies Plot", value = TRUE)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("cotna_centrality_show_table", "Show Table", value = TRUE),
            checkboxInput("cotna_centrality_show_plot", "Show Plot", value = TRUE),
            checkboxGroupInput("cotna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("cotna_centrality_loops", "Include loops", value = FALSE),
            checkboxInput("cotna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("cotna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("cotna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("cotna_community_method", "Method:", choices = community_methods),
            numericInput("cotna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("cotna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("cotna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("cotna_bootstrap_iterations", "Iterations:", value = 1000, min = 0, max = 10000),
            numericInput("cotna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "cotna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("link"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Co-occurrence Network"),
              tags$span(
                downloadButton("cotna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("cotna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("cotna_network_plot", width = "600px", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "cotna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("cotna_summary_table")
                ),
                accordion_panel(
                  title = "Co-occurrence Matrix",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("cotna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Metrics",
          icon = icon("chart-bar"),
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Histogram"),
                tags$span(
                  downloadButton("cotna_histogram_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("cotna_histogram_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("cotna_histogram_plot", width = "600px", height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Frequencies"),
                tags$span(
                  downloadButton("cotna_frequencies_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("cotna_frequencies_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("cotna_frequencies_plot", width = "600px", height = "400px")
              )
            )
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Table"),
            DT::dataTableOutput("cotna_centrality_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Centrality Plot"),
              tags$span(
                downloadButton("cotna_centrality_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("cotna_centrality_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("cotna_centrality_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            card_header("Community Table"),
            DT::dataTableOutput("cotna_community_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Plot"),
              tags$span(
                downloadButton("cotna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("cotna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("cotna_community_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            card_header("Bootstrap Table"),
            DT::dataTableOutput("cotna_bootstrap_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Plot"),
              tags$span(
                downloadButton("cotna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("cotna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("cotna_bootstrap_plot", width = "600px", height = "600px")
            )
          )
        )
      )
    )
  ),

  # ---- Group Co-occurrence TNA Tab ----
  nav_panel(
    title = "Group Co-occurrence",
    icon = icon("layer-group"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Group Co-occurrence Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("gcotna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("gcotna_run", "Run Analysis", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("gcotna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        checkboxInput("gcotna_show_plot", "Show Plot", value = TRUE),
        checkboxInput("gcotna_show_matrix", "Show Matrix", value = FALSE),

        accordion(
          id = "gcotna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            radioButtons("gcotna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling"),
            numericInput("gcotna_threshold", "Time Threshold:", value = 900, min = 1)
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("gcotna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("gcotna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("gcotna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("gcotna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("gcotna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("gcotna_layout", "Layout:", choices = layout_choices),
            checkboxInput("gcotna_show_histogram", "Show Histogram", value = FALSE),
            checkboxInput("gcotna_show_frequencies", "Show Frequencies Plot", value = FALSE)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("gcotna_centrality_show_table", "Show Table", value = TRUE),
            checkboxInput("gcotna_centrality_show_plot", "Show Plot", value = TRUE),
            checkboxGroupInput("gcotna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("gcotna_centrality_loops", "Include loops", value = FALSE),
            checkboxInput("gcotna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("gcotna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("gcotna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("gcotna_community_method", "Method:", choices = community_methods),
            numericInput("gcotna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Clique Analysis", icon = bsicons::bs_icon("diagram-2"),
            checkboxInput("gcotna_cliques_show_table", "Show Table", value = TRUE),
            checkboxInput("gcotna_cliques_show_plot", "Show Plot", value = TRUE),
            numericInput("gcotna_cliques_size", "Clique size:", value = 2, min = 2, max = 10),
            numericInput("gcotna_cliques_threshold", "Threshold:", value = 0, min = 0, max = 1, step = 0.1)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("gcotna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("gcotna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("gcotna_bootstrap_iterations", "Iterations:", value = 1000, min = 100, max = 10000),
            numericInput("gcotna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01),
            radioButtons("gcotna_bootstrap_method", "Method:",
                        choices = c("Stability" = "stability", "Threshold" = "threshold"),
                        selected = "stability"),
            conditionalPanel(
              condition = "input.gcotna_bootstrap_method == 'stability'",
              numericInput("gcotna_bootstrap_range_low", "Lower range:", value = 0.75, min = 0, max = 10),
              numericInput("gcotna_bootstrap_range_up", "Upper range:", value = 1.25, min = 0, max = 10)
            ),
            conditionalPanel(
              condition = "input.gcotna_bootstrap_method == 'threshold'",
              numericInput("gcotna_bootstrap_threshold", "Threshold:", value = 0.1, min = 0, max = 1)
            ),
            checkboxInput("gcotna_bootstrap_significant_only", "Significant only", value = FALSE),
            numericInput("gcotna_bootstrap_max_rows", "Max rows:", value = 20, min = 1, max = 1000)
          ),

          accordion_panel(
            title = "Permutation Test", icon = bsicons::bs_icon("arrow-left-right"),
            checkboxInput("gcotna_permutation_show_table", "Show Table", value = TRUE),
            checkboxInput("gcotna_permutation_show_plot", "Show Plot", value = TRUE),
            numericInput("gcotna_permutation_iter", "Iterations:", value = 1000, min = 1),
            checkboxInput("gcotna_permutation_paired", "Paired", value = FALSE),
            numericInput("gcotna_permutation_level", "Level:", value = 0.05, min = 0, max = 1)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "gcotna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("layer-group"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Group Co-occurrence Networks"),
              tags$span(
                downloadButton("gcotna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_network_plot", width = "100%", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "gcotna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("gcotna_summary_table")
                ),
                accordion_panel(
                  title = "Co-occurrence Matrices",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("gcotna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Metrics",
          icon = icon("chart-bar"),
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Histogram"),
                tags$span(
                  downloadButton("gcotna_histogram_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("gcotna_histogram_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("gcotna_histogram_plot", width = "600px", height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Frequencies"),
                tags$span(
                  downloadButton("gcotna_frequencies_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("gcotna_frequencies_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("gcotna_frequencies_plot", width = "600px", height = "400px")
              )
            )
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Table"),
            DT::dataTableOutput("gcotna_centrality_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Centrality Plot"),
              tags$span(
                downloadButton("gcotna_centrality_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_centrality_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_centrality_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            card_header("Community Assignments"),
            DT::dataTableOutput("gcotna_community_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Plot"),
              tags$span(
                downloadButton("gcotna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_community_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Cliques",
          icon = icon("circle-nodes"),
          card(
            card_header("Cliques"),
            verbatimTextOutput("gcotna_cliques_text")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Clique Plot"),
              tags$span(
                downloadButton("gcotna_cliques_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_cliques_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_cliques_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            card_header("Bootstrap Results"),
            DT::dataTableOutput("gcotna_bootstrap_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Plot"),
              tags$span(
                downloadButton("gcotna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_bootstrap_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Permutation",
          icon = icon("shuffle"),
          card(
            card_header("Permutation Test Results"),
            DT::dataTableOutput("gcotna_permutation_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Permutation Plot"),
              tags$span(
                downloadButton("gcotna_permutation_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("gcotna_permutation_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("gcotna_permutation_plot", width = "600px", height = "400px")
            )
          )
        )
      )
    )
  ),

  # ---- One-Hot TNA Tab ----
  nav_panel(
    title = "One-Hot",
    icon = icon("th"),
    layout_sidebar(
      sidebar = sidebar(
        title = "One-Hot Co-occurrence Network",
        width = 350,

        # Data status indicator
        uiOutput("ohtna_data_indicator"),
        hr(),

        # Run/Clear buttons
        div(
          class = "d-flex gap-2 mb-3",
          actionButton("ohtna_run", "Run Analysis", class = "btn-primary flex-grow-1", icon = icon("play")),
          actionButton("ohtna_clear", "Clear", class = "btn-outline-secondary", icon = icon("trash"))
        ),

        # Variable Selection (multiple for one-hot columns)
        selectizeInput("ohtna_onehot_cols", "One-Hot Columns (Actions):",
                      choices = NULL, multiple = TRUE,
                      options = list(placeholder = "Select binary columns")),
        selectInput("ohtna_actor", "Actor (Optional):", choices = NULL),
        selectInput("ohtna_session", "Session (Optional):", choices = NULL),
        selectInput("ohtna_group", "Group (Optional):", choices = NULL),

        checkboxInput("ohtna_show_plot", "Show Plot", value = TRUE),
        checkboxInput("ohtna_show_matrix", "Show Matrix", value = TRUE),

        accordion(
          id = "ohtna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Estimation Settings", icon = bsicons::bs_icon("sliders"),
            numericInput("ohtna_window", "Window Size:", value = 1, min = 1, max = 100),
            radioButtons("ohtna_scaling", "Scaling:",
                         choices = c("No scaling" = "noScaling",
                                    "MinMax" = "minmax",
                                    "Max" = "max",
                                    "Rank" = "rank"),
                         selected = "noScaling")
          ),

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("ohtna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("ohtna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("ohtna_edge_label_size", "Edge label size:", value = 1, min = 0, max = 10),
            numericInput("ohtna_node_size", "Node size:", value = 1, min = 0, max = 2, step = 0.1),
            numericInput("ohtna_node_label_size", "Node label size:", value = 1, min = 0, max = 10),
            selectInput("ohtna_layout", "Layout:", choices = layout_choices),
            checkboxInput("ohtna_show_histogram", "Show Histogram", value = TRUE),
            checkboxInput("ohtna_show_frequencies", "Show Frequencies Plot", value = TRUE),
            checkboxInput("ohtna_show_mosaic", "Show Mosaic Plot", value = TRUE)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("ohtna_centrality_show_table", "Show Table", value = TRUE),
            checkboxInput("ohtna_centrality_show_plot", "Show Plot", value = TRUE),
            checkboxGroupInput("ohtna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("ohtna_centrality_loops", "Include loops", value = FALSE),
            checkboxInput("ohtna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("ohtna_community_show_table", "Show Table", value = TRUE),
            checkboxInput("ohtna_community_show_plot", "Show Plot", value = TRUE),
            selectInput("ohtna_community_method", "Method:", choices = community_methods),
            numericInput("ohtna_community_gamma", "Gamma:", value = 1, min = 0, max = 100)
          ),

          accordion_panel(
            title = "Bootstrap Analysis", icon = bsicons::bs_icon("shuffle"),
            checkboxInput("ohtna_bootstrap_show_table", "Show Table", value = TRUE),
            checkboxInput("ohtna_bootstrap_show_plot", "Show Plot", value = TRUE),
            numericInput("ohtna_bootstrap_iterations", "Iterations:", value = 1000, min = 0, max = 10000),
            numericInput("ohtna_bootstrap_level", "Level:", value = 0.05, min = 0, max = 1, step = 0.01)
          ),

          accordion_panel(
            title = "Permutation Test", icon = bsicons::bs_icon("arrow-left-right"),
            checkboxInput("ohtna_permutation_show_table", "Show Table", value = TRUE),
            checkboxInput("ohtna_permutation_show_plot", "Show Plot", value = TRUE),
            numericInput("ohtna_permutation_iter", "Iterations:", value = 1000, min = 1),
            numericInput("ohtna_permutation_level", "Level:", value = 0.05, min = 0, max = 1)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "ohtna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("th"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("One-Hot Co-occurrence Network"),
              tags$span(
                downloadButton("ohtna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_network_plot", width = "600px", height = "600px")
            ),
            card_footer(
              class = "p-0",
              accordion(
                id = "ohtna_network_accordion",
                open = FALSE,
                accordion_panel(
                  title = "Network Summary",
                  icon = bsicons::bs_icon("table"),
                  DT::dataTableOutput("ohtna_summary_table")
                ),
                accordion_panel(
                  title = "Co-occurrence Matrix",
                  icon = bsicons::bs_icon("grid-3x3"),
                  verbatimTextOutput("ohtna_matrix_output")
                )
              )
            )
          )
        ),

        nav_panel(
          title = "Metrics",
          icon = icon("chart-bar"),
          layout_column_wrap(
            width = 1/2,
            heights_equal = "row",
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Histogram"),
                tags$span(
                  downloadButton("ohtna_histogram_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("ohtna_histogram_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("ohtna_histogram_plot", width = "600px", height = "400px")
              )
            ),
            card(
              full_screen = TRUE,
              card_header(
                class = "d-flex justify-content-between align-items-center",
                tags$strong("Frequencies"),
                tags$span(
                  downloadButton("ohtna_frequencies_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                  downloadButton("ohtna_frequencies_pdf", "PDF", class = "btn-sm btn-outline-secondary")
                )
              ),
              card_body(
                class = "p-2",
                style = "display: flex; justify-content: center; align-items: center;",
                plotOutput("ohtna_frequencies_plot", width = "600px", height = "400px")
              )
            )
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Mosaic"),
              tags$span(
                downloadButton("ohtna_mosaic_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_mosaic_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_mosaic_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Table"),
            DT::dataTableOutput("ohtna_centrality_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Centrality Plot"),
              tags$span(
                downloadButton("ohtna_centrality_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_centrality_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_centrality_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            card_header("Community Table"),
            DT::dataTableOutput("ohtna_community_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Community Plot"),
              tags$span(
                downloadButton("ohtna_community_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_community_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_community_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Bootstrap",
          icon = icon("random"),
          card(
            card_header("Bootstrap Table"),
            DT::dataTableOutput("ohtna_bootstrap_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Bootstrap Plot"),
              tags$span(
                downloadButton("ohtna_bootstrap_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_bootstrap_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_bootstrap_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Permutation",
          icon = icon("random"),
          card(
            card_header("Permutation Test Results"),
            DT::dataTableOutput("ohtna_permutation_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Permutation Plot"),
              tags$span(
                downloadButton("ohtna_permutation_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("ohtna_permutation_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("ohtna_permutation_plot", width = "600px", height = "400px")
            )
          )
        )
      )
    )
  ),

  # ---- SNA Tab ----
  nav_panel(
    title = "SNA",
    icon = icon("project-diagram"),
    layout_sidebar(
      sidebar = sidebar(
        title = "Social Network Analysis",
        width = 350,

        # Data status indicator
        uiOutput("sna_data_indicator"),
        hr(),

        # Aggregation Function
        radioButtons("sna_aggregate", "Aggregation:",
                     choices = c("Sum" = "sum", "Mean" = "mean",
                                "Max" = "max", "Min" = "min"),
                     selected = "sum", inline = TRUE),

        checkboxInput("sna_show_plot", "Show Plot", value = TRUE),

        # Collapsible sections
        accordion(
          id = "sna_accordion",
          open = FALSE,

          accordion_panel(
            title = "Visualization Settings", icon = bsicons::bs_icon("palette"),
            numericInput("sna_node_size", "Node Size:", value = 8, min = 1, max = 30),
            numericInput("sna_edge_size", "Edge Size:", value = 6, min = 1, max = 30),
            checkboxInput("sna_show_node_labels", "Show node labels", value = FALSE),
            checkboxInput("sna_show_edge_labels", "Show edge labels", value = FALSE),
            selectInput("sna_layout", "Layout:", choices = layout_choices),
            selectInput("sna_theme", "Color theme:", choices = theme_choices),
            numericInput("sna_plot_cut", "Cut value:", value = 0.1, min = 0, max = 1, step = 0.05),
            numericInput("sna_plot_min", "Minimum value:", value = 0.05, min = 0, max = 1, step = 0.01),
            numericInput("sna_plot_width", "Plot width:", value = 600, min = 300, max = 1200),
            numericInput("sna_plot_height", "Plot height:", value = 600, min = 300, max = 1200)
          ),

          accordion_panel(
            title = "Graph Level Metrics", icon = bsicons::bs_icon("graph-up"),
            checkboxInput("sna_show_summary", "Summary Table", value = TRUE),
            checkboxInput("sna_show_degree_plot", "Degree Distribution", value = TRUE)
          ),

          accordion_panel(
            title = "Centrality Analysis", icon = bsicons::bs_icon("bullseye"),
            checkboxInput("sna_centrality_show", "Show Centrality Table", value = TRUE),
            checkboxGroupInput("sna_centrality_measures", "Measures:",
                              choices = centrality_measures,
                              selected = c("OutStrength", "InStrength", "Betweenness")),
            checkboxInput("sna_centrality_loops", "Include self-loops", value = FALSE),
            checkboxInput("sna_centrality_normalize", "Normalize values", value = FALSE)
          ),

          accordion_panel(
            title = "Community Detection", icon = bsicons::bs_icon("people"),
            checkboxInput("sna_community_show", "Show Community Table", value = TRUE),
            checkboxGroupInput("sna_community_methods", "Methods:",
                              choices = community_methods,
                              selected = c("walktrap", "fast_greedy"))
          ),

          accordion_panel(
            title = "Edge Betweenness", icon = bsicons::bs_icon("arrow-right"),
            checkboxInput("sna_edge_betweenness_show", "Show Edge Betweenness Table", value = TRUE),
            checkboxInput("sna_edge_betweenness_directed", "Directed Network", value = TRUE)
          )
        )
      ),

      # Main panel with subtabs
      navset_pill(
        id = "sna_output_tabs",

        nav_panel(
          title = "Network",
          icon = icon("project-diagram"),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("SNA Network"),
              tags$span(
                downloadButton("sna_plot_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("sna_plot_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("sna_network_plot", width = "600px", height = "600px")
            )
          )
        ),

        nav_panel(
          title = "Metrics",
          icon = icon("chart-bar"),
          card(
            card_header("Summary"),
            DT::dataTableOutput("sna_summary_table")
          ),
          card(
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              tags$strong("Degree Distribution"),
              tags$span(
                downloadButton("sna_degree_png", "PNG", class = "btn-sm btn-outline-secondary me-1"),
                downloadButton("sna_degree_pdf", "PDF", class = "btn-sm btn-outline-secondary")
              )
            ),
            card_body(
              class = "p-2",
              style = "display: flex; justify-content: center; align-items: center;",
              plotOutput("sna_degree_plot", width = "600px", height = "400px")
            )
          )
        ),

        nav_panel(
          title = "Centrality",
          icon = icon("bullseye"),
          card(
            card_header("Centrality Measures"),
            DT::dataTableOutput("sna_centrality_table")
          )
        ),

        nav_panel(
          title = "Community",
          icon = icon("users"),
          card(
            card_header("Community Assignments"),
            DT::dataTableOutput("sna_community_table")
          )
        ),

        nav_panel(
          title = "Edge Betweenness",
          icon = icon("route"),
          card(
            card_header("Edge Betweenness"),
            DT::dataTableOutput("sna_edge_betweenness_table")
          )
        )
      )
    )
  ),

  # ---- About Tab ----
  nav_panel(
    title = "About",
    icon = icon("info-circle"),
    card(
      card_header("About TNA Shiny App"),
      card_body(
        h4("Transition Network Analysis (TNA)"),
        p("This Shiny application provides a comprehensive interface for Transition Network Analysis,
          converted from the JTNA Jamovi module."),
        h5("Available Analyses:"),
        tags$ul(
          tags$li(tags$strong("SNA"), " - Social Network Analysis for edge-list data (from/to/weight format)"),
          tags$li(tags$strong("TNA"), " - Transition Network Analysis for sequence data"),
          tags$li(tags$strong("Group TNA"), " - TNA with group comparisons and permutation tests"),
          tags$li(tags$strong("Cluster TNA"), " - TNA with automatic sequence clustering"),
          tags$li(tags$strong("Co-occurrence"), " - Co-occurrence Network Analysis for simultaneous events"),
          tags$li(tags$strong("One-Hot"), " - One-Hot encoded Co-occurrence Network Analysis")
        ),
        h5("Data Format Examples:"),
        tags$ul(
          tags$li(tags$strong("SNA:"), " CSV with columns: from, to, weight (optional), group (optional)"),
          tags$li(tags$strong("TNA/Group TNA/Cluster TNA:"), " CSV with columns: action (required), actor, time or order, group"),
          tags$li(tags$strong("One-Hot:"), " CSV with binary (0/1) columns representing actions")
        ),
        h5("Sample Data:"),
        p("Sample data files are available in the data/ folder:"),
        tags$ul(
          tags$li(tags$code("sample_edgelist.csv"), " - For SNA (edge list with groups)"),
          tags$li(tags$code("sample_sequences.csv"), " - For TNA/Group TNA (sequence data)"),
          tags$li(tags$code("sample_onehot.csv"), " - For One-Hot analysis"),
          tags$li(tags$code("Regulation_long.csv"), " - Larger real-world dataset")
        ),
        h5("Resources:"),
        tags$ul(
          tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html",
                        "TNA Tutorial", target = "_blank")),
          tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html",
                        "FTNA Guide", target = "_blank")),
          tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html",
                        "Group TNA Guide", target = "_blank"))
        ),
        hr(),
        p("Version: 1.0.0"),
        p("Based on the tna and codyna R packages")
      )
    )
  )
)

# ============================================================================
# SERVER
# ============================================================================

server <- function(input, output, session) {

  # ==========================================================================
  # Theme Settings
  # ==========================================================================

  # Observe theme changes and update the app theme

  observe({
    req(input$app_theme)
    session$setCurrentTheme(
      bs_theme(version = 5, bootswatch = input$app_theme)
    )
  })

  # Helper function to safely execute plots with margin handling
  safe_plot <- function(plot_expr, margins = c(0.5, 0.5, 1.5, 0.5)) {
    tryCatch({
      # Set minimal margins
      old_par <- par(no.readonly = TRUE)
      on.exit(par(old_par), add = TRUE)
      par(mar = margins, oma = c(0, 0, 0, 0))

      # Execute the plot expression
      force(plot_expr)
    }, error = function(e) {
      if (grepl("margin", tolower(e$message))) {
        # Try with even smaller margins
        par(mar = c(0.1, 0.1, 0.1, 0.1), oma = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Expand window\nto view plot", cex = 0.9, col = "gray50")
      } else {
        par(mar = c(0.1, 0.1, 0.1, 0.1))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 0.7, col = "red")
      }
    })
  }

  # ==========================================================================
  # Helper: Smart column matching
  # ==========================================================================

 # Only auto-select if column name matches expected names exactly (case-insensitive)
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

  # ==========================================================================
  # Helper: Styled DataTable with export buttons
  # ==========================================================================

  styled_datatable <- function(data, caption = NULL, pageLength = 15, scrollX = TRUE,
                                buttons = c("copy", "csv", "excel"), ...) {
    if (is.null(data) || nrow(data) == 0) {
      return(DT::datatable(
        data.frame(Message = "No data available"),
        options = list(dom = 't'),
        class = 'cell-border stripe'
      ))
    }

    # Round numeric columns to 4 decimal places for cleaner display
    numeric_cols <- sapply(data, is.numeric)
    data[numeric_cols] <- lapply(data[numeric_cols], function(x) round(x, 4))

    DT::datatable(
      data,
      caption = caption,
      extensions = c('Buttons'),
      options = list(
        dom = 'Bfrtip',
        buttons = buttons,
        pageLength = pageLength,
        scrollX = scrollX,
        autoWidth = TRUE,
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        ),
        initComplete = DT::JS(
          "function(settings, json) {",
          "  $(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#333', 'font-weight': '600', 'font-size': '0.75rem'});",
          "  $(this.api().table().body()).css({'font-size': '0.75rem'});",
          "}"
        )
      ),
      class = 'cell-border stripe hover compact',
      rownames = FALSE,
      ...
    ) %>%
      DT::formatStyle(
        columns = names(data),
        fontSize = '0.75rem'
      )
  }

  # ==========================================================================
  # CENTRALIZED DATA STORE
  # ==========================================================================
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

  # ==========================================================================
  # Data Loading Observers
  # ==========================================================================

  # Handle file upload
  observeEvent(input$data_file, {
    df <- read.csv(input$data_file$datapath, stringsAsFactors = FALSE)
    if (input$data_format == "activity_sequence") {
      shared_data$activity_data <- df
    } else {
      shared_data$edge_data <- df
    }
    shared_data$data_format <- input$data_format
  })

  # Handle sample data
  observeEvent(input$load_sample, {
    if (input$data_format == "activity_sequence") {
      shared_data$activity_data <- tna::group_regulation_long
      showNotification("Sample activity sequence data loaded", type = "message")
    } else {
      # Create sample edge list data
      sample_edges <- data.frame(
        from = c("A", "A", "B", "B", "C", "C", "D"),
        to = c("B", "C", "C", "D", "D", "A", "A"),
        weight = c(5, 3, 4, 2, 3, 2, 1),
        group = c("G1", "G1", "G1", "G2", "G2", "G2", "G2"),
        stringsAsFactors = FALSE
      )
      shared_data$edge_data <- sample_edges
      showNotification("Sample edge list data loaded", type = "message")
    }
    shared_data$data_format <- input$data_format
  })

  # Smart column matching for activity sequence
  observe({
    req(shared_data$activity_data)
    req(input$data_format == "activity_sequence")

    col_names <- names(shared_data$activity_data)
    cols_none <- c("(None)" = "", col_names)

    action_match <- find_matching_column(col_names, c("action", "activity", "event", "state"))
    actor_match <- find_matching_column(col_names, c("actor", "user", "student", "participant", "id"))
    group_match <- find_matching_column(col_names, c("group", "cluster", "category", "condition"))
    time_match <- find_matching_column(col_names, c("time", "timestamp", "datetime"))
    order_match <- find_matching_column(col_names, c("order", "sequence", "seq", "step"))

    updateSelectInput(session, "col_action", choices = col_names, selected = action_match)
    updateSelectInput(session, "col_actor", choices = cols_none, selected = actor_match)
    updateSelectInput(session, "col_group", choices = cols_none, selected = group_match)
    updateSelectInput(session, "col_time", choices = cols_none, selected = time_match)
    updateSelectInput(session, "col_order", choices = cols_none, selected = order_match)
  })

  # Smart column matching for edge list
  observe({
    req(shared_data$edge_data)
    req(input$data_format == "edge_list")

    col_names <- names(shared_data$edge_data)
    cols_none <- c("(None)" = "", col_names)

    from_match <- find_matching_column(col_names, c("from", "source", "src"))
    to_match <- find_matching_column(col_names, c("to", "target", "dest"))
    weight_match <- find_matching_column(col_names, c("weight", "value", "count"))
    group_match <- find_matching_column(col_names, c("group", "cluster", "category"))

    updateSelectInput(session, "col_from", choices = col_names, selected = from_match)
    updateSelectInput(session, "col_to", choices = col_names, selected = to_match)
    updateSelectInput(session, "col_weight", choices = cols_none, selected = weight_match)
    updateSelectInput(session, "col_sna_group", choices = cols_none, selected = group_match)
  })

  # Store column selections for activity sequence
  observe({
    shared_data$action_col <- input$col_action
    shared_data$actor_col <- if (!is.null(input$col_actor) && input$col_actor != "") input$col_actor else NULL
    shared_data$group_col <- if (!is.null(input$col_group) && input$col_group != "") input$col_group else NULL
    shared_data$time_col <- if (!is.null(input$col_time) && input$col_time != "") input$col_time else NULL
    shared_data$order_col <- if (!is.null(input$col_order) && input$col_order != "") input$col_order else NULL
    shared_data$has_group <- !is.null(shared_data$group_col)
  })

  # Store column selections for edge list
  observe({
    shared_data$from_col <- input$col_from
    shared_data$to_col <- input$col_to
    shared_data$weight_col <- if (!is.null(input$col_weight) && input$col_weight != "") input$col_weight else NULL
    shared_data$sna_group_col <- if (!is.null(input$col_sna_group) && input$col_sna_group != "") input$col_sna_group else NULL
  })

  # Data preview
  output$data_preview <- DT::renderDataTable({
    df <- if (input$data_format == "activity_sequence") shared_data$activity_data else shared_data$edge_data
    req(df)
    DT::datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # Data status (minimal)
  output$data_status <- renderUI({
    df <- if (input$data_format == "activity_sequence") shared_data$activity_data else shared_data$edge_data
    if (is.null(df)) {
      tags$small(class = "text-warning", bsicons::bs_icon("exclamation-triangle"), " No data loaded")
    } else {
      tagList(
        tags$small(class = "text-success", bsicons::bs_icon("check-circle-fill"), sprintf(" %d rows, %d cols", nrow(df), ncol(df))),
        if (shared_data$has_group && input$data_format == "activity_sequence") {
          tags$small(class = "text-info d-block mt-1", bsicons::bs_icon("people-fill"), " Groups enabled")
        }
      )
    }
  })

  # Empty state button handler
  observeEvent(input$empty_load_sample, {
    if (input$data_format == "activity_sequence") {
      shared_data$activity_data <- tna::group_regulation_long
      showNotification("Sample activity sequence data loaded", type = "message")
    } else {
      sample_edges <- data.frame(
        from = c("A", "A", "B", "B", "C", "C", "D"),
        to = c("B", "C", "C", "D", "D", "A", "A"),
        weight = c(5, 3, 4, 2, 3, 2, 1),
        group = c("G1", "G1", "G1", "G2", "G2", "G2", "G2"),
        stringsAsFactors = FALSE
      )
      shared_data$edge_data <- sample_edges
      showNotification("Sample edge list data loaded", type = "message")
    }
    shared_data$data_format <- input$data_format
  })

  # Data main panel - conditional rendering
  output$data_main_panel <- renderUI({
    df <- if (input$data_format == "activity_sequence") shared_data$activity_data else shared_data$edge_data

    if (is.null(df)) {
      # Empty state
      card(
        class = "h-100",
        card_body(
          class = "d-flex flex-column align-items-center justify-content-center text-center py-5",
          tags$div(
            class = "mb-4",
            bsicons::bs_icon("database", size = "5em", class = "text-muted")
          ),
          tags$h3(class = "mb-2", "Welcome to TNA"),
          tags$p(
            class = "text-muted mb-4",
            "Upload a CSV file or load sample data to get started with Transition Network Analysis"
          ),
          tags$div(
            class = "d-flex gap-2 flex-wrap justify-content-center",
            actionButton("empty_load_sample", "Load Sample Data",
                        class = "btn-primary", icon = icon("database")),
            tags$label(
              class = "btn btn-outline-secondary mb-0",
              `for` = "data_file",
              icon("upload"), " Upload CSV"
            )
          ),
          tags$div(
            class = "mt-4 text-muted",
            tags$small(
              "Current format: ",
              tags$strong(if (input$data_format == "activity_sequence") "Activity Sequence (TNA)" else "Edge List (SNA)")
            )
          )
        )
      )
    } else {
      # Data loaded state - just the preview card with stats in header
      card(
        full_screen = TRUE,
        card_header(
          class = "py-2",
          tags$div(
            class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
            tags$div(
              class = "d-flex align-items-center gap-2 flex-wrap",
              bsicons::bs_icon("table", class = "me-1"),
              tags$strong("Data Preview"),
              uiOutput("data_stats_badges", inline = TRUE)
            ),
            downloadButton("data_download_csv", "CSV",
                          class = "btn-sm btn-outline-secondary")
          )
        ),
        DT::dataTableOutput("data_preview")
      )
    }
  })

  # Data stats badges (inline in header)
  output$data_stats_badges <- renderUI({
    df <- if (input$data_format == "activity_sequence") shared_data$activity_data else shared_data$edge_data
    req(df)

    if (input$data_format == "activity_sequence") {
      n_actions <- if (!is.null(shared_data$action_col) && shared_data$action_col %in% names(df)) {
        length(unique(df[[shared_data$action_col]]))
      } else NULL

      n_actors <- if (!is.null(shared_data$actor_col) && shared_data$actor_col %in% names(df)) {
        length(unique(df[[shared_data$actor_col]]))
      } else NULL

      n_groups <- if (!is.null(shared_data$group_col) && shared_data$group_col %in% names(df)) {
        length(unique(df[[shared_data$group_col]]))
      } else NULL

      tags$span(
        class = "d-inline-flex gap-1 flex-wrap",
        tags$span(class = "badge bg-secondary", paste(format(nrow(df), big.mark = ","), "rows")),
        tags$span(class = "badge bg-secondary", paste(ncol(df), "cols")),
        if (!is.null(n_actions)) tags$span(class = "badge bg-info", paste(n_actions, "actions")),
        if (!is.null(n_actors)) tags$span(class = "badge bg-success", paste(n_actors, "actors")),
        if (!is.null(n_groups)) tags$span(class = "badge bg-warning text-dark", paste(n_groups, "groups"))
      )
    } else {
      n_nodes <- if (!is.null(shared_data$from_col) && !is.null(shared_data$to_col) &&
                     shared_data$from_col %in% names(df) && shared_data$to_col %in% names(df)) {
        length(unique(c(df[[shared_data$from_col]], df[[shared_data$to_col]])))
      } else NULL

      n_groups <- if (!is.null(shared_data$sna_group_col) && shared_data$sna_group_col %in% names(df)) {
        length(unique(df[[shared_data$sna_group_col]]))
      } else NULL

      tags$span(
        class = "d-inline-flex gap-1 flex-wrap",
        tags$span(class = "badge bg-secondary", paste(format(nrow(df), big.mark = ","), "edges")),
        tags$span(class = "badge bg-secondary", paste(ncol(df), "cols")),
        if (!is.null(n_nodes)) tags$span(class = "badge bg-info", paste(n_nodes, "nodes")),
        if (!is.null(n_groups)) tags$span(class = "badge bg-warning text-dark", paste(n_groups, "groups"))
      )
    }
  })

  # Download CSV handler
  output$data_download_csv <- downloadHandler(
    filename = function() {
      format_name <- if (input$data_format == "activity_sequence") "activity_data" else "edge_data"
      paste0(format_name, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      df <- if (input$data_format == "activity_sequence") shared_data$activity_data else shared_data$edge_data
      write.csv(df, file, row.names = FALSE)
    }
  )

  # ==========================================================================
  # Module Data Status Indicators
  # ==========================================================================

  # TNA data indicator
  output$tna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Data: %d rows", nrow(shared_data$activity_data)))
    }
  })

  # Group TNA data indicator
  output$gtna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else if (is.null(shared_data$actor_col)) {
      div(class = "alert alert-warning alert-sm py-2", icon("user"), " Select an Actor column in Data tab")
    } else if (!shared_data$has_group) {
      div(class = "alert alert-warning alert-sm py-2", icon("users"), " Select a Group column in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Ready: %d rows (Group: %s)", nrow(shared_data$activity_data), shared_data$group_col))
    }
  })

  # Cluster TNA data indicator
  output$ctna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else if (is.null(shared_data$actor_col)) {
      div(class = "alert alert-warning alert-sm py-2", icon("user"), " Select an Actor column in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Data: %d rows", nrow(shared_data$activity_data)))
    }
  })

  # Co-occurrence TNA data indicator
  output$cotna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Data: %d rows", nrow(shared_data$activity_data)))
    }
  })

  # One-Hot TNA data indicator
  output$ohtna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Data: %d rows, %d columns", nrow(shared_data$activity_data), ncol(shared_data$activity_data)))
    }
  })

  # SNA data indicator
  output$sna_data_indicator <- renderUI({
    if (is.null(shared_data$edge_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load Edge List data in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Edge data: %d rows", nrow(shared_data$edge_data)))
    }
  })

  # ==========================================================================
  # SNA Module
  # ==========================================================================

  # Reactive data for SNA (uses shared data)
  sna_data <- reactive({
    req(shared_data$edge_data)
    shared_data$edge_data
  })

  # Build SNA model
  sna_model <- reactive({
    req(shared_data$from_col, shared_data$to_col)
    req(shared_data$from_col != "", shared_data$to_col != "")

    df <- data.frame(
      from = as.character(sna_data()[[shared_data$from_col]]),
      to = as.character(sna_data()[[shared_data$to_col]]),
      stringsAsFactors = FALSE
    )

    # Add weight column
    if(!is.null(shared_data$weight_col) && shared_data$weight_col != "") {
      df$weight <- as.numeric(sna_data()[[shared_data$weight_col]])
    } else {
      df$weight <- 1
    }

    # Remove NA rows
    df <- df[complete.cases(df), ]

    if(nrow(df) == 0) return(NULL)

    # Get aggregation function
    agg_fn <- switch(input$sna_aggregate,
                     "sum" = sum,
                     "mean" = mean,
                     "max" = max,
                     "min" = min,
                     sum)

    # Check for group column
    if(!is.null(shared_data$sna_group_col) && shared_data$sna_group_col != "") {
      # Build separate models for each group
      df$group <- as.character(sna_data()[[shared_data$sna_group_col]])
      groups <- unique(df$group)
      groupModels <- list()

      for(g in groups) {
        groupDf <- df[df$group == g, c("from", "to", "weight")]
        groupModels[[g]] <- tna::sna(groupDf, aggregate = agg_fn)
      }

      return(list(type = "group", models = groupModels))
    } else {
      # Build single model
      model <- tna::sna(df[, c("from", "to", "weight")], aggregate = agg_fn)
      return(list(type = "single", model = model))
    }
  })

  # SNA Network Plot
  output$sna_network_plot <- renderPlot({
    req(sna_model(), input$sna_show_plot)

    result <- sna_model()

    # Set minimal margins
    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      if(result$type == "group") {
        groupModels <- result$models
        nGroups <- length(groupModels)
        nCols <- ceiling(sqrt(nGroups))
        nRows <- ceiling(nGroups / nCols)

        par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

        for(g in names(groupModels)) {
          plot(groupModels[[g]],
               title = g,
               cut = input$sna_plot_cut,
               minimum = input$sna_plot_min,
               layout = input$sna_layout,
               vsize = input$sna_node_size,
               esize = input$sna_edge_size,
               labels = input$sna_show_node_labels,
               edge.labels = input$sna_show_edge_labels,
               theme = input$sna_theme,
               bg = "white")
        }

        par(mfrow = c(1, 1))
      } else {
        plot(result$model,
             cut = input$sna_plot_cut,
             minimum = input$sna_plot_min,
             layout = input$sna_layout,
             vsize = input$sna_node_size,
             esize = input$sna_edge_size,
             labels = input$sna_show_node_labels,
             edge.labels = input$sna_show_edge_labels,
             theme = input$sna_theme,
             bg = "white")
      }
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # SNA Summary Table
  output$sna_summary_table <- DT::renderDataTable({
    req(sna_model(), input$sna_show_summary)

    result <- sna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    summaryDf <- summary(model)

    styled_datatable(summaryDf, caption = "Network Summary Statistics")
  })

  # SNA Degree Plot
  output$sna_degree_plot <- renderPlot({
    req(sna_model(), input$sna_show_degree_plot)

    par(mar = c(3, 3, 2, 1), oma = c(0, 0, 0, 0))

    result <- sna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      print(hist(model))
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # SNA Centrality Table
  output$sna_centrality_table <- DT::renderDataTable({
    req(sna_model(), input$sna_centrality_show)
    req(length(input$sna_centrality_measures) > 0)

    result <- sna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    cent <- centralities(x = model,
                        loops = input$sna_centrality_loops,
                        normalize = input$sna_centrality_normalize,
                        measures = input$sna_centrality_measures)

    styled_datatable(cent, caption = "Centrality Measures")
  })

  # SNA Community Table
  output$sna_community_table <- DT::renderDataTable({
    req(sna_model(), input$sna_community_show)
    req(length(input$sna_community_methods) > 0)

    result <- sna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    comm <- communities(x = model, methods = input$sna_community_methods)

    styled_datatable(comm$assignments, caption = "Community Assignments")
  })

  # SNA Edge Betweenness Table
  output$sna_edge_betweenness_table <- DT::renderDataTable({
    req(sna_model(), input$sna_edge_betweenness_show)

    result <- sna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    ebNetwork <- betweenness_network(x = model, directed = input$sna_edge_betweenness_directed)
    ebMatrix <- ebNetwork$weights

    # Convert to edge list
    df <- data.frame(
      from = character(),
      to = character(),
      betweenness = numeric(),
      stringsAsFactors = FALSE
    )

    for(i in 1:nrow(ebMatrix)) {
      for(j in 1:ncol(ebMatrix)) {
        if(ebMatrix[i, j] > 0) {
          df <- rbind(df, data.frame(
            from = rownames(ebMatrix)[i],
            to = colnames(ebMatrix)[j],
            betweenness = ebMatrix[i, j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    df <- df[order(-df$betweenness), ]

    styled_datatable(df, caption = "Edge Betweenness")
  })

  # ==========================================================================
  # TNA Module
  # ==========================================================================

  # Reactive data for TNA (uses shared data)
  tna_data <- reactive({
    req(shared_data$activity_data)
    shared_data$activity_data
  })

  # Prepare TNA data
  tna_prepared_data <- reactive({
    req(shared_data$action_col)
    req(shared_data$action_col != "")

    copyData <- tna_data()
    copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])

    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      time_threshold = input$tna_threshold
    )

    if(!is.null(shared_data$actor_col) && shared_data$actor_col != "") {
      copyData[[shared_data$actor_col]] <- as.character(copyData[[shared_data$actor_col]])
      args_prepare_data$actor <- shared_data$actor_col
    }

    if(!is.null(shared_data$time_col) && shared_data$time_col != "") {
      copyData[[shared_data$time_col]] <- as.POSIXct(copyData[[shared_data$time_col]])
      args_prepare_data$time <- shared_data$time_col
    }

    if(!is.null(shared_data$order_col) && shared_data$order_col != "") {
      args_prepare_data$order <- shared_data$order_col
    }

    args_prepare_data$data <- copyData

    tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) {
      NULL
    })
  })

  # Store computed TNA model
  tna_model <- reactiveVal(NULL)

  # Auto-run TNA analysis when data first becomes available
  observe({
    req(tna_prepared_data())
    # Only auto-run if model is NULL (first time)
    if (is.null(isolate(tna_model()))) {
      dataForTNA <- tna_prepared_data()
      scaling <- if(isolate(input$tna_scaling) == "noScaling") character(0L) else isolate(input$tna_scaling)

      result <- tryCatch({
        if(isolate(input$tna_type) == "attention") {
          build_model(x = dataForTNA, type = isolate(input$tna_type), scaling = scaling, lambda = isolate(input$tna_lambda))
        } else {
          build_model(x = dataForTNA, type = isolate(input$tna_type), scaling = scaling)
        }
      }, error = function(e) NULL)

      if (!is.null(result)) {
        tna_model(result)
      }
    }
  })

  # Run TNA analysis when button is clicked
  observeEvent(input$tna_run, {
    removeNotification(id = "tna_settings_warning")
    req(tna_prepared_data())

    dataForTNA <- tna_prepared_data()
    scaling <- if(input$tna_scaling == "noScaling") character(0L) else input$tna_scaling

    result <- tryCatch({
      if(input$tna_type == "attention") {
        build_model(x = dataForTNA, type = input$tna_type, scaling = scaling, lambda = input$tna_lambda)
      } else {
        build_model(x = dataForTNA, type = input$tna_type, scaling = scaling)
      }
    }, error = function(e) {
      showNotification(paste("Error building model:", e$message), type = "error")
      NULL
    })

    if (!is.null(result)) {
      tna_model(result)
      showNotification("TNA analysis complete", type = "message")
    }
  })

  # Clear TNA analysis
  observeEvent(input$tna_clear, {
    tna_model(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Analysis'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$tna_type, {
    if (!is.null(tna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "tna_settings_warning")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$tna_scaling, {
    if (!is.null(tna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "tna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # TNA Summary Table
  output$tna_summary_table <- DT::renderDataTable({
    model <- tna_model()
    req(model)

    tryCatch({
      summary_df <- summary(model)
      DT::datatable(
        summary_df,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE
        ),
        rownames = FALSE,
        colnames = c("Metric", "Value"),
        class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) {
      DT::datatable(data.frame(Metric = "Error", Value = as.character(e$message)))
    })
  })

  # TNA Network Plot
  output$tna_network_plot <- renderPlot({
    # Show message if no data or model not ready
    if (is.null(shared_data$activity_data)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Load data in the Data tab to begin", cex = 1.2, col = "gray50")
      return()
    }

    if (is.null(shared_data$action_col) || shared_data$action_col == "") {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Select an Action column in the Data tab", cex = 1.2, col = "gray50")
      return()
    }

    if (is.null(tna_prepared_data())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Error preparing data.\nCheck your column selections.", cex = 1, col = "#e74c3c")
      return()
    }

    if (is.null(tna_model())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Analysis' to build the network", cex = 1.2, col = "gray50")
      return()
    }

    if (!input$tna_show_plot) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show TNA Plot' to view", cex = 1, col = "gray50")
      return()
    }

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      plot(x = tna_model(),
           cut = input$tna_plot_cut,
           minimum = input$tna_plot_min,
           edge.label.cex = input$tna_edge_label_size,
           node.width = input$tna_node_size,
           label.cex = input$tna_node_label_size,
           layout = input$tna_layout,
           bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # TNA Matrix Output
  output$tna_matrix_output <- renderPrint({
    if (is.null(tna_model())) {
      cat("Build a model to see the transition matrix")
      return(invisible(NULL))
    }
    if (!input$tna_show_matrix) {
      cat("Enable 'Show Matrix' in the sidebar to view")
      return(invisible(NULL))
    }
    model <- tna_model()
    round(model$weights, 4)
  })

  # TNA Histogram
  output$tna_histogram_plot <- renderPlot({
    req(tna_model(), input$tna_show_histogram)
    par(mar = c(3, 3, 2, 1), oma = c(0, 0, 0, 0))
    tryCatch({
      hist(x = tna_model(), main = "Histogram of Edge Weights",
           xlab = "Edge Weights", ylab = "Frequency")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # TNA Frequencies Plot
  output$tna_frequencies_plot <- renderPlot({
    req(tna_model(), input$tna_show_frequencies)
    tryCatch({
      p <- plot_frequencies(x = tna_model())
      print(p)
    }, error = function(e) {
      hist(x = tna_model(), main = "Frequencies Plot",
           xlab = "Edge Weights", ylab = "Frequency")
    })
  })

  # TNA Mosaic Plot
  output$tna_mosaic_plot <- renderPlot({
    req(tna_model(), input$tna_show_mosaic, input$tna_type == "frequency")
    p <- plot_mosaic(x = tna_model())
    print(p)
  })

  # TNA Centrality Table
  output$tna_centrality_table <- DT::renderDataTable({
    req(tna_model(), input$tna_centrality_show_table)
    req(length(input$tna_centrality_measures) > 0)

    cent <- centralities(x = tna_model(),
                        loops = input$tna_centrality_loops,
                        normalize = input$tna_centrality_normalize,
                        measures = input$tna_centrality_measures)

    styled_datatable(cent, caption = "Centrality Measures")
  })

  # TNA Centrality Plot
  output$tna_centrality_plot <- renderPlot({
    req(tna_model(), input$tna_centrality_show_plot)
    req(length(input$tna_centrality_measures) > 0)

    cent <- centralities(x = tna_model(),
                        loops = input$tna_centrality_loops,
                        normalize = input$tna_centrality_normalize,
                        measures = input$tna_centrality_measures)

    centPlot <- plot(cent)
    print(centPlot)
  })

  # TNA Stability Table
  output$tna_stability_table <- DT::renderDataTable({
    req(tna_model(), input$tna_stability_show_table)
    req(length(input$tna_stability_measures) > 0)

    csResult <- tryCatch({
      tna::estimate_cs(
        x = tna_model(),
        loops = input$tna_centrality_loops,
        normalize = input$tna_centrality_normalize,
        measures = input$tna_stability_measures,
        iter = input$tna_stability_iterations,
        threshold = input$tna_stability_threshold,
        certainty = input$tna_stability_certainty,
        progressbar = FALSE
      )
    }, error = function(e) NULL)

    if(is.null(csResult)) return(NULL)

    df <- data.frame(
      measure = names(csResult),
      cs_coefficient = sapply(csResult, function(x) x$cs_coefficient),
      stringsAsFactors = FALSE
    )

    styled_datatable(df, caption = "Centrality Stability Coefficients")
  })

  # TNA Stability Plot
  output$tna_stability_plot <- renderPlot({
    req(tna_model(), input$tna_stability_show_plot)
    req(length(input$tna_stability_measures) > 0)

    csResult <- tryCatch({
      tna::estimate_cs(
        x = tna_model(),
        loops = input$tna_centrality_loops,
        normalize = input$tna_centrality_normalize,
        measures = input$tna_stability_measures,
        iter = input$tna_stability_iterations,
        threshold = input$tna_stability_threshold,
        certainty = input$tna_stability_certainty,
        progressbar = FALSE
      )
    }, error = function(e) NULL)

    if(is.null(csResult)) return()

    p <- plot(csResult)
    print(p)
  })

  # TNA Edge Betweenness Table
  output$tna_edge_betweenness_table <- DT::renderDataTable({
    req(tna_model(), input$tna_edge_betweenness_show_table)

    ebNetwork <- betweenness_network(x = tna_model())
    ebMatrix <- ebNetwork$weights

    df <- data.frame(
      from = character(),
      to = character(),
      value = numeric(),
      stringsAsFactors = FALSE
    )

    for(i in 1:nrow(ebMatrix)) {
      for(j in 1:ncol(ebMatrix)) {
        if(ebMatrix[i, j] > 0) {
          df <- rbind(df, data.frame(
            from = rownames(ebMatrix)[i],
            to = colnames(ebMatrix)[j],
            value = ebMatrix[i, j],
            stringsAsFactors = FALSE
          ))
        }
      }
    }

    df <- df[order(-df$value), ]

    styled_datatable(df, caption = "Edge Betweenness")
  })

  # TNA Edge Betweenness Plot
  output$tna_edge_betweenness_plot <- renderPlot({
    req(tna_model(), input$tna_edge_betweenness_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      ebNetwork <- betweenness_network(x = tna_model())

      plot(x = ebNetwork,
           cut = input$tna_plot_cut,
           minimum = input$tna_plot_min,
           layout = input$tna_edge_betweenness_layout,
           bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # TNA Community Table
  output$tna_community_table <- DT::renderDataTable({
    req(tna_model(), input$tna_community_show_table)

    coms <- tryCatch({
      tna::communities(x = tna_model(),
                      methods = input$tna_community_method,
                      gamma = input$tna_community_gamma)
    }, error = function(e) NULL)

    if(is.null(coms)) return(NULL)

    styled_datatable(coms$assignments, caption = "Community Assignments")
  })

  # TNA Community Plot
  output$tna_community_plot <- renderPlot({
    req(tna_model(), input$tna_community_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      coms <- tna::communities(x = tna_model(),
                      methods = input$tna_community_method,
                      gamma = input$tna_community_gamma)

      if(is.null(coms)) return()

      plot(x = coms, method = input$tna_community_method, bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # TNA Bootstrap Table
  output$tna_bootstrap_table <- DT::renderDataTable({
    req(tna_model(), input$tna_bootstrap_show_table)

    bs <- tryCatch({
      bootstrap(
        x = tna_model(),
        iter = input$tna_bootstrap_iterations,
        level = input$tna_bootstrap_level,
        method = input$tna_bootstrap_method,
        threshold = if(input$tna_bootstrap_method == "threshold") input$tna_bootstrap_threshold else 0.1,
        consistency_range = c(input$tna_bootstrap_range_low, input$tna_bootstrap_range_up)
      )
    }, error = function(e) NULL)

    if(is.null(bs) || is.null(bs$summary)) return(NULL)

    all_edges <- bs$summary

    if(input$tna_bootstrap_significant_only) {
      all_edges <- all_edges[all_edges$sig == TRUE, ]
    }

    if(nrow(all_edges) > input$tna_bootstrap_max_rows) {
      all_edges <- all_edges[1:input$tna_bootstrap_max_rows, ]
    }

    styled_datatable(all_edges, caption = "Bootstrap Analysis Results")
  })

  # TNA Bootstrap Plot
  output$tna_bootstrap_plot <- renderPlot({
    req(tna_model(), input$tna_bootstrap_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      bs <- bootstrap(
        x = tna_model(),
        iter = input$tna_bootstrap_iterations,
        level = input$tna_bootstrap_level,
        method = input$tna_bootstrap_method,
        threshold = if(input$tna_bootstrap_method == "threshold") input$tna_bootstrap_threshold else 0.1,
        consistency_range = c(input$tna_bootstrap_range_low, input$tna_bootstrap_range_up)
      )

      if(is.null(bs)) return()

      plot(x = bs, cut = 0.01)
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # TNA Sequences Plot
  output$tna_sequences_plot <- renderPlot({
    req(tna_model(), input$tna_sequences_show_plot)

    p <- tryCatch({
      tna::plot_sequences(
        x = tna_model(),
        type = input$tna_sequences_type,
        scale = input$tna_sequences_scale,
        geom = input$tna_sequences_geom,
        include_na = input$tna_sequences_include_na,
        tick = input$tna_sequences_tick
      )
    }, error = function(e) NULL)

    if(!is.null(p)) print(p)
  })

  # TNA Pattern Table
  output$tna_pattern_table <- DT::renderDataTable({
    req(tna_prepared_data(), input$tna_pattern_show_table)

    dataForTNA <- tna_prepared_data()
    seq_data <- dataForTNA$sequence_data

    patterns <- tryCatch({
      if(input$tna_pattern_type == "custom" && nchar(input$tna_pattern_custom) > 0) {
        codyna::discover_patterns(
          data = seq_data,
          pattern = input$tna_pattern_custom,
          min_support = input$tna_pattern_min_support,
          min_count = input$tna_pattern_min_count
        )
      } else {
        len_range <- input$tna_pattern_len_min:input$tna_pattern_len_max
        codyna::discover_patterns(
          data = seq_data,
          type = input$tna_pattern_type,
          len = len_range,
          min_support = input$tna_pattern_min_support,
          min_count = input$tna_pattern_min_count
        )
      }
    }, error = function(e) NULL)

    if(is.null(patterns) || nrow(patterns) == 0) return(NULL)

    if(nrow(patterns) > input$tna_pattern_max_rows) {
      patterns <- patterns[1:input$tna_pattern_max_rows, ]
    }

    styled_datatable(patterns, caption = "Pattern Discovery Results")
  })

  # TNA Indices Table
  output$tna_indices_table <- DT::renderDataTable({
    req(tna_prepared_data(), input$tna_indices_show_table)

    dataForTNA <- tna_prepared_data()
    seq_data <- dataForTNA$sequence_data
    seq_data <- as.data.frame(lapply(seq_data, as.character), stringsAsFactors = FALSE)

    indices <- tryCatch({
      codyna::sequence_indices(data = seq_data)
    }, error = function(e) NULL)

    if(is.null(indices)) return(NULL)

    indices$sequence_id <- 1:nrow(indices)

    if(nrow(indices) > input$tna_indices_max_rows) {
      indices <- indices[1:input$tna_indices_max_rows, ]
    }

    styled_datatable(indices, caption = "Sequence Indices")
  })

  # ==========================================================================
  # Group TNA Module
  # ==========================================================================

  # Reactive data for Group TNA (uses shared data)
  gtna_data <- reactive({
    req(shared_data$activity_data)
    req(shared_data$actor_col)   # Require actor column for Group TNA
    shared_data$activity_data
  })

  # Store computed Group TNA model

  gtna_model <- reactiveVal(NULL)

  # Run Group TNA when button is clicked
  observeEvent(input$gtna_run, {
    removeNotification(id = "gtna_settings_warning")
    req(shared_data$action_col, shared_data$actor_col)
    req(shared_data$action_col != "", shared_data$actor_col != "")

    # Check if group column is available
    if (!shared_data$has_group) {
      showNotification("Please select a Group column in the Data tab", type = "warning")
      gtna_model(NULL)
      return()
    }

    copyData <- gtna_data()
    copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])
    copyData[[shared_data$actor_col]] <- as.character(copyData[[shared_data$actor_col]])

    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      actor = shared_data$actor_col,
      time_threshold = input$gtna_threshold
    )

    if(!is.null(shared_data$time_col) && shared_data$time_col != "") {
      copyData[[shared_data$time_col]] <- as.POSIXct(copyData[[shared_data$time_col]])
      args_prepare_data$time <- shared_data$time_col
    }

    if(!is.null(shared_data$order_col) && shared_data$order_col != "") {
      args_prepare_data$order <- shared_data$order_col
    }

    args_prepare_data$data <- copyData

    dataForTNA <- tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) {
      showNotification(paste("Error preparing data:", e$message), type = "error")
      NULL
    })

    if(is.null(dataForTNA)) {
      gtna_model(NULL)
      return()
    }

    scaling <- if(input$gtna_scaling == "noScaling") character(0L) else input$gtna_scaling

    # Get actor-to-group mapping (one group per actor)
    # After prepare_data, we have sequence data with one row per actor
    actor_group_map <- unique(copyData[, c(shared_data$actor_col, shared_data$group_col)])
    names(actor_group_map) <- c("actor", "group")

    # Get the actor names from the prepared sequence data
    seq_actors <- rownames(dataForTNA$sequence_data)

    # Create group vector matching the sequence data order
    group_vec <- actor_group_map$group[match(seq_actors, actor_group_map$actor)]

    result <- tryCatch({
      if(input$gtna_type == "attention") {
        tna::group_model(x = dataForTNA, group = group_vec, type = input$gtna_type,
                        scaling = scaling, lambda = input$gtna_lambda)
      } else {
        tna::group_model(x = dataForTNA, group = group_vec, type = input$gtna_type, scaling = scaling)
      }
    }, error = function(e) {
      showNotification(paste("Error building model:", e$message), type = "error")
      NULL
    })

    if (!is.null(result)) {
      gtna_model(result)
      showNotification(paste("Group analysis complete:", length(result), "groups"), type = "message")
    } else {
      gtna_model(NULL)
    }
  })

  # Clear Group TNA analysis
  observeEvent(input$gtna_clear, {
    gtna_model(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Analysis'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$gtna_type, {
    if (!is.null(gtna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "gtna_settings_warning")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$gtna_scaling, {
    if (!is.null(gtna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "gtna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # Group TNA Summary Table
  output$gtna_summary_table <- DT::renderDataTable({
    model <- gtna_model()
    req(model)

    tryCatch({
      # Combine summaries from all groups
      summaries <- lapply(names(model), function(grp) {
        s <- summary(model[[grp]])
        s$group <- grp
        s
      })
      combined <- do.call(rbind, summaries)
      combined <- combined[, c("group", "metric", "value")]

      DT::datatable(
        combined,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          scrollY = "400px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE,
        colnames = c("Group", "Metric", "Value"),
        class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) {
      DT::datatable(data.frame(Message = "Run Group TNA analysis first"))
    })
  })

  # Group TNA Network Plot
  output$gtna_network_plot <- renderPlot({
    # Show helpful messages if requirements not met
    if (is.null(shared_data$activity_data)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Load data in the Data tab to begin", cex = 1.2, col = "gray50")
      return()
    }

    if (is.null(shared_data$actor_col)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Select an Actor column in the Data tab", cex = 1.2, col = "gray50")
      return()
    }

    if (!shared_data$has_group) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Select a Group column in the Data tab,\nthen click 'Run Group Analysis'", cex = 1.2, col = "gray50")
      return()
    }

    if (is.null(gtna_model())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Group Analysis' to begin", cex = 1.2, col = "gray50")
      return()
    }

    if (!input$gtna_show_plot) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show Plot' to view", cex = 1, col = "gray50")
      return()
    }

    groupModels <- gtna_model()

    if(is.null(groupModels) || length(groupModels) == 0) return()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        plot(groupModels[[g]],
             title = g,
             cut = input$gtna_plot_cut,
             minimum = input$gtna_plot_min,
             edge.label.cex = input$gtna_edge_label_size,
             node.width = input$gtna_node_size,
             label.cex = input$gtna_node_label_size,
             layout = input$gtna_layout,
             bg = "white")
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # Group TNA Matrix Output
  output$gtna_matrix_output <- renderPrint({
    req(gtna_model(), input$gtna_show_matrix)

    groupModels <- gtna_model()

    for(g in names(groupModels)) {
      cat("=== Group:", g, "===\n")
      print(round(groupModels[[g]]$weights, 4))
      cat("\n")
    }
  })

  # Group TNA Centrality Table
  output$gtna_centrality_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_centrality_show_table)
    req(length(input$gtna_centrality_measures) > 0)

    groupModels <- gtna_model()

    # Combine centrality from all groups
    all_cent <- NULL
    for(g in names(groupModels)) {
      cent <- centralities(x = groupModels[[g]],
                          loops = input$gtna_centrality_loops,
                          normalize = input$gtna_centrality_normalize,
                          measures = input$gtna_centrality_measures)
      cent$group <- g
      all_cent <- rbind(all_cent, cent)
    }

    styled_datatable(all_cent, caption = "Group Centrality Measures")
  })

  # Group TNA Centrality Plot
  output$gtna_centrality_plot <- renderPlot({
    req(gtna_model(), input$gtna_centrality_show_plot)
    req(length(input$gtna_centrality_measures) > 0)

    groupModels <- gtna_model()
    model <- groupModels[[1]]

    cent <- centralities(x = model,
                        loops = input$gtna_centrality_loops,
                        normalize = input$gtna_centrality_normalize,
                        measures = input$gtna_centrality_measures)

    centPlot <- plot(cent)
    print(centPlot)
  })

  # Group TNA Permutation Table
  output$gtna_permutation_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_permutation_show_table)

    groupModels <- gtna_model()

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$gtna_permutation_iter,
                           paired = input$gtna_permutation_paired,
                           level = input$gtna_permutation_level)
    }, error = function(e) NULL)

    if(is.null(permResult)) return(NULL)

    styled_datatable(permResult$summary, caption = "Permutation Test Results")
  })

  # Group TNA Permutation Plot
  output$gtna_permutation_plot <- renderPlot({
    req(gtna_model(), input$gtna_permutation_show_plot)

    groupModels <- gtna_model()

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$gtna_permutation_iter,
                           paired = input$gtna_permutation_paired,
                           level = input$gtna_permutation_level)
    }, error = function(e) NULL)

    if(is.null(permResult)) return()

    p <- plot(permResult)
    print(p)
  })

  # Group TNA Sequences Plot
  output$gtna_sequences_plot <- renderPlot({
    req(gtna_model(), input$gtna_sequences_show_plot)

    groupModels <- gtna_model()

    p <- tryCatch({
      tna::plot_sequences(
        x = groupModels,
        type = input$gtna_sequences_type,
        scale = input$gtna_sequences_scale,
        geom = input$gtna_sequences_geom,
        include_na = input$gtna_sequences_include_na,
        tick = input$gtna_sequences_tick
      )
    }, error = function(e) NULL)

    if(!is.null(p)) print(p)
  })

  # Group TNA Community Table
  output$gtna_community_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_community_show_table)

    groupModels <- gtna_model()

    all_coms <- NULL
    for(g in names(groupModels)) {
      coms <- tryCatch({
        tna::communities(x = groupModels[[g]],
                        methods = input$gtna_community_method,
                        gamma = input$gtna_community_gamma)
      }, error = function(e) NULL)

      if(!is.null(coms) && !is.null(coms$assignments)) {
        df <- coms$assignments
        df$group <- g
        all_coms <- rbind(all_coms, df)
      }
    }

    if(is.null(all_coms)) return(NULL)
    styled_datatable(all_coms, caption = "Community Assignments by Group")
  })

  # Group TNA Community Plot
  output$gtna_community_plot <- renderPlot({
    req(gtna_model(), input$gtna_community_show_plot)

    groupModels <- gtna_model()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        coms <- tna::communities(x = groupModels[[g]],
                                methods = input$gtna_community_method,
                                gamma = input$gtna_community_gamma)
        if(!is.null(coms)) {
          plot(x = coms, method = input$gtna_community_method, bg = "white")
          title(main = g, line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group TNA Cliques Text
  output$gtna_cliques_text <- renderPrint({
    req(gtna_model(), input$gtna_cliques_show_table)

    groupModels <- gtna_model()

    for(g in names(groupModels)) {
      cat("=== Group:", g, "===\n")
      cliques <- tryCatch({
        tna::cliques(x = groupModels[[g]],
                    size = input$gtna_cliques_size,
                    threshold = input$gtna_cliques_threshold)
      }, error = function(e) NULL)

      if(!is.null(cliques)) {
        print(cliques)
      } else {
        cat("No cliques found\n")
      }
      cat("\n")
    }
  })

  # Group TNA Cliques Plot
  output$gtna_cliques_plot <- renderPlot({
    req(gtna_model(), input$gtna_cliques_show_plot)

    groupModels <- gtna_model()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        cliques <- tna::cliques(x = groupModels[[g]],
                               size = input$gtna_cliques_size,
                               threshold = input$gtna_cliques_threshold)
        if(!is.null(cliques) && length(cliques) > 0) {
          plot(x = cliques, ask = FALSE, first = 1, n = 1, bg = "white")
          title(main = g, line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group TNA Bootstrap Table
  output$gtna_bootstrap_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_bootstrap_show_table)

    groupModels <- gtna_model()

    all_bs <- NULL
    for(g in names(groupModels)) {
      bs <- tryCatch({
        tna::bootstrap(
          x = groupModels[[g]],
          iter = input$gtna_bootstrap_iterations,
          level = input$gtna_bootstrap_level,
          method = input$gtna_bootstrap_method,
          threshold = if(input$gtna_bootstrap_method == "threshold") input$gtna_bootstrap_threshold else 0.1,
          consistency_range = c(input$gtna_bootstrap_range_low, input$gtna_bootstrap_range_up)
        )
      }, error = function(e) NULL)

      if(!is.null(bs) && !is.null(bs$summary)) {
        df <- bs$summary
        df$group <- g

        if(input$gtna_bootstrap_significant_only) {
          df <- df[df$sig == TRUE, ]
        }

        all_bs <- rbind(all_bs, df)
      }
    }

    if(is.null(all_bs) || nrow(all_bs) == 0) return(NULL)

    if(nrow(all_bs) > input$gtna_bootstrap_max_rows) {
      all_bs <- all_bs[1:input$gtna_bootstrap_max_rows, ]
    }

    styled_datatable(all_bs, caption = "Bootstrap Analysis Results")
  })

  # Group TNA Bootstrap Plot
  output$gtna_bootstrap_plot <- renderPlot({
    req(gtna_model(), input$gtna_bootstrap_show_plot)

    groupModels <- gtna_model()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        bs <- tna::bootstrap(
          x = groupModels[[g]],
          iter = input$gtna_bootstrap_iterations,
          level = input$gtna_bootstrap_level,
          method = input$gtna_bootstrap_method,
          threshold = if(input$gtna_bootstrap_method == "threshold") input$gtna_bootstrap_threshold else 0.1,
          consistency_range = c(input$gtna_bootstrap_range_low, input$gtna_bootstrap_range_up)
        )
        if(!is.null(bs)) {
          plot(x = bs, cut = 0.01)
          title(main = g, line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group TNA Compare Sequences Table
  output$gtna_compare_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_compare_show_table)

    groupModels <- gtna_model()

    compSeq <- tryCatch({
      sub_range <- input$gtna_compare_min_len:input$gtna_compare_max_len
      tna::compare_sequences(
        x = groupModels,
        sub = sub_range,
        min_freq = input$gtna_compare_min_freq,
        correction = input$gtna_compare_correction
      )
    }, error = function(e) NULL)

    if(is.null(compSeq) || nrow(compSeq) == 0) return(NULL)

    # Remove patterns containing * (NAs)
    compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

    # Sort by total frequency
    freqCols <- colnames(compSeq)[grep("^freq_", colnames(compSeq))]
    if(length(freqCols) > 0) {
      compSeq$total_freq <- rowSums(compSeq[, freqCols, drop=FALSE])
      compSeq <- compSeq[order(-compSeq$total_freq), ]
      compSeq$total_freq <- NULL
    }

    # Keep only top 20
    if(nrow(compSeq) > 20) {
      compSeq <- compSeq[1:20, ]
    }

    styled_datatable(compSeq, caption = "Sequence Comparison Across Groups")
  })

  # Group TNA Compare Sequences Plot
  output$gtna_compare_plot <- renderPlot({
    req(gtna_model(), input$gtna_compare_show_plot)

    groupModels <- gtna_model()

    compSeq <- tryCatch({
      sub_range <- input$gtna_compare_min_len:input$gtna_compare_max_len
      tna::compare_sequences(
        x = groupModels,
        sub = sub_range,
        min_freq = input$gtna_compare_min_freq,
        correction = input$gtna_compare_correction
      )
    }, error = function(e) NULL)

    if(is.null(compSeq)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No comparison data available", cex = 1, col = "gray50")
      return()
    }

    # Remove patterns containing *
    compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

    if(nrow(compSeq) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No valid patterns found", cex = 1, col = "gray50")
      return()
    }

    tryCatch({
      p <- plot(compSeq)
      p <- p + ggplot2::theme(
        text = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14)
      )
      print(p)
    }, error = function(e) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group TNA Sequence Indices Table
  output$gtna_indices_table <- DT::renderDataTable({
    req(gtna_model(), input$gtna_indices_show_table)
    req(shared_data$action_col, shared_data$actor_col)

    copyData <- gtna_data()

    # Prepare data
    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      actor = shared_data$actor_col
    )

    if(!is.null(shared_data$time_col) && shared_data$time_col != "") {
      args_prepare_data$time <- shared_data$time_col
    }

    if(!is.null(shared_data$order_col) && shared_data$order_col != "") {
      args_prepare_data$order <- shared_data$order_col
    }

    dataForIndices <- tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) NULL)

    if(is.null(dataForIndices)) return(NULL)

    seq_data <- dataForIndices$sequence_data
    seq_data <- as.data.frame(lapply(seq_data, as.character), stringsAsFactors = FALSE)

    indices <- tryCatch({
      codyna::sequence_indices(data = seq_data, omega = input$gtna_indices_omega)
    }, error = function(e) NULL)

    if(is.null(indices)) return(NULL)

    # Add actor and group info
    indices$sequence_id <- 1:nrow(indices)
    actor_names <- rownames(dataForIndices$sequence_data)
    indices$actor <- actor_names

    # Add group
    if(shared_data$has_group) {
      actor_group_map <- unique(copyData[, c(shared_data$actor_col, shared_data$group_col)])
      names(actor_group_map) <- c("actor", "group")
      indices$group <- actor_group_map$group[match(actor_names, actor_group_map$actor)]
    }

    # Limit rows
    if(nrow(indices) > input$gtna_indices_max_rows) {
      indices <- indices[1:input$gtna_indices_max_rows, ]
    }

    styled_datatable(indices, caption = "Sequence Indices")
  })

  # ==========================================================================
  # Cluster TNA Module
  # ==========================================================================

  # Reactive data for Cluster TNA (uses shared data)
  ctna_data <- reactive({
    req(shared_data$activity_data)
    req(shared_data$actor_col)  # Cluster TNA requires actor column
    shared_data$activity_data
  })

  # Cluster results (reactive value)
  ctna_clusters <- reactiveVal(NULL)
  ctna_prepared_data <- reactiveVal(NULL)

  # Run clustering when button is clicked
  observeEvent(input$ctna_run_clustering, {
    removeNotification(id = "ctna_settings_warning")
    req(shared_data$action_col, shared_data$actor_col)
    req(shared_data$action_col != "", shared_data$actor_col != "")

    copyData <- ctna_data()
    copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])
    copyData[[shared_data$actor_col]] <- as.character(copyData[[shared_data$actor_col]])

    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      actor = shared_data$actor_col
    )

    if (!is.null(shared_data$time_col) && nzchar(shared_data$time_col)) {
      args_prepare_data$time <- shared_data$time_col
    }

    if (!is.null(shared_data$order_col) && nzchar(shared_data$order_col)) {
      args_prepare_data$order <- shared_data$order_col
    }

    dataForTNA <- tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) {
      showNotification(paste("Error preparing data:", e$message), type = "error")
      NULL
    })

    if (is.null(dataForTNA)) {
      ctna_clusters(NULL)
      ctna_prepared_data(NULL)
      return()
    }

    # Store prepared data for later use
    ctna_prepared_data(dataForTNA)

    # Run clustering
    clusterResult <- tryCatch({
      tna::cluster_sequences(
        data = dataForTNA$sequence_data,
        k = input$ctna_k,
        dissimilarity = input$ctna_dissimilarity,
        method = input$ctna_method
      )
    }, error = function(e) {
      showNotification(paste("Error clustering:", e$message), type = "error")
      NULL
    })

    if (is.null(clusterResult)) {
      ctna_clusters(NULL)
      return()
    }

    # Build TNA models for each cluster
    scaling <- if (input$ctna_scaling == "noScaling") character(0L) else input$ctna_scaling

    # Get cluster assignments (it's an integer vector, not a data frame)
    cluster_vec <- clusterResult$assignments
    if (is.null(cluster_vec) || length(cluster_vec) == 0) {
      ctna_clusters(list(
        assignments = NULL,
        models = NULL,
        cluster_result = clusterResult
      ))
      return()
    }

    # Get actor names from the original sequence data
    seq_data <- dataForTNA$sequence_data
    actor_names <- rownames(seq_data)

    # Create assignments data frame
    assignments_df <- data.frame(
      actor = actor_names,
      cluster = cluster_vec,
      stringsAsFactors = FALSE
    )

    # Build models for each cluster
    cluster_ids <- unique(cluster_vec)
    models <- list()

    for (cid in cluster_ids) {
      # Get row indices for this cluster
      cluster_indices <- which(cluster_vec == cid)

      # Build model for this cluster subset
      model <- tryCatch({
        # Create subset of sequence data for this cluster
        cluster_data <- seq_data[cluster_indices, , drop = FALSE]

        if (nrow(cluster_data) > 1) {  # Need at least 2 sequences
          build_model(x = cluster_data, type = input$ctna_type, scaling = scaling)
        } else {
          NULL
        }
      }, error = function(e) {
        message("Error building model for cluster ", cid, ": ", e$message)
        NULL
      })

      if (!is.null(model)) {
        models[[as.character(cid)]] <- model
      }
    }

    # Store both assignments and models
    result <- list(
      assignments = assignments_df,
      models = if (length(models) > 0) models else NULL,
      cluster_result = clusterResult,
      silhouette = clusterResult$silhouette,
      sizes = clusterResult$sizes
    )

    ctna_clusters(result)
    showNotification(
      paste("Clustering complete:", length(cluster_ids), "clusters,", length(models), "models built"),
      type = "message"
    )
  })

  # Clear Cluster TNA analysis
  observeEvent(input$ctna_clear, {
    ctna_clusters(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Clustering'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$ctna_type, {
    if (!is.null(ctna_clusters())) {
      showNotification("Settings changed. Click 'Run Clustering' to apply.",
                       type = "warning", duration = NULL, id = "ctna_settings_warning")
    }
  }, ignoreInit = TRUE)

  observeEvent(input$ctna_scaling, {
    if (!is.null(ctna_clusters())) {
      showNotification("Settings changed. Click 'Run Clustering' to apply.",
                       type = "warning", duration = NULL, id = "ctna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # Cluster Assignment Table
  output$ctna_cluster_assignment <- DT::renderDataTable({
    if (is.null(ctna_clusters())) {
      return(styled_datatable(data.frame(Message = "Run clustering to see assignments"), caption = "Cluster Assignments"))
    }

    clusters <- ctna_clusters()

    if (is.null(clusters$assignments)) {
      return(styled_datatable(data.frame(Message = "No assignments available"), caption = "Cluster Assignments"))
    }

    styled_datatable(clusters$assignments, caption = "Cluster Assignments")
  })

  # Cluster TNA Summary Table
  output$ctna_summary_table <- DT::renderDataTable({
    clusters <- ctna_clusters()
    req(clusters)
    req(clusters$models)

    tryCatch({
      # Combine summaries from all clusters
      summaries <- lapply(names(clusters$models), function(clust) {
        s <- summary(clusters$models[[clust]])
        s$cluster <- clust
        s
      })
      combined <- do.call(rbind, summaries)
      combined <- combined[, c("cluster", "metric", "value")]

      DT::datatable(
        combined,
        options = list(
          dom = 't',
          paging = FALSE,
          searching = FALSE,
          ordering = FALSE,
          info = FALSE,
          scrollY = "400px",
          scrollCollapse = TRUE
        ),
        rownames = FALSE,
        colnames = c("Cluster", "Metric", "Value"),
        class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) {
      DT::datatable(data.frame(Message = "Run Cluster TNA analysis first"))
    })
  })

  # Cluster TNA Matrix Output
  output$ctna_matrix_output <- renderPrint({
    clusters <- ctna_clusters()
    req(clusters, clusters$models)
    for(clust in names(clusters$models)) {
      cat("=== Cluster:", clust, "===\n")
      print(round(clusters$models[[clust]]$weights, 4))
      cat("\n")
    }
  })

  # Cluster TNA Network Plot
  output$ctna_network_plot <- renderPlot({
    # Show message if no clustering done yet
    if (is.null(ctna_clusters())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Clustering' to generate clusters", cex = 1.2, col = "gray50")
      return()
    }

    if (!isTRUE(input$ctna_show_plot)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show Cluster Plots' to view", cex = 1, col = "gray50")
      return()
    }

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No models could be built.\nCheck your data and settings.", cex = 1, col = "#e74c3c")
      return()
    }

    tryCatch({
      groupModels <- clusters$models
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for (g in names(groupModels)) {
        plot(groupModels[[g]],
             title = paste("Cluster", g),
             cut = input$ctna_plot_cut,
             minimum = input$ctna_plot_min,
             edge.label.cex = input$ctna_edge_label_size,
             node.width = input$ctna_node_size,
             label.cex = input$ctna_node_label_size,
             layout = input$ctna_layout,
             bg = "white")
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # Cluster TNA Centrality Table
  output$ctna_centrality_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_centrality_show_table)) return(NULL)
    if (length(input$ctna_centrality_measures) == 0) return(NULL)

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      return(styled_datatable(data.frame(Message = "No models available"), caption = "Centrality Measures"))
    }

    groupModels <- clusters$models

    all_cent <- NULL
    for (g in names(groupModels)) {
      cent <- tryCatch({
        centralities(x = groupModels[[g]], measures = input$ctna_centrality_measures)
      }, error = function(e) NULL)

      if (!is.null(cent)) {
        cent$cluster <- g
        all_cent <- rbind(all_cent, cent)
      }
    }

    if (is.null(all_cent) || nrow(all_cent) == 0) {
      return(styled_datatable(data.frame(Message = "Could not compute centralities"), caption = "Centrality Measures"))
    }

    styled_datatable(all_cent, caption = "Cluster Centrality Measures")
  })

  # Cluster TNA Permutation Table
  output$ctna_permutation_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_permutation_show_table)) return(NULL)

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) < 2) {
      return(styled_datatable(data.frame(Message = "Need at least 2 clusters for permutation test"), caption = "Permutation Test"))
    }

    groupModels <- clusters$models

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$ctna_permutation_iter,
                           level = input$ctna_permutation_level)
    }, error = function(e) NULL)

    if (is.null(permResult) || is.null(permResult$summary)) {
      return(styled_datatable(data.frame(Message = "Permutation test failed"), caption = "Permutation Test"))
    }

    styled_datatable(permResult$summary, caption = "Permutation Test Results")
  })

  # Cluster TNA Permutation Plot
  output$ctna_permutation_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_permutation_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) < 2) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Need at least 2 clusters for permutation test", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$ctna_permutation_iter,
                           level = input$ctna_permutation_level)
    }, error = function(e) NULL)

    if (is.null(permResult)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Permutation test failed", cex = 1, col = "#e74c3c")
      return()
    }

    tryCatch({
      p <- plot(permResult)
      print(p)
    }, error = function(e) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Cluster TNA Sequences Plot
  output$ctna_sequences_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_sequences_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No models available", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    p <- tryCatch({
      tna::plot_sequences(
        x = groupModels,
        type = input$ctna_sequences_type,
        scale = input$ctna_sequences_scale,
        geom = input$ctna_sequences_geom,
        include_na = input$ctna_sequences_include_na,
        tick = input$ctna_sequences_tick
      )
    }, error = function(e) NULL)

    if(!is.null(p)) print(p)
  })

  # Cluster TNA Community Table
  output$ctna_community_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_community_show_table)) return(NULL)

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      return(styled_datatable(data.frame(Message = "No models available"), caption = "Community Assignments"))
    }

    groupModels <- clusters$models

    all_coms <- NULL
    for(g in names(groupModels)) {
      coms <- tryCatch({
        tna::communities(x = groupModels[[g]],
                        methods = input$ctna_community_method,
                        gamma = input$ctna_community_gamma)
      }, error = function(e) NULL)

      if(!is.null(coms) && !is.null(coms$assignments)) {
        df <- coms$assignments
        df$cluster <- g
        all_coms <- rbind(all_coms, df)
      }
    }

    if(is.null(all_coms)) return(NULL)
    styled_datatable(all_coms, caption = "Community Assignments by Cluster")
  })

  # Cluster TNA Community Plot
  output$ctna_community_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_community_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No models available", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        coms <- tna::communities(x = groupModels[[g]],
                                methods = input$ctna_community_method,
                                gamma = input$ctna_community_gamma)
        if(!is.null(coms)) {
          plot(x = coms, method = input$ctna_community_method, bg = "white")
          title(main = paste("Cluster", g), line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Cluster TNA Cliques Text
  output$ctna_cliques_text <- renderPrint({
    if (is.null(ctna_clusters())) {
      cat("Run clustering to see cliques\n")
      return()
    }
    if (!isTRUE(input$ctna_cliques_show_table)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      cat("No models available\n")
      return()
    }

    groupModels <- clusters$models

    for(g in names(groupModels)) {
      cat("=== Cluster:", g, "===\n")
      cliques <- tryCatch({
        tna::cliques(x = groupModels[[g]],
                    size = input$ctna_cliques_size,
                    threshold = input$ctna_cliques_threshold)
      }, error = function(e) NULL)

      if(!is.null(cliques)) {
        print(cliques)
      } else {
        cat("No cliques found\n")
      }
      cat("\n")
    }
  })

  # Cluster TNA Cliques Plot
  output$ctna_cliques_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_cliques_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No models available", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        cliques <- tna::cliques(x = groupModels[[g]],
                               size = input$ctna_cliques_size,
                               threshold = input$ctna_cliques_threshold)
        if(!is.null(cliques) && length(cliques) > 0) {
          plot(x = cliques, ask = FALSE, first = 1, n = 1, bg = "white")
          title(main = paste("Cluster", g), line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Cluster TNA Bootstrap Table
  output$ctna_bootstrap_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_bootstrap_show_table)) return(NULL)

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      return(styled_datatable(data.frame(Message = "No models available"), caption = "Bootstrap Analysis"))
    }

    groupModels <- clusters$models

    all_bs <- NULL
    for(g in names(groupModels)) {
      bs <- tryCatch({
        tna::bootstrap(
          x = groupModels[[g]],
          iter = input$ctna_bootstrap_iterations,
          level = input$ctna_bootstrap_level,
          method = input$ctna_bootstrap_method,
          threshold = if(input$ctna_bootstrap_method == "threshold") input$ctna_bootstrap_threshold else 0.1,
          consistency_range = c(input$ctna_bootstrap_range_low, input$ctna_bootstrap_range_up)
        )
      }, error = function(e) NULL)

      if(!is.null(bs) && !is.null(bs$summary)) {
        df <- bs$summary
        df$cluster <- g

        if(input$ctna_bootstrap_significant_only) {
          df <- df[df$sig == TRUE, ]
        }

        all_bs <- rbind(all_bs, df)
      }
    }

    if(is.null(all_bs) || nrow(all_bs) == 0) return(NULL)

    if(nrow(all_bs) > input$ctna_bootstrap_max_rows) {
      all_bs <- all_bs[1:input$ctna_bootstrap_max_rows, ]
    }

    styled_datatable(all_bs, caption = "Bootstrap Analysis Results")
  })

  # Cluster TNA Bootstrap Plot
  output$ctna_bootstrap_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_bootstrap_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No models available", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        bs <- tna::bootstrap(
          x = groupModels[[g]],
          iter = input$ctna_bootstrap_iterations,
          level = input$ctna_bootstrap_level,
          method = input$ctna_bootstrap_method,
          threshold = if(input$ctna_bootstrap_method == "threshold") input$ctna_bootstrap_threshold else 0.1,
          consistency_range = c(input$ctna_bootstrap_range_low, input$ctna_bootstrap_range_up)
        )
        if(!is.null(bs)) {
          plot(x = bs, cut = 0.01)
          title(main = paste("Cluster", g), line = 0.5)
        }
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Cluster TNA Compare Sequences Table
  output$ctna_compare_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_compare_show_table)) return(NULL)

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) < 2) {
      return(styled_datatable(data.frame(Message = "Need at least 2 clusters for comparison"), caption = "Sequence Comparison"))
    }

    groupModels <- clusters$models

    compSeq <- tryCatch({
      sub_range <- input$ctna_compare_min_len:input$ctna_compare_max_len
      tna::compare_sequences(
        x = groupModels,
        sub = sub_range,
        min_freq = input$ctna_compare_min_freq,
        correction = input$ctna_compare_correction
      )
    }, error = function(e) NULL)

    if(is.null(compSeq) || nrow(compSeq) == 0) return(NULL)

    # Remove patterns containing * (NAs)
    compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

    # Sort by total frequency
    freqCols <- colnames(compSeq)[grep("^freq_", colnames(compSeq))]
    if(length(freqCols) > 0) {
      compSeq$total_freq <- rowSums(compSeq[, freqCols, drop=FALSE])
      compSeq <- compSeq[order(-compSeq$total_freq), ]
      compSeq$total_freq <- NULL
    }

    # Keep only top 20
    if(nrow(compSeq) > 20) {
      compSeq <- compSeq[1:20, ]
    }

    styled_datatable(compSeq, caption = "Sequence Comparison Across Clusters")
  })

  # Cluster TNA Compare Sequences Plot
  output$ctna_compare_plot <- renderPlot({
    if (is.null(ctna_clusters())) return()
    if (!isTRUE(input$ctna_compare_show_plot)) return()

    clusters <- ctna_clusters()

    if (is.null(clusters$models) || length(clusters$models) < 2) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Need at least 2 clusters for comparison", cex = 1, col = "gray50")
      return()
    }

    groupModels <- clusters$models

    compSeq <- tryCatch({
      sub_range <- input$ctna_compare_min_len:input$ctna_compare_max_len
      tna::compare_sequences(
        x = groupModels,
        sub = sub_range,
        min_freq = input$ctna_compare_min_freq,
        correction = input$ctna_compare_correction
      )
    }, error = function(e) NULL)

    if(is.null(compSeq)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No comparison data available", cex = 1, col = "gray50")
      return()
    }

    # Remove patterns containing *
    compSeq <- compSeq[!grepl("\\*", compSeq$pattern), ]

    if(nrow(compSeq) == 0) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "No valid patterns found", cex = 1, col = "gray50")
      return()
    }

    tryCatch({
      p <- plot(compSeq)
      p <- p + ggplot2::theme(
        text = ggplot2::element_text(size = 14),
        axis.text = ggplot2::element_text(size = 12),
        axis.title = ggplot2::element_text(size = 14)
      )
      print(p)
    }, error = function(e) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Cluster TNA Sequence Indices Table
  output$ctna_indices_table <- DT::renderDataTable({
    if (is.null(ctna_clusters())) return(NULL)
    if (!isTRUE(input$ctna_indices_show_table)) return(NULL)
    if (is.null(ctna_prepared_data())) return(NULL)

    clusters <- ctna_clusters()
    dataForIndices <- ctna_prepared_data()

    if(is.null(dataForIndices)) return(NULL)

    seq_data <- dataForIndices$sequence_data
    seq_data <- as.data.frame(lapply(seq_data, as.character), stringsAsFactors = FALSE)

    indices <- tryCatch({
      codyna::sequence_indices(data = seq_data, omega = input$ctna_indices_omega)
    }, error = function(e) NULL)

    if(is.null(indices)) return(NULL)

    # Add actor and cluster info
    indices$sequence_id <- 1:nrow(indices)
    actor_names <- rownames(dataForIndices$sequence_data)
    indices$actor <- actor_names

    # Add cluster assignments
    if(!is.null(clusters$assignments)) {
      indices$cluster <- clusters$assignments$cluster[match(actor_names, clusters$assignments$actor)]
    }

    # Limit rows
    if(nrow(indices) > input$ctna_indices_max_rows) {
      indices <- indices[1:input$ctna_indices_max_rows, ]
    }

    styled_datatable(indices, caption = "Sequence Indices")
  })

  # ==========================================================================
  # Co-occurrence TNA Module
  # ==========================================================================

  # Reactive data for Co-occurrence TNA (uses shared data)
  cotna_data <- reactive({
    req(shared_data$activity_data)
    shared_data$activity_data
  })

  # Prepare Co-occurrence TNA data
  cotna_prepared_data <- reactive({
    req(shared_data$action_col)
    req(shared_data$action_col != "")

    copyData <- cotna_data()
    copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])

    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      time_threshold = input$cotna_threshold
    )

    if(!is.null(shared_data$actor_col) && shared_data$actor_col != "") {
      copyData[[shared_data$actor_col]] <- as.character(copyData[[shared_data$actor_col]])
      args_prepare_data$actor <- shared_data$actor_col
    }

    if(!is.null(shared_data$time_col) && shared_data$time_col != "") {
      copyData[[shared_data$time_col]] <- as.POSIXct(copyData[[shared_data$time_col]])
      args_prepare_data$time <- shared_data$time_col
    }

    if(!is.null(shared_data$order_col) && shared_data$order_col != "") {
      args_prepare_data$order <- shared_data$order_col
    }

    args_prepare_data$data <- copyData

    tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) NULL)
  })

  # Store computed Co-occurrence TNA model
  cotna_model <- reactiveVal(NULL)

  # Auto-run Co-occurrence TNA analysis when data first becomes available
  observe({
    req(cotna_prepared_data())
    # Only auto-run if model is NULL (first time)
    if (is.null(isolate(cotna_model()))) {
      dataForTNA <- cotna_prepared_data()
      scaling <- if(isolate(input$cotna_scaling) == "noScaling") character(0L) else isolate(input$cotna_scaling)

      result <- tryCatch({
        build_model(x = dataForTNA, type = "co-occurrence", scaling = scaling)
      }, error = function(e) NULL)

      if (!is.null(result)) {
        cotna_model(result)
      }
    }
  })

  # Run Co-occurrence TNA analysis when button is clicked
  observeEvent(input$cotna_run, {
    removeNotification(id = "cotna_settings_warning")
    req(cotna_prepared_data())

    dataForTNA <- cotna_prepared_data()
    scaling <- if(input$cotna_scaling == "noScaling") character(0L) else input$cotna_scaling

    result <- tryCatch({
      build_model(x = dataForTNA, type = "co-occurrence", scaling = scaling)
    }, error = function(e) {
      showNotification(paste("Error building model:", e$message), type = "error")
      NULL
    })

    if (!is.null(result)) {
      cotna_model(result)
      showNotification("Co-occurrence analysis complete", type = "message")
    }
  })

  # Clear Co-occurrence TNA analysis
  observeEvent(input$cotna_clear, {
    cotna_model(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Analysis'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$cotna_scaling, {
    if (!is.null(cotna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "cotna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # Co-occurrence TNA Network Plot
  output$cotna_network_plot <- renderPlot({
    # Show message if no analysis run yet
    if (is.null(cotna_model())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Analysis' to build the network", cex = 1.2, col = "gray50")
      return()
    }

    if (!input$cotna_show_plot) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show Plot' to view", cex = 1, col = "gray50")
      return()
    }

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      plot(x = cotna_model(),
           cut = input$cotna_plot_cut,
           minimum = input$cotna_plot_min,
           edge.label.cex = input$cotna_edge_label_size,
           node.width = input$cotna_node_size,
           label.cex = input$cotna_node_label_size,
           layout = input$cotna_layout,
           bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # Co-occurrence TNA Matrix Output
  output$cotna_matrix_output <- renderPrint({
    req(cotna_model(), input$cotna_show_matrix)
    model <- cotna_model()
    round(model$weights, 4)
  })

  # Co-occurrence TNA Summary Table
  output$cotna_summary_table <- DT::renderDataTable({
    model <- cotna_model()
    req(model)

    tryCatch({
      summary_df <- summary(model)
      DT::datatable(
        summary_df,
        options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE),
        rownames = FALSE, colnames = c("Metric", "Value"), class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) DT::datatable(data.frame(Metric = "Error", Value = as.character(e$message))))
  })

  # Co-occurrence TNA Histogram
  output$cotna_histogram_plot <- renderPlot({
    req(cotna_model(), input$cotna_show_histogram)
    par(mar = c(3, 3, 2, 1), oma = c(0, 0, 0, 0))
    tryCatch({
      hist(x = cotna_model(), main = "Histogram of Co-occurrence Weights",
           xlab = "Weights", ylab = "Frequency")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # Co-occurrence TNA Frequencies Plot
  output$cotna_frequencies_plot <- renderPlot({
    req(cotna_model(), input$cotna_show_frequencies)
    tryCatch({
      p <- plot_frequencies(x = cotna_model())
      print(p)
    }, error = function(e) {
      hist(x = cotna_model(), main = "Frequencies Plot",
           xlab = "Weights", ylab = "Frequency")
    })
  })

  # Co-occurrence TNA Centrality Table
  output$cotna_centrality_table <- DT::renderDataTable({
    req(cotna_model(), input$cotna_centrality_show_table)
    req(length(input$cotna_centrality_measures) > 0)

    cent <- centralities(x = cotna_model(),
                        loops = input$cotna_centrality_loops,
                        normalize = input$cotna_centrality_normalize,
                        measures = input$cotna_centrality_measures)

    styled_datatable(cent, caption = "Centrality Measures")
  })

  # Co-occurrence TNA Centrality Plot
  output$cotna_centrality_plot <- renderPlot({
    req(cotna_model(), input$cotna_centrality_show_plot)
    req(length(input$cotna_centrality_measures) > 0)

    cent <- centralities(x = cotna_model(),
                        loops = input$cotna_centrality_loops,
                        normalize = input$cotna_centrality_normalize,
                        measures = input$cotna_centrality_measures)

    centPlot <- plot(cent)
    print(centPlot)
  })

  # Co-occurrence TNA Community Table
  output$cotna_community_table <- DT::renderDataTable({
    req(cotna_model(), input$cotna_community_show_table)

    coms <- tryCatch({
      tna::communities(x = cotna_model(),
                      methods = input$cotna_community_method,
                      gamma = input$cotna_community_gamma)
    }, error = function(e) NULL)

    if(is.null(coms)) return(NULL)

    styled_datatable(coms$assignments, caption = "Community Assignments")
  })

  # Co-occurrence TNA Community Plot
  output$cotna_community_plot <- renderPlot({
    req(cotna_model(), input$cotna_community_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      coms <- tna::communities(x = cotna_model(),
                      methods = input$cotna_community_method,
                      gamma = input$cotna_community_gamma)

      if(is.null(coms)) return()

      plot(x = coms, method = input$cotna_community_method, bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # Co-occurrence TNA Bootstrap Table
  output$cotna_bootstrap_table <- DT::renderDataTable({
    req(cotna_model(), input$cotna_bootstrap_show_table)

    bs <- tryCatch({
      bootstrap(
        x = cotna_model(),
        iter = input$cotna_bootstrap_iterations,
        level = input$cotna_bootstrap_level
      )
    }, error = function(e) NULL)

    if(is.null(bs) || is.null(bs$summary)) return(NULL)

    styled_datatable(bs$summary, caption = "Bootstrap Analysis Results")
  })

  # Co-occurrence TNA Bootstrap Plot
  output$cotna_bootstrap_plot <- renderPlot({
    req(cotna_model(), input$cotna_bootstrap_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      bs <- bootstrap(
        x = cotna_model(),
        iter = input$cotna_bootstrap_iterations,
        level = input$cotna_bootstrap_level
      )

      if(is.null(bs)) return()

      plot(x = bs, cut = 0.01)
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # ==========================================================================
  # Group Co-occurrence TNA Module
  # ==========================================================================

  # Group Co-occurrence data indicator
  output$gcotna_data_indicator <- renderUI({
    if (is.null(shared_data$activity_data)) {
      div(class = "alert alert-warning alert-sm py-2", icon("exclamation-triangle"), " Load data in Data tab")
    } else if (is.null(shared_data$actor_col)) {
      div(class = "alert alert-warning alert-sm py-2", icon("user"), " Select an Actor column in Data tab")
    } else if (!shared_data$has_group) {
      div(class = "alert alert-warning alert-sm py-2", icon("users"), " Select a Group column in Data tab")
    } else {
      div(class = "alert alert-success alert-sm py-2",
          icon("check-circle"), sprintf(" Ready: %d rows (Group: %s)", nrow(shared_data$activity_data), shared_data$group_col))
    }
  })

  # Store computed Group Co-occurrence model
  gcotna_model <- reactiveVal(NULL)

  # Run Group Co-occurrence when button is clicked
  observeEvent(input$gcotna_run, {
    removeNotification(id = "gcotna_settings_warning")
    req(shared_data$action_col, shared_data$actor_col)
    req(shared_data$action_col != "", shared_data$actor_col != "")

    # Check if group column is available
    if (!shared_data$has_group) {
      showNotification("Please select a Group column in the Data tab", type = "warning")
      gcotna_model(NULL)
      return()
    }

    copyData <- shared_data$activity_data
    copyData[[shared_data$action_col]] <- as.character(copyData[[shared_data$action_col]])
    copyData[[shared_data$actor_col]] <- as.character(copyData[[shared_data$actor_col]])

    args_prepare_data <- list(
      data = copyData,
      action = shared_data$action_col,
      actor = shared_data$actor_col,
      time_threshold = input$gcotna_threshold
    )

    if(!is.null(shared_data$time_col) && shared_data$time_col != "") {
      copyData[[shared_data$time_col]] <- as.POSIXct(copyData[[shared_data$time_col]])
      args_prepare_data$time <- shared_data$time_col
    }

    if(!is.null(shared_data$order_col) && shared_data$order_col != "") {
      args_prepare_data$order <- shared_data$order_col
    }

    args_prepare_data$data <- copyData

    dataForTNA <- tryCatch({
      do.call(prepare_data, args_prepare_data)
    }, error = function(e) {
      showNotification(paste("Error preparing data:", e$message), type = "error")
      NULL
    })

    if(is.null(dataForTNA)) {
      gcotna_model(NULL)
      return()
    }

    scaling <- if(input$gcotna_scaling == "noScaling") character(0L) else input$gcotna_scaling

    # Get group from long data
    group <- dataForTNA$long_data[!duplicated(dataForTNA$long_data$.session_id),]
    group_vec <- group[[shared_data$group_col]]

    result <- tryCatch({
      tna::group_model(x = dataForTNA, group = group_vec, type = "co-occurrence", scaling = scaling)
    }, error = function(e) {
      showNotification(paste("Error building model:", e$message), type = "error")
      NULL
    })

    if (!is.null(result)) {
      gcotna_model(result)
      showNotification(paste("Group co-occurrence analysis complete:", length(result), "groups"), type = "message")
    } else {
      gcotna_model(NULL)
    }
  })

  # Clear Group Co-occurrence TNA analysis
  observeEvent(input$gcotna_clear, {
    gcotna_model(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Analysis'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$gcotna_scaling, {
    if (!is.null(gcotna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "gcotna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # Group Co-occurrence Network Plot
  output$gcotna_network_plot <- renderPlot({
    if (is.null(shared_data$activity_data)) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Load data in the Data tab to begin", cex = 1.2, col = "gray50")
      return()
    }

    if (!shared_data$has_group) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Select a Group column in the Data tab,\nthen click 'Run Group Analysis'", cex = 1.2, col = "gray50")
      return()
    }

    if (is.null(gcotna_model())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Group Analysis' to begin", cex = 1.2, col = "gray50")
      return()
    }

    if (!input$gcotna_show_plot) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show Plot' to view", cex = 1, col = "gray50")
      return()
    }

    groupModels <- gcotna_model()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))

      for(g in names(groupModels)) {
        plot(groupModels[[g]],
             title = g,
             cut = input$gcotna_plot_cut,
             minimum = input$gcotna_plot_min,
             edge.label.cex = input$gcotna_edge_label_size,
             node.width = input$gcotna_node_size,
             label.cex = input$gcotna_node_label_size,
             layout = input$gcotna_layout,
             bg = "white")
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Matrix Output
  output$gcotna_matrix_output <- renderPrint({
    req(gcotna_model(), input$gcotna_show_matrix)

    groupModels <- gcotna_model()

    for(g in names(groupModels)) {
      cat("=== Group:", g, "===\n")
      print(round(groupModels[[g]]$weights, 4))
      cat("\n")
    }
  })

  # Group Co-occurrence Summary Table
  output$gcotna_summary_table <- DT::renderDataTable({
    model <- gcotna_model()
    req(model)

    tryCatch({
      summaries <- lapply(names(model), function(grp) {
        s <- summary(model[[grp]])
        s$group <- grp
        s
      })
      combined <- do.call(rbind, summaries)
      combined <- combined[, c("group", "metric", "value")]

      DT::datatable(
        combined,
        options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE,
                       scrollY = "300px", scrollCollapse = TRUE),
        rownames = FALSE, colnames = c("Group", "Metric", "Value"), class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) DT::datatable(data.frame(Message = "Run analysis first")))
  })

  # Group Co-occurrence Histogram
  output$gcotna_histogram_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_show_histogram)

    groupModels <- gcotna_model()

    tryCatch({
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)

      par(mfrow = c(nRows, nCols), mar = c(3, 3, 2, 1))

      for(g in names(groupModels)) {
        hist(x = groupModels[[g]], main = paste("Histogram -", g),
             xlab = "Edge Weights", ylab = "Frequency")
      }

      par(mfrow = c(1, 1))
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Frequencies Plot
  output$gcotna_frequencies_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_show_frequencies)

    groupModels <- gcotna_model()

    tryCatch({
      p <- tna::plot_frequencies(x = groupModels)
      if(!is.null(p)) print(p)
    }, error = function(e) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Centrality Table
  output$gcotna_centrality_table <- DT::renderDataTable({
    req(gcotna_model(), input$gcotna_centrality_show_table)
    req(length(input$gcotna_centrality_measures) > 0)

    groupModels <- gcotna_model()

    cent <- tryCatch({
      centralities(x = groupModels,
                  loops = input$gcotna_centrality_loops,
                  normalize = input$gcotna_centrality_normalize,
                  measures = input$gcotna_centrality_measures)
    }, error = function(e) NULL)

    if(is.null(cent)) return(NULL)

    styled_datatable(cent, caption = "Group Centrality Measures")
  })

  # Group Co-occurrence Centrality Plot
  output$gcotna_centrality_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_centrality_show_plot)
    req(length(input$gcotna_centrality_measures) > 0)

    groupModels <- gcotna_model()

    cent <- tryCatch({
      centralities(x = groupModels,
                  loops = input$gcotna_centrality_loops,
                  normalize = input$gcotna_centrality_normalize,
                  measures = input$gcotna_centrality_measures)
    }, error = function(e) NULL)

    if(!is.null(cent)) {
      p <- plot(cent)
      print(p)
    }
  })

  # Group Co-occurrence Community Table
  output$gcotna_community_table <- DT::renderDataTable({
    req(gcotna_model(), input$gcotna_community_show_table)

    groupModels <- gcotna_model()

    coms <- tryCatch({
      tna::communities(x = groupModels,
                      methods = input$gcotna_community_method,
                      gamma = input$gcotna_community_gamma)
    }, error = function(e) NULL)

    if(is.null(coms) || is.null(coms$assignments)) return(NULL)

    styled_datatable(coms$assignments, caption = "Community Assignments by Group")
  })

  # Group Co-occurrence Community Plot
  output$gcotna_community_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_community_show_plot)

    groupModels <- gcotna_model()

    tryCatch({
      coms <- tna::communities(x = groupModels,
                              methods = input$gcotna_community_method,
                              gamma = input$gcotna_community_gamma)

      if(!is.null(coms)) {
        nGroups <- length(groupModels)
        nCols <- ceiling(sqrt(nGroups))
        nRows <- ceiling(nGroups / nCols)

        par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))
        plot(x = coms, method = input$gcotna_community_method)
        par(mfrow = c(1, 1))
      }
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Cliques Text
  output$gcotna_cliques_text <- renderPrint({
    req(gcotna_model(), input$gcotna_cliques_show_table)

    groupModels <- gcotna_model()

    cliques <- tryCatch({
      tna::cliques(x = groupModels,
                  size = input$gcotna_cliques_size,
                  threshold = input$gcotna_cliques_threshold)
    }, error = function(e) NULL)

    if(!is.null(cliques)) {
      print(cliques)
    } else {
      cat("No cliques found\n")
    }
  })

  # Group Co-occurrence Cliques Plot
  output$gcotna_cliques_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_cliques_show_plot)

    groupModels <- gcotna_model()

    tryCatch({
      cliques <- tna::cliques(x = groupModels,
                             size = input$gcotna_cliques_size,
                             threshold = input$gcotna_cliques_threshold)

      if(!is.null(cliques) && length(cliques) > 0) {
        nGroups <- length(groupModels)
        nCols <- ceiling(sqrt(nGroups))
        nRows <- ceiling(nGroups / nCols)

        par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))
        plot(x = cliques, ask = FALSE, first = 1, n = 1, bg = "white")
        par(mfrow = c(1, 1))
      }
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Bootstrap Table
  output$gcotna_bootstrap_table <- DT::renderDataTable({
    req(gcotna_model(), input$gcotna_bootstrap_show_table)

    groupModels <- gcotna_model()

    bs <- tryCatch({
      tna::bootstrap(
        x = groupModels,
        iter = input$gcotna_bootstrap_iterations,
        level = input$gcotna_bootstrap_level,
        method = input$gcotna_bootstrap_method,
        threshold = if(input$gcotna_bootstrap_method == "threshold") input$gcotna_bootstrap_threshold else 0.1,
        consistency_range = c(input$gcotna_bootstrap_range_low, input$gcotna_bootstrap_range_up)
      )
    }, error = function(e) NULL)

    if(is.null(bs)) return(NULL)

    # Combine results from all groups
    all_bs <- NULL
    for(g in names(bs)) {
      if(!is.null(bs[[g]]$summary)) {
        df <- bs[[g]]$summary
        df$group <- g

        if(input$gcotna_bootstrap_significant_only) {
          df <- df[df$sig == TRUE, ]
        }

        all_bs <- rbind(all_bs, df)
      }
    }

    if(is.null(all_bs) || nrow(all_bs) == 0) return(NULL)

    if(nrow(all_bs) > input$gcotna_bootstrap_max_rows) {
      all_bs <- all_bs[1:input$gcotna_bootstrap_max_rows, ]
    }

    styled_datatable(all_bs, caption = "Bootstrap Analysis Results")
  })

  # Group Co-occurrence Bootstrap Plot
  output$gcotna_bootstrap_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_bootstrap_show_plot)

    groupModels <- gcotna_model()

    tryCatch({
      bs <- tna::bootstrap(
        x = groupModels,
        iter = input$gcotna_bootstrap_iterations,
        level = input$gcotna_bootstrap_level,
        method = input$gcotna_bootstrap_method,
        threshold = if(input$gcotna_bootstrap_method == "threshold") input$gcotna_bootstrap_threshold else 0.1,
        consistency_range = c(input$gcotna_bootstrap_range_low, input$gcotna_bootstrap_range_up)
      )

      if(!is.null(bs)) {
        nGroups <- length(groupModels)
        nCols <- ceiling(sqrt(nGroups))
        nRows <- ceiling(nGroups / nCols)

        par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5), oma = c(0, 0, 0, 0))
        plot(x = bs, cut = 0.01)
        par(mfrow = c(1, 1))
      }
    }, error = function(e) {
      par(mfrow = c(1, 1), mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
    })
  })

  # Group Co-occurrence Permutation Table
  output$gcotna_permutation_table <- DT::renderDataTable({
    req(gcotna_model(), input$gcotna_permutation_show_table)

    groupModels <- gcotna_model()

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$gcotna_permutation_iter,
                           paired = input$gcotna_permutation_paired,
                           level = input$gcotna_permutation_level)
    }, error = function(e) NULL)

    if(is.null(permResult)) return(NULL)

    styled_datatable(permResult$summary, caption = "Permutation Test Results")
  })

  # Group Co-occurrence Permutation Plot
  output$gcotna_permutation_plot <- renderPlot({
    req(gcotna_model(), input$gcotna_permutation_show_plot)

    groupModels <- gcotna_model()

    permResult <- tryCatch({
      tna::permutation_test(x = groupModels,
                           iter = input$gcotna_permutation_iter,
                           paired = input$gcotna_permutation_paired,
                           level = input$gcotna_permutation_level)
    }, error = function(e) NULL)

    if(!is.null(permResult)) {
      p <- plot(permResult)
      print(p)
    }
  })

  # ==========================================================================
  # One-Hot TNA Module
  # ==========================================================================

  # Reactive data for One-Hot TNA (uses shared data)
  ohtna_data <- reactive({
    req(shared_data$activity_data)
    shared_data$activity_data
  })

  # Update column choices when data is loaded - One-Hot needs its own column selectors
  observe({
    req(ohtna_data())
    col_names <- names(ohtna_data())
    cols <- c("(None)" = "", col_names)

    # Smart matching for One-Hot TNA fields (no auto-select for one-hot columns)
    actor_match <- find_matching_column(col_names, c("actor", "user", "student", "participant", "id", "subject"))
    session_match <- find_matching_column(col_names, c("session", "session_id", "sessionid"))
    group_match <- find_matching_column(col_names, c("group", "cluster", "category", "condition", "class"))

    updateSelectizeInput(session, "ohtna_onehot_cols", choices = col_names, selected = character(0))
    updateSelectInput(session, "ohtna_actor", choices = cols, selected = actor_match)
    updateSelectInput(session, "ohtna_session", choices = cols, selected = session_match)
    updateSelectInput(session, "ohtna_group", choices = cols, selected = group_match)
  })

  # Store computed One-Hot TNA model
  ohtna_model <- reactiveVal(NULL)

  # Run One-Hot TNA analysis when button is clicked
  observeEvent(input$ohtna_run, {
    removeNotification(id = "ohtna_settings_warning")
    req(input$ohtna_onehot_cols)
    req(length(input$ohtna_onehot_cols) >= 2)

    copyData <- ohtna_data()

    # Get the one-hot columns
    onehot_data <- copyData[, input$ohtna_onehot_cols, drop = FALSE]

    scaling <- if(input$ohtna_scaling == "noScaling") character(0L) else input$ohtna_scaling

    # Check for group column
    hasGroup <- !is.null(input$ohtna_group) && input$ohtna_group != ""

    result <- tryCatch({
      if(hasGroup) {
        # Build group models
        group_col <- copyData[[input$ohtna_group]]
        groups <- unique(group_col)
        groupModels <- list()

        for(g in groups) {
          group_data <- onehot_data[group_col == g, , drop = FALSE]
          # Build co-occurrence model from one-hot data
          model <- tna::build_model_from_onehot(
            x = group_data,
            window = input$ohtna_window,
            scaling = scaling
          )
          groupModels[[as.character(g)]] <- model
        }

        list(type = "group", models = groupModels)
      } else {
        # Build single model
        model <- tna::build_model_from_onehot(
          x = onehot_data,
          window = input$ohtna_window,
          scaling = scaling
        )
        list(type = "single", model = model)
      }
    }, error = function(e) {
      # Fallback: manual co-occurrence calculation
      tryCatch({
        # Calculate co-occurrence matrix manually
        n_actions <- ncol(onehot_data)
        action_names <- names(onehot_data)
        cooc_matrix <- matrix(0, nrow = n_actions, ncol = n_actions)
        rownames(cooc_matrix) <- action_names
        colnames(cooc_matrix) <- action_names

        for(i in 1:(nrow(onehot_data) - input$ohtna_window + 1)) {
          window_data <- onehot_data[i:(i + input$ohtna_window - 1), , drop = FALSE]
          active_actions <- which(colSums(window_data) > 0)
          if(length(active_actions) >= 2) {
            for(a in active_actions) {
              for(b in active_actions) {
                if(a != b) {
                  cooc_matrix[a, b] <- cooc_matrix[a, b] + 1
                }
              }
            }
          }
        }

        # Normalize if needed
        if(length(scaling) > 0 && scaling == "max") {
          max_val <- max(cooc_matrix)
          if(max_val > 0) cooc_matrix <- cooc_matrix / max_val
        }

        # Create a simple model-like structure
        model <- list(
          weights = cooc_matrix,
          labels = action_names
        )
        class(model) <- c("tna", "list")

        list(type = "single", model = model)
      }, error = function(e2) NULL)
    })

    if (!is.null(result)) {
      ohtna_model(result)
      showNotification("One-Hot analysis complete", type = "message")
    }
  })

  # Clear One-Hot TNA analysis
  observeEvent(input$ohtna_clear, {
    ohtna_model(NULL)
    showNotification("Analysis cleared. Change settings and click 'Run Analysis'.", type = "message")
  })

  # Notify user when settings change and analysis needs to be re-run
  observeEvent(input$ohtna_scaling, {
    if (!is.null(ohtna_model())) {
      showNotification("Settings changed. Click 'Run Analysis' to apply.",
                       type = "warning", duration = NULL, id = "ohtna_settings_warning")
    }
  }, ignoreInit = TRUE)

  # One-Hot TNA Network Plot
  output$ohtna_network_plot <- renderPlot({
    # Show message if no analysis run yet
    if (is.null(ohtna_model())) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Click 'Run Analysis' to build the network", cex = 1.2, col = "gray50")
      return()
    }

    if (!input$ohtna_show_plot) {
      par(mar = c(0, 0, 0, 0))
      plot.new()
      text(0.5, 0.5, "Enable 'Show Plot' to view", cex = 1, col = "gray50")
      return()
    }

    result <- ohtna_model()

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    tryCatch({
      if(result$type == "group") {
        groupModels <- result$models
        nGroups <- length(groupModels)
        nCols <- ceiling(sqrt(nGroups))
        nRows <- ceiling(nGroups / nCols)

        par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1, 0.5))

        for(g in names(groupModels)) {
          plot(groupModels[[g]],
               title = g,
               cut = input$ohtna_plot_cut,
               minimum = input$ohtna_plot_min,
               edge.label.cex = input$ohtna_edge_label_size,
               node.width = input$ohtna_node_size,
               label.cex = input$ohtna_node_label_size,
               layout = input$ohtna_layout,
               bg = "white")
        }

        par(mfrow = c(1, 1))
      } else {
        plot(result$model,
             cut = input$ohtna_plot_cut,
             minimum = input$ohtna_plot_min,
             edge.label.cex = input$ohtna_edge_label_size,
             node.width = input$ohtna_node_size,
             label.cex = input$ohtna_node_label_size,
             layout = input$ohtna_layout,
             bg = "white")
      }
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # One-Hot TNA Matrix Output
  output$ohtna_matrix_output <- renderPrint({
    req(ohtna_model(), input$ohtna_show_matrix)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model
    round(model$weights, 4)
  })

  # One-Hot TNA Summary Table
  output$ohtna_summary_table <- DT::renderDataTable({
    result <- ohtna_model()
    req(result)

    tryCatch({
      model <- if(result$type == "group") result$models[[1]] else result$model
      summary_df <- summary(model)
      DT::datatable(
        summary_df,
        options = list(dom = 't', paging = FALSE, searching = FALSE, ordering = FALSE, info = FALSE),
        rownames = FALSE, colnames = c("Metric", "Value"), class = 'compact stripe'
      ) %>% DT::formatRound('value', digits = 4)
    }, error = function(e) DT::datatable(data.frame(Metric = "Error", Value = as.character(e$message))))
  })

  # One-Hot TNA Histogram
  output$ohtna_histogram_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_show_histogram)

    par(mar = c(3, 3, 2, 1), oma = c(0, 0, 0, 0))

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      hist(x = model, main = "Histogram of Weights",
           xlab = "Weights", ylab = "Frequency")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # One-Hot TNA Frequencies Plot
  output$ohtna_frequencies_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_show_frequencies)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      p <- plot_frequencies(x = model)
      print(p)
    }, error = function(e) {
      hist(x = model, main = "Frequencies Plot",
           xlab = "Weights", ylab = "Frequency")
    })
  })

  # One-Hot TNA Mosaic Plot
  output$ohtna_mosaic_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_show_mosaic)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      p <- plot_mosaic(x = model)
      print(p)
    }, error = function(e) {
      plot.new()
      text(0.5, 0.5, "Mosaic plot not available", col = "gray")
    })
  })

  # One-Hot TNA Centrality Table
  output$ohtna_centrality_table <- DT::renderDataTable({
    req(ohtna_model(), input$ohtna_centrality_show_table)
    req(length(input$ohtna_centrality_measures) > 0)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    cent <- centralities(x = model,
                        loops = input$ohtna_centrality_loops,
                        normalize = input$ohtna_centrality_normalize,
                        measures = input$ohtna_centrality_measures)

    styled_datatable(cent, caption = "Centrality Measures")
  })

  # One-Hot TNA Centrality Plot
  output$ohtna_centrality_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_centrality_show_plot)
    req(length(input$ohtna_centrality_measures) > 0)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    cent <- centralities(x = model,
                        loops = input$ohtna_centrality_loops,
                        normalize = input$ohtna_centrality_normalize,
                        measures = input$ohtna_centrality_measures)

    centPlot <- plot(cent)
    print(centPlot)
  })

  # One-Hot TNA Community Table
  output$ohtna_community_table <- DT::renderDataTable({
    req(ohtna_model(), input$ohtna_community_show_table)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    coms <- tryCatch({
      tna::communities(x = model,
                      methods = input$ohtna_community_method,
                      gamma = input$ohtna_community_gamma)
    }, error = function(e) NULL)

    if(is.null(coms)) return(NULL)

    styled_datatable(coms$assignments, caption = "Community Assignments")
  })

  # One-Hot TNA Community Plot
  output$ohtna_community_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_community_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      coms <- tna::communities(x = model,
                      methods = input$ohtna_community_method,
                      gamma = input$ohtna_community_gamma)

      if(is.null(coms)) return()

      plot(x = coms, method = input$ohtna_community_method, bg = "white")
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # One-Hot TNA Bootstrap Table
  output$ohtna_bootstrap_table <- DT::renderDataTable({
    req(ohtna_model(), input$ohtna_bootstrap_show_table)

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    bs <- tryCatch({
      bootstrap(
        x = model,
        iter = input$ohtna_bootstrap_iterations,
        level = input$ohtna_bootstrap_level
      )
    }, error = function(e) NULL)

    if(is.null(bs) || is.null(bs$summary)) return(NULL)

    styled_datatable(bs$summary, caption = "Bootstrap Analysis Results")
  })

  # One-Hot TNA Bootstrap Plot
  output$ohtna_bootstrap_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_bootstrap_show_plot)

    par(mar = c(0.5, 0.5, 1, 0.5), oma = c(0, 0, 0, 0))

    result <- ohtna_model()
    model <- if(result$type == "group") result$models[[1]] else result$model

    tryCatch({
      bs <- bootstrap(
        x = model,
        iter = input$ohtna_bootstrap_iterations,
        level = input$ohtna_bootstrap_level
      )

      if(is.null(bs)) return()

      plot(x = bs, cut = 0.01)
    }, error = function(e) {
      if (grepl("margin|figure", tolower(e$message))) {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, "Please expand the\nbrowser window", cex = 1, col = "gray50")
      } else {
        par(mar = c(0, 0, 0, 0))
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), col = "red", cex = 0.7)
      }
    })
  })

  # One-Hot TNA Permutation Table
  output$ohtna_permutation_table <- DT::renderDataTable({
    req(ohtna_model(), input$ohtna_permutation_show_table)

    result <- ohtna_model()

    if(result$type != "group" || length(result$models) < 2) return(NULL)

    permResult <- tryCatch({
      tna::permutation_test(x = result$models,
                           iter = input$ohtna_permutation_iter,
                           level = input$ohtna_permutation_level)
    }, error = function(e) NULL)

    if(is.null(permResult)) return(NULL)

    styled_datatable(permResult$summary, caption = "Permutation Test Results")
  })

  # One-Hot TNA Permutation Plot
  output$ohtna_permutation_plot <- renderPlot({
    req(ohtna_model(), input$ohtna_permutation_show_plot)

    result <- ohtna_model()

    if(result$type != "group" || length(result$models) < 2) return()

    permResult <- tryCatch({
      tna::permutation_test(x = result$models,
                           iter = input$ohtna_permutation_iter,
                           level = input$ohtna_permutation_level)
    }, error = function(e) NULL)

    if(is.null(permResult)) return()

    p <- plot(permResult)
    print(p)
  })

  # ==========================================================================
  # Plot Download Handlers
  # ==========================================================================

  # Helper function to create plot download handler
  create_plot_download <- function(plot_func, filename_prefix, width = 10, height = 8) {
    list(
      png = downloadHandler(
        filename = function() paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
        content = function(file) {
          png(file, width = width * 100, height = height * 100, res = 100)
          tryCatch({
            plot_func()
          }, error = function(e) {
            plot.new()
            text(0.5, 0.5, "Error generating plot", col = "red")
          })
          dev.off()
        }
      ),
      pdf = downloadHandler(
        filename = function() paste0(filename_prefix, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
        content = function(file) {
          pdf(file, width = width, height = height)
          tryCatch({
            plot_func()
          }, error = function(e) {
            plot.new()
            text(0.5, 0.5, "Error generating plot", col = "red")
          })
          dev.off()
        }
      )
    )
  }

  # SNA Plot Downloads
  output$sna_plot_png <- downloadHandler(
    filename = function() paste0("sna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(sna_model())
      png(file, width = input$sna_plot_width, height = input$sna_plot_height, res = 100)
      result <- sna_model()
      tryCatch({
        if(result$type == "group") {
          groupModels <- result$models
          nGroups <- length(groupModels)
          nCols <- ceiling(sqrt(nGroups))
          nRows <- ceiling(nGroups / nCols)
          par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
          for(g in names(groupModels)) {
            plot(groupModels[[g]], title = g, cut = input$sna_plot_cut, minimum = input$sna_plot_min,
                 layout = input$sna_layout, vsize = input$sna_node_size, esize = input$sna_edge_size,
                 labels = input$sna_show_node_labels, edge.labels = input$sna_show_edge_labels,
                 theme = input$sna_theme, bg = "white")
          }
        } else {
          par(mar = c(0.5, 0.5, 1, 0.5))
          plot(result$model, cut = input$sna_plot_cut, minimum = input$sna_plot_min,
               layout = input$sna_layout, vsize = input$sna_node_size, esize = input$sna_edge_size,
               labels = input$sna_show_node_labels, edge.labels = input$sna_show_edge_labels,
               theme = input$sna_theme, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$sna_plot_pdf <- downloadHandler(
    filename = function() paste0("sna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(sna_model())
      pdf(file, width = input$sna_plot_width/100, height = input$sna_plot_height/100)
      result <- sna_model()
      tryCatch({
        if(result$type == "group") {
          groupModels <- result$models
          nGroups <- length(groupModels)
          nCols <- ceiling(sqrt(nGroups))
          nRows <- ceiling(nGroups / nCols)
          par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
          for(g in names(groupModels)) {
            plot(groupModels[[g]], title = g, cut = input$sna_plot_cut, minimum = input$sna_plot_min,
                 layout = input$sna_layout, vsize = input$sna_node_size, esize = input$sna_edge_size,
                 labels = input$sna_show_node_labels, edge.labels = input$sna_show_edge_labels,
                 theme = input$sna_theme, bg = "white")
          }
        } else {
          par(mar = c(0.5, 0.5, 1, 0.5))
          plot(result$model, cut = input$sna_plot_cut, minimum = input$sna_plot_min,
               layout = input$sna_layout, vsize = input$sna_node_size, esize = input$sna_edge_size,
               labels = input$sna_show_node_labels, edge.labels = input$sna_show_edge_labels,
               theme = input$sna_theme, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Plot Downloads
  output$tna_plot_png <- downloadHandler(
    filename = function() paste0("tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 800, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        plot(x = tna_model(), cut = input$tna_plot_cut, minimum = input$tna_plot_min,
             edge.label.cex = input$tna_edge_label_size, node.width = input$tna_node_size,
             label.cex = input$tna_node_label_size, layout = input$tna_layout, bg = "white")
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_plot_pdf <- downloadHandler(
    filename = function() paste0("tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 8)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        plot(x = tna_model(), cut = input$tna_plot_cut, minimum = input$tna_plot_min,
             edge.label.cex = input$tna_edge_label_size, node.width = input$tna_node_size,
             label.cex = input$tna_node_label_size, layout = input$tna_layout, bg = "white")
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Group TNA Plot Downloads
  output$gtna_plot_png <- downloadHandler(
    filename = function() paste0("group_tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(gtna_model())
      png(file, width = 1000, height = 800, res = 100)
      groupModels <- gtna_model()
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)
      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
      tryCatch({
        for(g in names(groupModels)) {
          plot(groupModels[[g]], title = g, cut = input$gtna_plot_cut, minimum = input$gtna_plot_min,
               edge.label.cex = input$gtna_edge_label_size, node.width = input$gtna_node_size,
               label.cex = input$gtna_node_label_size, layout = input$gtna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$gtna_plot_pdf <- downloadHandler(
    filename = function() paste0("group_tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(gtna_model())
      pdf(file, width = 10, height = 8)
      groupModels <- gtna_model()
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)
      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
      tryCatch({
        for(g in names(groupModels)) {
          plot(groupModels[[g]], title = g, cut = input$gtna_plot_cut, minimum = input$gtna_plot_min,
               edge.label.cex = input$gtna_edge_label_size, node.width = input$gtna_node_size,
               label.cex = input$gtna_node_label_size, layout = input$gtna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Cluster TNA Plot Downloads
  output$ctna_plot_png <- downloadHandler(
    filename = function() paste0("cluster_tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ctna_clusters())
      clusters <- ctna_clusters()
      if(is.null(clusters$models)) return()
      png(file, width = 1000, height = 800, res = 100)
      groupModels <- clusters$models
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)
      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
      tryCatch({
        for(g in names(groupModels)) {
          plot(groupModels[[g]], title = paste("Cluster", g), cut = input$ctna_plot_cut,
               minimum = input$ctna_plot_min, edge.label.cex = input$ctna_edge_label_size,
               node.width = input$ctna_node_size, label.cex = input$ctna_node_label_size,
               layout = input$ctna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ctna_plot_pdf <- downloadHandler(
    filename = function() paste0("cluster_tna_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ctna_clusters())
      clusters <- ctna_clusters()
      if(is.null(clusters$models)) return()
      pdf(file, width = 10, height = 8)
      groupModels <- clusters$models
      nGroups <- length(groupModels)
      nCols <- ceiling(sqrt(nGroups))
      nRows <- ceiling(nGroups / nCols)
      par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
      tryCatch({
        for(g in names(groupModels)) {
          plot(groupModels[[g]], title = paste("Cluster", g), cut = input$ctna_plot_cut,
               minimum = input$ctna_plot_min, edge.label.cex = input$ctna_edge_label_size,
               node.width = input$ctna_node_size, label.cex = input$ctna_node_label_size,
               layout = input$ctna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence TNA Plot Downloads
  output$cotna_plot_png <- downloadHandler(
    filename = function() paste0("cooccurrence_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_model())
      png(file, width = 800, height = 800, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        plot(x = cotna_model(), cut = input$cotna_plot_cut, minimum = input$cotna_plot_min,
             edge.label.cex = input$cotna_edge_label_size, node.width = input$cotna_node_size,
             label.cex = input$cotna_node_label_size, layout = input$cotna_layout, bg = "white")
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_plot_pdf <- downloadHandler(
    filename = function() paste0("cooccurrence_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_model())
      pdf(file, width = 8, height = 8)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        plot(x = cotna_model(), cut = input$cotna_plot_cut, minimum = input$cotna_plot_min,
             edge.label.cex = input$cotna_edge_label_size, node.width = input$cotna_node_size,
             label.cex = input$cotna_node_label_size, layout = input$cotna_layout, bg = "white")
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot TNA Plot Downloads
  output$ohtna_plot_png <- downloadHandler(
    filename = function() paste0("onehot_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      png(file, width = 1000, height = 800, res = 100)
      tryCatch({
        if(result$type == "group") {
          groupModels <- result$models
          nGroups <- length(groupModels)
          nCols <- ceiling(sqrt(nGroups))
          nRows <- ceiling(nGroups / nCols)
          par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
          for(g in names(groupModels)) {
            plot(groupModels[[g]], title = g, cut = input$ohtna_plot_cut, minimum = input$ohtna_plot_min,
                 edge.label.cex = input$ohtna_edge_label_size, node.width = input$ohtna_node_size,
                 label.cex = input$ohtna_node_label_size, layout = input$ohtna_layout, bg = "white")
          }
        } else {
          par(mar = c(0.5, 0.5, 1, 0.5))
          plot(result$model, cut = input$ohtna_plot_cut, minimum = input$ohtna_plot_min,
               edge.label.cex = input$ohtna_edge_label_size, node.width = input$ohtna_node_size,
               label.cex = input$ohtna_node_label_size, layout = input$ohtna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_plot_pdf <- downloadHandler(
    filename = function() paste0("onehot_network_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      pdf(file, width = 10, height = 8)
      tryCatch({
        if(result$type == "group") {
          groupModels <- result$models
          nGroups <- length(groupModels)
          nCols <- ceiling(sqrt(nGroups))
          nRows <- ceiling(nGroups / nCols)
          par(mfrow = c(nRows, nCols), mar = c(0.5, 0.5, 1.5, 0.5))
          for(g in names(groupModels)) {
            plot(groupModels[[g]], title = g, cut = input$ohtna_plot_cut, minimum = input$ohtna_plot_min,
                 edge.label.cex = input$ohtna_edge_label_size, node.width = input$ohtna_node_size,
                 label.cex = input$ohtna_node_label_size, layout = input$ohtna_layout, bg = "white")
          }
        } else {
          par(mar = c(0.5, 0.5, 1, 0.5))
          plot(result$model, cut = input$ohtna_plot_cut, minimum = input$ohtna_plot_min,
               edge.label.cex = input$ohtna_edge_label_size, node.width = input$ohtna_node_size,
               label.cex = input$ohtna_node_label_size, layout = input$ohtna_layout, bg = "white")
        }
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # ==========================================================================
  # Additional Plot Download Handlers
  # ==========================================================================

  # SNA Degree Plot Downloads
  output$sna_degree_png <- downloadHandler(
    filename = function() paste0("sna_degree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(sna_model(), input$sna_show_degree_plot)
      result <- sna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$sna_degree_pdf <- downloadHandler(
    filename = function() paste0("sna_degree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(sna_model(), input$sna_show_degree_plot)
      result <- sna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Histogram Downloads
  output$tna_histogram_png <- downloadHandler(
    filename = function() paste0("tna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_histogram_pdf <- downloadHandler(
    filename = function() paste0("tna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Frequencies Downloads
  output$tna_frequencies_png <- downloadHandler(
    filename = function() paste0("tna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_frequencies_pdf <- downloadHandler(
    filename = function() paste0("tna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Mosaic Downloads
  output$tna_mosaic_png <- downloadHandler(
    filename = function() paste0("tna_mosaic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(mosaic(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_mosaic_pdf <- downloadHandler(
    filename = function() paste0("tna_mosaic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(mosaic(tna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Centrality Plot Downloads
  output$tna_centrality_png <- downloadHandler(
    filename = function() paste0("tna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model(), length(input$tna_centrality_measures) > 0)
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = tna_model(), loops = input$tna_centrality_loops,
                            normalize = input$tna_centrality_normalize, measures = input$tna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_centrality_pdf <- downloadHandler(
    filename = function() paste0("tna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model(), length(input$tna_centrality_measures) > 0)
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = tna_model(), loops = input$tna_centrality_loops,
                            normalize = input$tna_centrality_normalize, measures = input$tna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Stability Plot Downloads
  output$tna_stability_png <- downloadHandler(
    filename = function() paste0("tna_stability_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_prepared_data())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        stab <- centrality_stability(tna_prepared_data(), measures = input$tna_stability_measures,
                                     iter = input$tna_stability_iterations, threshold = input$tna_stability_threshold,
                                     certainty = input$tna_stability_certainty)
        print(plot(stab))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_stability_pdf <- downloadHandler(
    filename = function() paste0("tna_stability_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_prepared_data())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        stab <- centrality_stability(tna_prepared_data(), measures = input$tna_stability_measures,
                                     iter = input$tna_stability_iterations, threshold = input$tna_stability_threshold,
                                     certainty = input$tna_stability_certainty)
        print(plot(stab))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Community Plot Downloads
  output$tna_community_png <- downloadHandler(
    filename = function() paste0("tna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = tna_model(), method = input$tna_community_method, gamma = input$tna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_community_pdf <- downloadHandler(
    filename = function() paste0("tna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = tna_model(), method = input$tna_community_method, gamma = input$tna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Bootstrap Plot Downloads
  output$tna_bootstrap_png <- downloadHandler(
    filename = function() paste0("tna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_prepared_data(), tna_model())
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(tna_prepared_data(), iter = input$tna_bootstrap_iterations, level = input$tna_bootstrap_level,
                         method = input$tna_bootstrap_method)
        print(plot(boot, model = tna_model()))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_bootstrap_pdf <- downloadHandler(
    filename = function() paste0("tna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_prepared_data(), tna_model())
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(tna_prepared_data(), iter = input$tna_bootstrap_iterations, level = input$tna_bootstrap_level,
                         method = input$tna_bootstrap_method)
        print(plot(boot, model = tna_model()))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Sequences Plot Downloads
  output$tna_sequences_png <- downloadHandler(
    filename = function() paste0("tna_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_prepared_data())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        p <- plot_sequences(tna_prepared_data(), type = input$tna_sequences_type, scale = input$tna_sequences_scale,
                           geom = input$tna_sequences_geom, include_missing = input$tna_sequences_include_na,
                           tick = input$tna_sequences_tick)
        print(p)
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_sequences_pdf <- downloadHandler(
    filename = function() paste0("tna_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_prepared_data())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        p <- plot_sequences(tna_prepared_data(), type = input$tna_sequences_type, scale = input$tna_sequences_scale,
                           geom = input$tna_sequences_geom, include_missing = input$tna_sequences_include_na,
                           tick = input$tna_sequences_tick)
        print(p)
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # TNA Edge Betweenness Plot Downloads
  output$tna_edgebetweenness_png <- downloadHandler(
    filename = function() paste0("tna_edgebetweenness_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(tna_model())
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        ebNetwork <- betweenness_network(x = tna_model())
        print(plot(ebNetwork, layout = input$tna_edge_betweenness_layout))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$tna_edgebetweenness_pdf <- downloadHandler(
    filename = function() paste0("tna_edgebetweenness_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(tna_model())
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        ebNetwork <- betweenness_network(x = tna_model())
        print(plot(ebNetwork, layout = input$tna_edge_betweenness_layout))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Group TNA Centrality Plot Downloads
  output$gtna_centrality_png <- downloadHandler(
    filename = function() paste0("gtna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(gtna_model(), length(input$gtna_centrality_measures) > 0)
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = gtna_model(), loops = input$gtna_centrality_loops,
                            normalize = input$gtna_centrality_normalize, measures = input$gtna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$gtna_centrality_pdf <- downloadHandler(
    filename = function() paste0("gtna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(gtna_model(), length(input$gtna_centrality_measures) > 0)
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = gtna_model(), loops = input$gtna_centrality_loops,
                            normalize = input$gtna_centrality_normalize, measures = input$gtna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Group TNA Permutation Plot Downloads
  output$gtna_permutation_png <- downloadHandler(
    filename = function() paste0("gtna_permutation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(gtna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        perm <- permutation_test(gtna_model(), iter = input$gtna_permutation_iter,
                                paired = input$gtna_permutation_paired, level = input$gtna_permutation_level)
        print(plot(perm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$gtna_permutation_pdf <- downloadHandler(
    filename = function() paste0("gtna_permutation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(gtna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        perm <- permutation_test(gtna_model(), iter = input$gtna_permutation_iter,
                                paired = input$gtna_permutation_paired, level = input$gtna_permutation_level)
        print(plot(perm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Group TNA Sequences Plot Downloads
  output$gtna_sequences_png <- downloadHandler(
    filename = function() paste0("gtna_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(gtna_prepared_data())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        p <- plot_sequences(gtna_prepared_data(), type = input$gtna_sequences_type)
        print(p)
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$gtna_sequences_pdf <- downloadHandler(
    filename = function() paste0("gtna_sequences_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(gtna_prepared_data())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        p <- plot_sequences(gtna_prepared_data(), type = input$gtna_sequences_type)
        print(p)
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence Histogram Downloads
  output$cotna_histogram_png <- downloadHandler(
    filename = function() paste0("cotna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(cotna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_histogram_pdf <- downloadHandler(
    filename = function() paste0("cotna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(cotna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence Frequencies Downloads
  output$cotna_frequencies_png <- downloadHandler(
    filename = function() paste0("cotna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_model())
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(cotna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_frequencies_pdf <- downloadHandler(
    filename = function() paste0("cotna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_model())
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(cotna_model())) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence Centrality Plot Downloads
  output$cotna_centrality_png <- downloadHandler(
    filename = function() paste0("cotna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_model(), length(input$cotna_centrality_measures) > 0)
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = cotna_model(), loops = input$cotna_centrality_loops,
                            normalize = input$cotna_centrality_normalize, measures = input$cotna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_centrality_pdf <- downloadHandler(
    filename = function() paste0("cotna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_model(), length(input$cotna_centrality_measures) > 0)
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = cotna_model(), loops = input$cotna_centrality_loops,
                            normalize = input$cotna_centrality_normalize, measures = input$cotna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence Community Plot Downloads
  output$cotna_community_png <- downloadHandler(
    filename = function() paste0("cotna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_model())
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = cotna_model(), method = input$cotna_community_method, gamma = input$cotna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_community_pdf <- downloadHandler(
    filename = function() paste0("cotna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_model())
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = cotna_model(), method = input$cotna_community_method, gamma = input$cotna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # Co-occurrence Bootstrap Plot Downloads
  output$cotna_bootstrap_png <- downloadHandler(
    filename = function() paste0("cotna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(cotna_prepared_data(), cotna_model())
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(cotna_prepared_data(), iter = input$cotna_bootstrap_iterations, level = input$cotna_bootstrap_level)
        print(plot(boot, model = cotna_model()))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$cotna_bootstrap_pdf <- downloadHandler(
    filename = function() paste0("cotna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(cotna_prepared_data(), cotna_model())
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(cotna_prepared_data(), iter = input$cotna_bootstrap_iterations, level = input$cotna_bootstrap_level)
        print(plot(boot, model = cotna_model()))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Histogram Downloads
  output$ohtna_histogram_png <- downloadHandler(
    filename = function() paste0("ohtna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_histogram_pdf <- downloadHandler(
    filename = function() paste0("ohtna_histogram_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(hist(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Frequencies Downloads
  output$ohtna_frequencies_png <- downloadHandler(
    filename = function() paste0("ohtna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_frequencies_pdf <- downloadHandler(
    filename = function() paste0("ohtna_frequencies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(frequencies(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Mosaic Downloads
  output$ohtna_mosaic_png <- downloadHandler(
    filename = function() paste0("ohtna_mosaic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(mosaic(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_mosaic_pdf <- downloadHandler(
    filename = function() paste0("ohtna_mosaic_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({ print(mosaic(model)) }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Centrality Plot Downloads
  output$ohtna_centrality_png <- downloadHandler(
    filename = function() paste0("ohtna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model(), length(input$ohtna_centrality_measures) > 0)
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = model, loops = input$ohtna_centrality_loops,
                            normalize = input$ohtna_centrality_normalize, measures = input$ohtna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_centrality_pdf <- downloadHandler(
    filename = function() paste0("ohtna_centrality_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model(), length(input$ohtna_centrality_measures) > 0)
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        cent <- centralities(x = model, loops = input$ohtna_centrality_loops,
                            normalize = input$ohtna_centrality_normalize, measures = input$ohtna_centrality_measures)
        print(plot(cent))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Community Plot Downloads
  output$ohtna_community_png <- downloadHandler(
    filename = function() paste0("ohtna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = model, method = input$ohtna_community_method, gamma = input$ohtna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_community_pdf <- downloadHandler(
    filename = function() paste0("ohtna_community_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        comm <- communities(x = model, method = input$ohtna_community_method, gamma = input$ohtna_community_gamma)
        print(plot(comm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Bootstrap Plot Downloads
  output$ohtna_bootstrap_png <- downloadHandler(
    filename = function() paste0("ohtna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_prepared_data(), ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      png(file, width = 800, height = 600, res = 100)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(ohtna_prepared_data(), iter = input$ohtna_bootstrap_iterations, level = input$ohtna_bootstrap_level)
        print(plot(boot, model = model))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_bootstrap_pdf <- downloadHandler(
    filename = function() paste0("ohtna_bootstrap_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_prepared_data(), ohtna_model())
      result <- ohtna_model()
      model <- if(result$type == "group") result$models[[1]] else result$model
      pdf(file, width = 8, height = 6)
      par(mar = c(0.5, 0.5, 1, 0.5))
      tryCatch({
        boot <- bootstrap(ohtna_prepared_data(), iter = input$ohtna_bootstrap_iterations, level = input$ohtna_bootstrap_level)
        print(plot(boot, model = model))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  # One-Hot Permutation Plot Downloads
  output$ohtna_permutation_png <- downloadHandler(
    filename = function() paste0("ohtna_permutation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      if(result$type != "group") return()
      png(file, width = 800, height = 400, res = 100)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        perm <- permutation_test(result$models, iter = input$ohtna_permutation_iter, level = input$ohtna_permutation_level)
        print(plot(perm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )

  output$ohtna_permutation_pdf <- downloadHandler(
    filename = function() paste0("ohtna_permutation_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf"),
    content = function(file) {
      req(ohtna_model())
      result <- ohtna_model()
      if(result$type != "group") return()
      pdf(file, width = 8, height = 4)
      par(mar = c(3, 3, 2, 1))
      tryCatch({
        perm <- permutation_test(result$models, iter = input$ohtna_permutation_iter, level = input$ohtna_permutation_level)
        print(plot(perm))
      }, error = function(e) { plot.new(); text(0.5, 0.5, "Error", col = "red") })
      dev.off()
    }
  )
}

# ============================================================================
# RUN APP
# ============================================================================

shinyApp(ui = ui, server = server)
