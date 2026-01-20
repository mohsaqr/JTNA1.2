# Shiny Example: Multi-Group TNA Plot
#
# This example shows how to render multi-group TNA plots in Shiny.
# The key is using par(mfrow) to set up the grid before calling plot().

library(shiny)
library(tna)

ui <- fluidPage(
  titlePanel("Multi-Group TNA Plot Example"),

  sidebarLayout(
    sidebarPanel(
      fileInput("dataFile", "Upload CSV Data"),
      selectInput("actionCol", "Action Column", choices = NULL),
      selectInput("actorCol", "Actor Column", choices = NULL),
      selectInput("groupCol", "Group Column", choices = NULL),
      actionButton("runAnalysis", "Run Analysis"),
      hr(),
      sliderInput("cut", "Cut Value", min = 0, max = 1, value = 0.1, step = 0.05),
      sliderInput("minimum", "Minimum", min = 0, max = 1, value = 0.05, step = 0.01)
    ),

    mainPanel(
      plotOutput("tnaPlot", height = "800px")
    )
  )
)

server <- function(input, output, session) {

  # Reactive: Load data
  data <- reactive({
    req(input$dataFile)
    read.csv(input$dataFile$datapath)
  })

  # Update column selectors when data is loaded
  observe({
    req(data())
    cols <- colnames(data())
    updateSelectInput(session, "actionCol", choices = cols)
    updateSelectInput(session, "actorCol", choices = cols)
    updateSelectInput(session, "groupCol", choices = cols)
  })

  # Reactive: Build model
  model <- eventReactive(input$runAnalysis, {
    req(data(), input$actionCol, input$actorCol, input$groupCol)

    df <- data()

    # Prepare data
    prepData <- tna::prepare_data(
      data = df,
      action = input$actionCol,
      actor = input$actorCol
    )

    # Get group assignments per session
    group <- prepData$long_data[!duplicated(prepData$long_data$.session_id), ]

    # Build group model
    tna::group_model(
      x = prepData,
      group = group[[input$groupCol]],
      type = "relative"
    )
  })

  # Render multi-group plot
  output$tnaPlot <- renderPlot({
    req(model())

    # Set up grid based on number of groups
    n_groups <- length(model())
    if (n_groups == 1) {
      par(mfrow = c(1, 1))
    } else if (n_groups <= 4) {
      par(mfrow = c(2, 2))
    } else if (n_groups <= 6) {
      par(mfrow = c(2, 3))
    } else if (n_groups <= 9) {
      par(mfrow = c(3, 3))
    } else {
      rows <- ceiling(sqrt(n_groups))
      cols <- ceiling(n_groups / rows)
      par(mfrow = c(rows, cols))
    }

    # Single plot call - tna handles iteration through groups internally
    plot(
      model(),
      cut = input$cut,
      minimum = input$minimum,
      layout = "circle"
    )
  })
}

shinyApp(ui, server)
