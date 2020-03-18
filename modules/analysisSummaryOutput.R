analysisSummaryOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h1("Analysis summary"),
    plotOutput(outputId = ns("summary"))
  )
}

analysisSummaryOutputFunction <- function(input, output, session, resultsr) {
  output$summary <- renderPlot({
    results <- resultsr()
    print(results$model)
    plot(results$model, type = "summary")
  })
}