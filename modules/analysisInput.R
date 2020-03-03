analysisInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(inputId = ns("start"), label = "Start analysis")
  )
}

analysisInputFunction <- function(input, output, session, settings) {
  observeEvent(input$start, {
    print("Started analysis")
    print(settings()$sampleSize)
  })
}