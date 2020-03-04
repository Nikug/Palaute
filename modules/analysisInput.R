analysisInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(inputId = ns("start"), label = "Start analysis")
  )
}

analysisInputFunction <- function(input, output, session, settingsr) {
  observeEvent(input$start, {
    settings <- isolate(reactiveValuesToList(settingsr()))

  })
}