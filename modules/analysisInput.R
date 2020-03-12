analysisInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(inputId = ns("start"), label = "Start analysis")
  )
}

analysisInputFunction <- function(input, output, session, settingsr, datar) {
  observeEvent(input$start, {
    validate(
      need(settingsr(), message = "There are no settings"),
      need(datar(), message = "There is no data")
    )
    settings <- isolate(reactiveValuesToList(settingsr()))
    data <- datar()

  })
}