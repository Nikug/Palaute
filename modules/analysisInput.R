

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
    data <- isolate(datar())
    print("Started analysis")
    
    analysisData <- NULL
    if(settings$useSampling) {
      analysisData <- sampleDocuments(data, settings$sampleSize)
    } else {
      analysisData <- data
    }
    
    processedAnalysisData <- preprocessDocuments(analysisData, settings)
    model <- topicModelAnalysis(processedAnalysisData, settings)
  })

}