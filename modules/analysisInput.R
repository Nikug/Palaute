analysisInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    actionButton(inputId = ns("start"), label = "Start analysis")
  )
}

analysisInputFunction <- function(input, output, session, settingsr, datar) {
  analysis <- reactiveValues(results = NULL)
  
  # Process data
  analysisDatar <- reactive({
    validate(
      need(settingsr(), message = "Data preprocess: There are no settings"),
      need(datar(), message = "Data preprocess: There is no data")
    )
    
    settings <- reactiveValuesToList(settingsr())
    data <- datar()
    
    analysisData <- NULL
    if(settings$useSampling) {
      analysisData <- sampleDocuments(data, settings$sampleSize)
    } else {
      analysisData <- data
    }
    analysisData <- preprocessDocuments(analysisData, settings)
    analysisData
  })
  
  # Topic modeling
  modelr <- reactive({
    validate(
      need(settingsr(), message = "Topic modeling: There are no settings"),
      need(analysisDatar(), message = "Topic modeling: There is no data")
    )
    analysisData <- analysisDatar()
    settings <- isolate(reactiveValuesToList(settingsr()))
    
    model <- topicModelAnalysis(analysisData, settings)
    model
  })
  
  # Summary sentiment analysis
  sentimentSummaryr <- reactive({
    validate(
      need(settingsr(), message = "Sentiment analysis: There are no settings"),
      need(analysisDatar(), message = "Sentiment analysis: There is no data")
    )
    analysisData <- analysisDatar()
    settings <- isolate(reactiveValuesToList(settingsr()))
    
    result <- sentimentAnalysis(analysisData$meta$documents, settings$language)
    result
  })
  
  # Topic specific sentiment analysis
  topicSentimentsr <- reactive({
    validate(
      need(settingsr(), message = "Topic sentiment analysis: There are no settings"),
      need(analysisDatar(), message = "Topic sentiment analysis: There is no data")
    )
    model <- modelr()
    analysisData <- analysisDatar()
    settings <- isolate(reactiveValuesToList(settingsr()))
    
    documentList <- topicDocuments(model, analysisData, settings)
    topicSentiments <- vector(mode = "list", length = length(documentList))
    for(i in 1:length(documentList)) {
      documents <- documentList[[i]]
      topicSentiments[[i]] <- sentimentAnalysis(documents, settings$language)
    }
    topicSentiments
  })
  
  observeEvent(input$start, {
    print("Started analysis")
    settings <- settingsr()
    model <- modelr()
    sentimentSummary <- sentimentSummaryr()
    topicSentiments <- topicSentimentsr()
    analysisData <- analysisDatar()

    analysisResults <- list("model" = model, 
                            "sentiment" = sentimentSummary,
                            "topicSentiment" = topicSentiments,
                            "data" = analysisData)

    print(analysisResults$topicSentiment)
    analysis$results <- analysisResults
    print("Analysis completed!")
  })
  
  return(analysis)
}