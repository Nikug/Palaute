# Notification settings
Notifications <- list(
  "displayTime" = 20
)

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
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0.3, message = "Sentiment and emotion analysis: ",
                 detail = paste0("For the whole data set",
                                  "\nThis can take minutes..."))
    
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
    
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(0, message = "Performing topic specific sentiment and emotion analysis: ")
    
    documentList <- topicDocuments(model, analysisData, settings)
    topics <- length(documentList)
    topicSentiments <- vector(mode = "list", length = topics)
    for(i in 1:topics) {
      progress$inc(1 / topics,
                   detail = paste0("Topic ", i, "/", topics))
      
      documents <- documentList[[i]]
      topicSentiments[[i]] <- sentimentAnalysis(documents, settings$language)
    }
    topicSentiments
  })
  
  observeEvent(input$start, {
    if(Verbose) {print("Started analysis")}
    mainProgress <- shiny::Progress$new()
    on.exit(mainProgress$close())
    mainProgress$set(message = "Status:", value = 0)
    
    settings <- settingsr()
    
    mainProgress$inc(0, detail = "Calculating topic model")
    model <- modelr()
    
    mainProgress$inc (1/3, detail = "Performing sentiment analysis")
    sentimentSummary <- sentimentSummaryr()
    
    mainProgress$inc(1/3, detail = "Performing topic specific sentiment analysis")
    topicSentiments <- topicSentimentsr()
    analysisData <- analysisDatar()

    analysisResults <- list("model" = model, 
                            "sentiment" = sentimentSummary,
                            "topicSentiment" = topicSentiments,
                            "data" = analysisData)

    analysis$results <- analysisResults
    
    if(Verbose) {print("Analysis completed!")}
    mainProgress$set(1, detail = "Finished!")
    showNotification("Analysis completed!",
                     duration = Notifications$displayTime,
                     type = "message")
  })
  
  return(analysis)
}