DetailSettings <- list(
  "keywords" = 10,
  "minKeywords" = 1,
  "maxKeywords" = 1000,
  "documents" = 2,
  "minDocuments" = 0,
  "maxDocuments" = 1000
)

analysisDetailsOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 4,
        div(class = "fixed",
          wellPanel(class = "scaleWidth",
            tags$h2("Filter"),
            checkboxInput(inputId = ns("orderEmotions"),
                          label = "Sort emotion analysis",
                          value = FALSE
                          ),
            numericInput(inputId = ns("numberOfKeywords"),
                         label = "Number of keywords",
                         value = DetailSettings$keywords,
                         min = DetailSettings$minKeywords,
                         max = DetailSettings$maxKeywords
                         ),
            numericInput(inputId = ns("showDocuments"),
                         label = "Show top documents",
                         value = DetailSettings$documents, 
                         min = DetailSettings$minDocuments,
                         max = DetailSettings$maxDocuments
                         ),
            checkboxInput(inputId = ns("hideKeywords"),
                          label = "Hide keywords",
                          value = FALSE),
            checkboxInput(inputId = ns("hideSentiment"),
                          label = "Hide sentiment",
                          value = FALSE),
            checkboxInput(inputId = ns("hideEmotions"),
                          label = "Hide emotions",
                          value = FALSE),
            checkboxInput(inputId = ns("hideDocuments"),
                          label = "Hide documents",
                          value = FALSE)
          )
        )
      ),
      column(width = 8,
        tags$h2("Topics"),
        div(id = "topics")
      )
    )
  )
}

analysisDetailsOutputFunction <- function(input, output, session, resultsr) {
  observeEvent(c(resultsr(), input$hideEmotions), {
    validate(
      need(resultsr(), message = FALSE)
    )
    results <- resultsr()
    model <- results$model
    
    removeUI(selector = "#topics >", multiple = TRUE)
    
    lapply(1:model$settings$dim$K, function(topic) {
      # Generate UI
      hide <- input[[paste0("hideTopic", topic)]]
      ui <- generateUI(topic, model, results$topicSentiment[[topic]], session$ns, input, hide)
      insertUI(selector = "#topics", where = "beforeEnd", ui = ui)
      
      # Sentiment
      outputSentiment <- paste0("sentiment", topic)
      sentiment <- results$topicSentiment[[topic]][9:10, ]
      sentimentPlot <- simpleSentimentBarPlot(sentiment, plotTitle = NULL)
      
      # Outputs
      output[[outputSentiment]] <- renderPlot({
        sentimentPlot
      }, bg = "transparent")
    })
  })
  
  observeEvent(c(resultsr(), input$numberOfKeywords), {
    validate(
      need(resultsr(), message = FALSE),
      need(is.numeric(input$numberOfKeywords), message = "Invalid number of keywords"),
      need(input$numberOfKeywords <= DetailSettings$maxKeywords & input$numberOfKeywords >= DetailSettings$minKeywords,
           message = "Invalid number of keywords")
    )
    results <- resultsr()
    model <- results$model
    
    topicLabels <- labelTopics(model, n = input$numberOfKeywords)
    
    lapply(1:model$settings$dim$K, function(topic) {
      outputKeywords <- paste0("keywords", topic)
      textOut <- keywordsTextFormat(topicLabels, topic)
      
      output[[outputKeywords]] <- renderUI({
        textOut
      })
    })
  })
  
  observeEvent(c(resultsr(), input$orderEmotions), {
    validate(
      need(resultsr(), message = FALSE)
    )
    results <- resultsr()
    model <- results$model
    
    lapply(1:model$settings$dim$K, function(topic) {
      outputEmotions <- paste0("emotion", topic)
      emotions <- results$topicSentiment[[topic]][1:8, ]
      emotionPlot <- sentimentBarPlot(emotions,
                                      orderData = input$orderEmotions,
                                      plotTitle = NULL,
                                      transparent = TRUE)
      
      output[[outputEmotions]] <- renderPlot({
        emotionPlot
      }, bg = "transparent")
    })
  })
  
  observeEvent(c(resultsr(), input$showDocuments), {
    validate(
      need(resultsr(), message = FALSE),
      need(is.numeric(input$showDocuments), message = "Invalid number of documents"),
      need(input$numberOfKeywords <= DetailSettings$maxDocuments & input$numberOfKeywords >= DetailSettings$minDocuments,
           message = "Invalid number of documents")
    )
  
    results <- resultsr()
    model <- results$model
    
    lapply(1:model$settings$dim$K, function(topic) {
      documentCount <- input$showDocuments
      
      validate(need(documentCount, message = FALSE))
      
      documentCount <- as.numeric(documentCount)
      documents <- findThoughts(model,
                                texts = as.character(results$data$meta$documents),
                                topics = topic,
                                n = ifelse(documentCount > 0, documentCount, 1)
                                )$docs
      
      names(documents) <- paste0("Topic", topic)
      outputDocuments <- paste0("documents", topic)
      output[[outputDocuments]] <- renderTable(rownames = TRUE, expr = {
        documents
      })
    })
  })
}

generateUI <- function(topic, model, sentiment, ns, input, hide) {
  topicProportion <- sum(model$theta[, topic] / model$settings$dim$N)
  documentCount <- sentiment$documentCount[1]
  ui <- tagList(
    wellPanel(
      fluidRow(
        column(width = 10,
          tags$h3(paste("Topic", topic))
        ),
        column(width = 2,
          checkboxInput(inputId = ns(paste0("hideTopic", topic)),
                        label = "Hide",
                        value = hide)
        )
      ),
      conditionalPanel(condition = paste0("input.hideTopic", topic, " == false"), ns = ns,
        fluidRow(
          column(width = 12,
            div(class = "text-info",
              tags$p(paste0("Topic proportion ", round(topicProportion * 100, 0), "%")),
              tags$p(paste0(documentCount, " exclusive documents used in sentiment and emotion analysis"))
            )
          )
        ),
        fluidRow(
          conditionalPanel(condition = "input.hideSentiment == false || input.hideKeywords == false", ns = ns,
            column(width = ifelse(input$hideEmotions == FALSE, 6, 12),
              conditionalPanel(condition = "input.hideSentiment == false", ns = ns,
                wellPanel(
                  tags$h3("Sentiment"),
                  plotOutput(outputId = ns(paste0("sentiment", topic)), height = "100px")
                )
              ),
              conditionalPanel(condition = "input.hideKeywords == false", ns = ns,
                wellPanel(
                  tags$h3("Key words"),
                  uiOutput(outputId = ns(paste0("keywords", topic)))
                )
              )
            )
          ),
          conditionalPanel(condition = "input.hideEmotions == false", ns = ns,
            column(width= 6,
              wellPanel(
                tags$h3("Top emotions"),
                plotOutput(outputId = ns(paste0("emotion", topic)))
              )
            )
          )
        ),
        fluidRow(
          column(width = 12,
            conditionalPanel(condition = "input.hideDocuments == false && input.showDocuments > 0", ns = ns,
              wellPanel(
                tags$h3("Top documents"),
                tableOutput(outputId = ns(paste0("documents", topic)))
              )
            )
          )
        )
      )
    )
  )
  return(ui)
}

keywordsTextFormat <- function(labels, topic) {
  ui <- tagList(
    tags$p(tags$strong("Most probable:"), paste(labels$prob[topic, ], collapse = " ")),
    tags$p(tags$strong("Frex:"), paste(labels$frex[topic, ], collapse = " ")),
    tags$p(tags$strong("Lift:"), paste(labels$lift[topic, ], collapse = " ")),
    tags$p(tags$strong("Score:"), paste(labels$score[topic, ], collapse = " "))
  )
  
  return(ui)
}

simpleSentimentBarPlot <- function(data, plotTitle = "Summary", plotSubtitle = "") {
  data <- data %>%
    mutate(dummy = 0)
  
  colorPalette <- wes_palette("Zissou1", 5)[c(2, 5)]
  
  sentimentPlot <- ggplot(data = data) +
    geom_bar(aes(
      x = dummy,
      y = percentage,
      fill = rev(emotion)
    ),
      stat = "identity"
    ) + 
    geom_text(aes(
      x = dummy,
      y = percentage,
      label = paste0(round(percentage * 100, 0), "%")
    ),
      position = position_stack(vjust = 0.5),
      size = PlotSettings$geomTextSize
    ) + 
    geom_text(aes(
      x = dummy + 1, # Totally illegal fix
      y = percentage,
      label = emotion
    ),
      position = position_stack(vjust = 0.5),
      size = PlotSettings$geomTextSize
    ) + 
    scale_y_reverse() + 
    coord_flip() + 
    scale_fill_manual(values = colorPalette) + 
    theme(
      legend.position = "none",
      text = element_text(size = PlotSettings$textSize),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.subtitle = element_text(size = PlotSettings$subtitleSize),
      panel.background = element_blank(),
      plot.background = element_blank()
    ) + 
    ggtitle(plotTitle) + 
    labs(subtitle = plotSubtitle) +
    xlab(label = NULL) + 
    ylab(label = NULL)
  
  return(sentimentPlot)
}
