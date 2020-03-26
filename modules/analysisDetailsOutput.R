DetailSettings <- list(
  "labelCount" = 10
)

analysisDetailsOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 4,
        wellPanel(
          tags$h2("Filter")
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
  showDocumentsInputsr <- reactive({
    validate(
      need(resultsr(), message = FALSE)
    )
    results <- resultsr()
    
    inputs <- list()
    inputs <- lapply(1:results$model$settings$dim$K, function(topic) {
      showDocumentsName <- paste0("showDocuments", topic)
      inputs <- paste0(inputs, input[[showDocumentsName]])
    })
  })
  
  orderEmotionsr <- reactive({
    validate(
      need(resultsr(), message = FALSE)
    )
    results <- resultsr()
    
    inputs <- list()
    inputs <- lapply(1:results$model$settings$dim$K, function(topic) {
      orderName <- paste0("sortEmotions", topic)
      inputs <- paste0(inputs, input[[orderName]])
    })
  })
  
  
  
  observeEvent(resultsr(), {
    results <- resultsr()
    model <- results$model
    
    topicLabels <- labelTopics(model, n = DetailSettings$labelCount)
    
    lapply(1:model$settings$dim$K, function(topic) {
      # Generate UI
      ui <- generateUI(topic, model, results$topicSentiment[[topic]], session$ns)
      insertUI(selector = "#topics", where = "beforeEnd", ui = ui)
      
      # Key words
      outputKeywords <- paste0("keywords", topic)
      textOut <- keywordsTextFormat(topicLabels, topic)
      
      # Sentiment
      outputSentiment <- paste0("sentiment", topic)
      sentiment <- results$topicSentiment[[topic]][9:10, ]
      sentimentPlot <- simpleSentimentBarPlot(sentiment, plotTitle = NULL)
      

      
      # Outputs
      output[[outputKeywords]] <- renderText({
        textOut
      })
      
      output[[outputSentiment]] <- renderPlot({
        sentimentPlot
      }, bg = "transparent")
    })
  })
  
  observeEvent(showDocumentsInputsr(), {
    validate(
      need(resultsr(), message = FALSE),
      need(showDocumentsInputsr(), message = FALSE)
    )
    
    lengths <- showDocumentsInputsr()
    results <- resultsr()
    model <- results$model
    
    lapply(1:length(lengths), function(topic) {
      documentCount <- lengths[[topic]]
      
      validate(need(documentCount, message = FALSE))
      
      documentCount <- as.numeric(documentCount)
      documents <- findThoughts(model,
                                texts = as.character(results$data$meta$documents),
                                topics = topic,
                                n = ifelse(documentCount > 0, documentCount, 1)
                                )$docs
      
      names(documents) <- paste0("Topic", topic)
      outputDocuments <- paste0("documents", topic)
      output[[outputDocuments]] <- renderTable({
        documents
      })
    })
  })
  
  observeEvent(orderEmotionsr(), {
    validate(
      need(resultsr(), message = FALSE),
      need(orderEmotionsr(), message = FALSE)
    )
    
    orderings <- orderEmotionsr()
    results <- resultsr()
    
    lapply(1:length(orderings), function(topic) {
      validate(need(orderings[[topic]], message = FALSE))
      
      emotions <- results$topicSentiment[[topic]][1:8, ]
      emotionPlot <- sentimentBarPlot(emotions, orderData = orderings[[topic]], plotTitle = NULL, transparent = TRUE)
      
      outputEmotion <- paste0("emotion", topic)
      output[[outputEmotion]] <- renderPlot({
        emotionPlot
      }, bg = "transparent")
    })
  })
}

generateUI <- function(topic, model, sentiment, ns) {
  topicProportion <- sum(model$theta[, topic] / model$settings$dim$N)
  ui <- tagList(
    wellPanel(
      fluidRow(
        column(width = 6,
          tags$h3(paste("Topic", topic)),
          tags$p(paste0("Topic proportion ", round(topicProportion * 100, 0), "%")),
          tags$h3("Key words"),
          verbatimTextOutput(outputId = ns(paste0("keywords", topic)))
        ),
        column(width= 6,
          tags$h3("Topic sentiment"),
          plotOutput(outputId = ns(paste0("sentiment", topic)), height = "100px"),
          tags$h3("Top emotions"),
          checkboxInput(inputId = ns(paste0("sortEmotions", topic)),
                        label = "Sort",
                        value = FALSE),
          plotOutput(outputId = ns(paste0("emotion", topic)))
        ),
        fluidRow(
          column(width = 2,
            numericInput(inputId = ns(paste0("showDocuments", topic)),
                         label = "Show top documents",
                         value = 0, 
                         min = 0,
                         max = 100)
          )
        ),
        fluidRow(
          column(width = 12,
            conditionalPanel(condition = paste0("input.showDocuments", topic, " > 0"), ns = ns,
              tableOutput(outputId = ns(paste0("documents", topic)))
            )
          )
        )
      )
    )
  )
  return(ui)
}

keywordsTextFormat <- function(labels, topic) {
  text <- paste0(
    paste("Most probable:", paste(labels$prob[topic, ], collapse = " "), "\n"),
    paste("Frex:", paste(labels$frex[topic, ], collapse = " "), "\n"),
    paste("Lift:", paste(labels$lift[topic, ], collapse = " "), "\n"),
    paste("Score:", paste(labels$score[topic, ], collapse = " "), "\n")
  )
  return(text)
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
