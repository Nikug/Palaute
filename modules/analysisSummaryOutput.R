PlotSettings <- list(
  "textSize" = 16,
  "axisSize" = 14,
  "geomTextSize" = 14 / ggplot2::.pt,
  "subtitleSize" = 12,
  "plotHeight" = "700px",
  "plotWidth" = "100%",
  "barPlotWidth" = "70%",
  
  "clickThreshold" = 20,
  "keyWords" = 5
)

analysisSummaryOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
        tags$h2("Analysis statistics"),
        verbatimTextOutput(outputId = ns("statistics"))
      ),
      column(width = 6,
        tags$h2("Model convergence"),
        plotOutput(outputId = ns("convergence"))       
      )
    ),
    tags$hr(),
    tags$h2("Topic analysis summary"),
    fluidRow(
      column(width = 6, align = "left",
        plotOutput(outputId = ns("topicSummary"),
                   width = PlotSettings$plotWidth,
                   height = PlotSettings$plotHeight,
                   click = ns("summaryClick")),
        uiOutput(outputId = ns("topicSummaryDetails")),
        tags$p(class = "small", align = "center",
               "Each circle represents a topic",
               br(),
               "Circle size = Topic prevalence. Larger circles are more prevelant in the corpus",
               br(),
               "Colour = Topic sentiment (Grey means there is no sentiment)",
               br(),
               "Distance between the topics is calculated as the difference between the vocabulary used by a topic.
               Topics away from each other use different words, whereas close topics share more words",
               br(),
               "Clicking on the centers of the topics shows additional information. Clicking away from topics hides
               the additional information box"
        )       
      ),
      column(width = 6, align = "left",
        plotOutput(outputId = ns("documentSummary"),
                   width = PlotSettings$plotWidth,
                   height = PlotSettings$plotHeight,
                   click = ns("documentClick")),
        uiOutput(outputId = ns("documentSummaryClick")),
        tags$p(class = "small", align = "center",
               "Each circle represents a document",
               br(),
               "Circle size = Topic proportion of the document. Larger circles have higher proportion of them dedicated to one topic",
               br(),
               "Colour = Indicates the topic that has the highest proportion of the document",
               br(),
               "Topic labels are placed on the mathematical center of the topic's documents",
               br(),
               "Clicking on the centers of the documents shows that document and additional information.
               Clicking away from documents hides the additional information box"
        ) 
      )
    ),
    tags$hr(),
    tags$h2("Sentiment analysis summary"),
    fluidRow(
      column(width = 6,
        plotOutput(outputId = ns("emotionSummary"), width = PlotSettings$barPlotWidth)
      ),
      column(width = 6,
             plotOutput(outputId = ns("sentimentSummary"), width = PlotSettings$barPlotWidth)
      )
    ),
    br(),
    br()
  )
}

analysisSummaryOutputFunction <- function(input, output, session, resultsr) {
  topicDistancesr <- reactive({
    validate(
      need(resultsr(), message = FALSE)
    )
    results <- resultsr()
    model <- results$model
    
    topicCount <- model$settings$dim$K
    topicSentiments <- lapply(results$topicSentiment, ggplottableSentimentFormat)
    
    distanceMatrix <- topicDistances(exp(model$beta$logbeta[[1]]))
    distanceMatrix <- cbind(distanceMatrix, "size" = sapply(1:topicCount,
                                                            function(i) sum(model$theta[, i]) / model$settings$dim$N))
    distanceMatrix <- cbind(distanceMatrix, "topic" = c(1:topicCount))
    distanceMatrix <- cbind(distanceMatrix, "sentiment" = sapply(topicSentiments, function(s) s$percentage[10]))
    distanceMatrix
  })
  
  documentMapr <- reactive({
    validate(
      need(resultsr(), message = FALSE)
    )
    
    results <- resultsr()
    model <- results$model
    topicCount <- model$settings$dim$K
    perplexity <- floor(topicCount / 3 - 1)
    reducedDimensions <- Rtsne(model$theta, k = 2,
                               initial_dims = topicCount,
                               perplexity = ifelse(perplexity < 1, 1, perplexity),
                               max_iter = 500,
                               verbose = Verbose,
                               check_duplicates = FALSE)
    reducedDimensionsDataframe <- data.frame(list("x" = reducedDimensions$Y[, 1],
                                                  "y" = reducedDimensions$Y[, 2],
                                                  "size" = sapply(1:model$settings$dim$N, function(i) max(model$theta[i, ])),
                                                  "topic" = max.col(model$theta, ties.method = "first")
    ))
    reducedDimensionsDataframe
  })
  
  output$statistics <- renderText({
    validate(
      need(resultsr(), message = "Run analysis to see the results")
    )
    
    results <- resultsr()
    model <- results$model
    change <- 0
    if(length(model$convergence$bound) > 1) {
      last <- length(model$convergence$bound)
      change <- (model$convergence$bound[last] - model$convergence$bound[last - 1]) / abs(model$convergence$bound[last - 1])
    }
    
    text <- paste0(
      ifelse(model$convergence$converged, "Model Converged!", "Model did not converge, results might be inaccurate."),
      "\nTopics: ", model$settings$dim$K,
      "\nIterations: ", model$convergence$its, "/", results$settings$maxIters,
      "\nLast relative change: ", sprintf("%.2e", change),
      " Convergence threshold: ", sprintf("%.0e", model$settings$convergence$em.converge.thresh),
      "\nDocuments in the model after preprocessing: ", model$settings$dim$N
      )
  })
  
  # Model convergence graph
  output$convergence <- renderPlot({
    validate(
      need(resultsr(), message = "Run analysis to see the results")
    )
    
    results <- resultsr()
    convergence <- as.data.frame(results$model$convergence)
    
    convergencePlot <- ggplot(data = convergence) + 
      geom_line(
        aes(x = seq_along(bound), y = bound)
      ) +
      theme(
        legend.position = "none",
        aspect.ratio = 0.5,
        text = element_text(size = PlotSettings$textSize),
        axis.text = element_text(size = PlotSettings$axisSize),
        plot.subtitle = element_text(size = PlotSettings$subtitleSize)
      ) + 
      ggtitle("Model convergence") + 
      xlab("Iteration") + ylab("Bounds")
    convergencePlot
  })
  
  # Document topic graph
  output$documentSummary <- renderPlot({
    validate(
      need(documentMapr(), message = "Run analysis to see the results"),
      need(resultsr(), message = FALSE)
    )
    
    reducedDimensionsDataframe <- documentMapr()    
    
    results <- resultsr()
    model <- results$model
    
    topicCount <- model$settings$dim$K
    
    centers <- sapply(1:topicCount,
                      function(i) c(mean(reducedDimensionsDataframe$x[reducedDimensionsDataframe$topic == i]),
                                    mean(reducedDimensionsDataframe$y[reducedDimensionsDataframe$topic == i])))
    
    centersDataframe <- data.frame(centers[1, ], centers[2, ], c(1:topicCount))
    colnames(centersDataframe) <- c("x", "y", "topic")
    
    summaryPlot <- ggplot(data = reducedDimensionsDataframe) +
      geom_point(aes(
        x = x,
        y = y,
        size = size,
        group = factor(topic),
        color = factor(topic),
        alpha = 0.7
      ),
        na.rm = TRUE
      ) +
      scale_radius(range = c(5 / sqrt(topicCount), 50 / sqrt(topicCount))) + 
      
      geom_label(data = centersDataframe, aes(
        x = x,
        y = y,
        label = paste("Topic",topic),
        group = topic,
        color = factor(topic)
      ),
        na.rm = TRUE,
        size = PlotSettings$geomTextSize
      ) + 
      theme(
        legend.position = "none",
        aspect.ratio = 1,
        text = element_text(size = PlotSettings$textSize),
        axis.text = element_text(size = PlotSettings$axisSize),
        plot.subtitle = element_text(size = PlotSettings$subtitleSize)
      ) + 
      ggtitle("Topic-document relation") + 
      labs(subtitle = "2D projection of document topic proportions by topics\n(structural topic model theta matrix)") +
      xlab("") + ylab("")
    
    summaryPlot
  })
  
  # Document summary click
  output$documentSummaryClick <- renderUI({
    validate(
      need(documentMapr(), message = FALSE),
      need(resultsr(), message = FALSE)
    )
    click <- input$documentClick
    documentMap <- isolate(documentMapr())
    results <- isolate(resultsr())
    
    closePoints <- nearPoints(documentMap, click,
                              xvar = "x",
                              yvar = "y",
                              maxpoints = 1,
                              threshold = PlotSettings$clickThreshold)
    
    if(nrow(closePoints) == 0) {
      return(NULL)
    }
    index <- as.numeric(row.names(closePoints))
    document <- as.character(results$data$meta$documents[index])
    proportions <- results$model$theta[index, ]
    names(proportions) <- c(1:length(proportions))
    sortedProportions <- sort(proportions, decreasing = TRUE)
    
    textOut <- sapply(1:length(sortedProportions), function(i) {
                        topicProportion <- sortedProportions[i]
                        roundedProportion <- round(topicProportion * 100, 0)
                        if(roundedProportion > 0) {
                          tagList(
                            tags$span(tags$strong("Topic", names(topicProportion)),
                                   paste0(roundedProportion, "%",
                                          ifelse(i == length(sortedProportions), "", ",")))
                          )
                        }
                      })

    tagList(
      fluidRow(
        column(width = 8, offset = 2,
          wellPanel(
            tags$p(tags$strong("Document"), index),
            tags$p(textOut),
            tags$strong("Text:"),
            tags$p(document)
          )
        )
      )
    )
  })
  
  # Topic distance graph
  output$topicSummary <- renderPlot({
    validate(
      need(resultsr(), message = "Run analysis to see the results"),
      need(topicDistancesr(), message = "Run analysis to see the results")
      )
    
    results <- resultsr()
    model <- results$model
    
    topicCount <- model$settings$dim$K
    distanceMatrix <- topicDistancesr()
    
    margins <- 0.1
    colorPalette <- rev(wes_palette("Zissou1", 10, type = "continuous"))
        
    summaryPlot <- ggplot(data = distanceMatrix) +
      geom_point(aes(
        x = x,
        y = y,
        size = size,
        group = factor(topic),
        color = sentiment,
        alpha = 0.7
      )) + 
      scale_radius(range = c(20 / sqrt(topicCount), 100 / sqrt(topicCount)),
                    limits = c(0, max(distanceMatrix$size))) + 
      geom_text(aes(
        x = x,
        y = y,
        label = paste("Topic", topic)
      ),
        size = PlotSettings$geomTextSize
      ) +
      lims(x = c(min(distanceMatrix$x) - margins,
                 max(distanceMatrix$x + margins)), 
           y = c(min(distanceMatrix$y) - margins,
                 max(distanceMatrix$y + margins))) + 
      scale_color_gradientn(colors = colorPalette,
                            limits = c(0, 1),
                            breaks = c(0, 0.5, 1),
                            labels = c("Negative", "Mixed", "Positive")
      ) + 
      guides(size = FALSE, alpha = FALSE) + 
      theme(
        legend.position = "right",
        aspect.ratio = 1,
        text = element_text(size = PlotSettings$textSize),
        axis.text = element_text(size = PlotSettings$axisSize),
        plot.subtitle = element_text(size = PlotSettings$subtitleSize)
      ) + 
      ggtitle("Topic distance map") + 
      labs(subtitle = "2D projection of topic distances based on the vocabulary used by the topics\n(structural topic model beta matrix)") +
      xlab("") + ylab("")
    
    summaryPlot
  })
  
  # Topic distance click
  output$topicSummaryDetails <- renderUI({
    validate(
      need(topicDistancesr(), message = FALSE),
      need(resultsr(), message = FALSE)
    )
    click <- input$summaryClick
    distanceMatrix <- isolate(topicDistancesr())
    results <- isolate(resultsr())
    
    closePoints <- nearPoints(distanceMatrix, click,
                              xvar = "x",
                              yvar = "y",
                              maxpoints = 1,
                              threshold = PlotSettings$clickThreshold)
    
    if(nrow(closePoints) == 0) {
      return(NULL)
    }
    
    model <- results$model
    vocabularyLength <- length(model$vocab)
    additionalInfo <- NULL
    if(PlotSettings$keyWords > vocabularyLength) {
      additionalInfo <- tagList(tags$p(class = "text-danger", tags$strong("Warning:"),
                                       paste0("Vocabulary size is ", vocabularyLength, ", only showing ",
                                              vocabularyLength, " keywords"))
      )
    }
    
    topicLabels <- labelTopics(model, n = clamp(PlotSettings$keyWords, 1, vocabularyLength))
    topic <- closePoints$topic
    textOut <- keywordsTextFormat(topicLabels, topic)
    
    sentiment <- "No sentiment"
    if(!is.na(closePoints$sentiment)) {
      sentiment <- paste0(round(closePoints$sentiment * 100, 0), "% positive")
    }
    
    tagList(
      fluidRow(
        column(width = 8, offset = 2,
          wellPanel(
            tags$p(tags$strong(paste("Topic", topic)),
                   "Proportion:", paste0(round(closePoints$size * 100, 0), "%")),
            tags$p(tags$strong("Sentiment:"), sentiment),
            tags$strong("Keywords:"),
            textOut,
            additionalInfo
          )
        )
      )
    )
  })
  
  output$emotionSummary <- renderPlot({
    validate(
      need(resultsr(), message = "Run analysis to see the results")
    )
    
    results <- resultsr()
    emotion <- ggplottableSentimentFormat(results$sentiment)[1:8, ]
    
    validate(
      need(!is.na(emotion$percentage), message = "No emotions were identified")
    )
    
    emotionPlot <- sentimentBarPlot(emotion,
                                    orderData = TRUE,
                                    plotTitle = "Emotion summary",
                                    plotSubtitle = "Emotion analysis using NRC lexicon")
    emotionPlot
  })
  
  output$sentimentSummary <- renderPlot({
    validate(
      need(resultsr(), message = "Run analysis to see the results")
    )

    
    results <- resultsr()
    sentiment <- ggplottableSentimentFormat(results$sentiment)[9:10, ]
    
    validate(
      need(!is.na(sentiment$percentage), message = "Sentiment was not identified")
    )
    
    sentimentPlot <- sentimentBarPlot(sentiment,
                                      orderData = FALSE,
                                      plotTitle = "Sentiment summary",
                                      plotSubtitle = "Sentiment analysis using NRC lexicon")
    sentimentPlot
  })
}

sentimentBarPlot <- function(data, orderData = TRUE, plotTitle = "Summary", plotSubtitle = "", transparent = FALSE) {
  if(orderData) {
    data$emotion <- reorder(data$emotion, data$percentage)
  }
  
  sentimentPlot <- ggplot(data = data) +
    geom_bar(aes(
      x = emotion,
      y = percentage,
      fill = emotion
    ),
      stat = "identity",
      position = "dodge"
    ) + 
    geom_text(aes(
      x = emotion,
      y = percentage,
      label = paste0(round(percentage * 100, 0), "%")
    ),
      position = position_dodge(width = 1),
      hjust = 1.25,
      size = PlotSettings$geomTextSize
    ) + 
    coord_flip() + 
    theme(
      legend.position = "none",
      text = element_text(size = PlotSettings$textSize),
      axis.text = element_text(size = PlotSettings$axisSize),
      plot.subtitle = element_text(size = PlotSettings$subtitleSize)
    ) + 
    ggtitle(plotTitle) + 
    labs(subtitle = plotSubtitle) +
    xlab(label = NULL) + 
    ylab(label = "Percentage")
  
  if(transparent) {
    sentimentPlot <- sentimentPlot +
      theme(
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid = element_line(size = 0.25, linetype = "solid", color = "grey")
      )
  }
  
  return(sentimentPlot)
}
