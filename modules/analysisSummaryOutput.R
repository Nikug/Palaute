PlotSettings <- list(
  "textSize" = 16,
  "axisSize" = 14,
  "geomTextSize" = 14 / .pt,
  "subtitleSize" = 12,
  "plotHeight" = "700px",
  "plotWidth" = "100%",
  "barPlotWidth" = "70%"
)

analysisSummaryOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 6,
        tags$h2("Analysis statistics"),
        verbatimTextOutput(outputId = ns("statistics"))
      )
    ),
    tags$h2("Topic analysis summary"),
    fluidRow(
      column(width = 6, align = "left",
             plotOutput(outputId = ns("topicSummary"), width = PlotSettings$plotWidth, height = PlotSettings$plotHeight),
             tags$p(class = "small", align = "center",
                    "Each circle represents a topic",
                    br(),
                    "Circle size = Topic prevalence. Larger circles are more prevelant in the corpus",
                    br(),
                    "Colour = Topic sentiment (Grey means there is no sentiment)",
                    br(),
                    "Distance between the topics is calculated as the difference between the vocabulary used by a topic.
                    Topics away from each other use different words, whereas close topics share more words"
             )       
      ),
      column(width = 6, align = "left",
          plotOutput(outputId = ns("documentSummary"), width = PlotSettings$plotWidth, height = PlotSettings$plotHeight),
          
          tags$p(class = "small", align = "center",
                 "Each circle represents a document",
                 br(),
                 "Circle size = Topic proportion of the document. Larger circles have higher proportion of them dedicated to one topic",
                 br(),
                 "Colour = Indicates the topic that has the highest proportion of the document",
                 br(),
                 "Topic labels are placed on the mathematical center of the topic's documents"
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
    )
  )
}

analysisSummaryOutputFunction <- function(input, output, session, resultsr) {
  output$statistics <- renderText({
    results <- resultsr()
    model <- results$model
    text <- paste(
      ifelse(model$convergence$converged, "Model Converged!", "Model did not converge, results might be inaccurate."),
      "\nTopics:", model$settings$dim$K,
      "\nIterations:", model$convergence$its
      )
  })
  
  # Document topic graph
  output$documentSummary <- renderPlot({
    results <- resultsr()
    model <- results$model
    topicCount <- model$settings$dim$K
    perplexity <- floor(topicCount / 3 - 1)
    reducedDimensions <- Rtsne(model$theta, k = 2,
                               initial_dims = topicCount,
                               perplexity = ifelse(perplexity < 1, 1, perplexity),
                               max_iter = 500,
                               verbose = TRUE,
                               check_duplicates = FALSE)

    reducedDimensionsDataframe <- data.frame(list("x" = reducedDimensions$Y[, 1],
                                                  "y" = reducedDimensions$Y[, 2],
                                                  "size" = sapply(1:model$settings$dim$N, function(i) max(model$theta[i, ])),
                                                  "topic" = max.col(model$theta, ties.method = "first")
                                                  ))
    
    centers <- sapply(1:length(unique(reducedDimensionsDataframe$topic)),
                      function(i) c(mean(reducedDimensionsDataframe$x[reducedDimensionsDataframe$topic == i]),
                                    mean(reducedDimensionsDataframe$y[reducedDimensionsDataframe$topic == i])))
    centers <- rbind(centers, 1:topicCount)
    centersDataframe <- data.frame(t(centers))
    colnames(centersDataframe) <- c("x", "y", "topic")
    
    summaryPlot <- ggplot(data = reducedDimensionsDataframe) +
      geom_point(aes(
        x = x,
        y = y,
        size = size,
        group = factor(topic),
        color = factor(topic),
        alpha = 0.7
      )) +
      scale_radius(range = c(5 / sqrt(topicCount), 50 / sqrt(topicCount))) + 
      
      geom_label(data = centersDataframe, aes(
        x = x,
        y = y,
        label = paste("Topic",topic),
        group = topic,
        color = factor(topic)
      ),
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
  
  # Topic distance graph
  output$topicSummary <- renderPlot({
    results <- resultsr()
    model <- results$model
    
    margins <- 0.1
    colorPalette <- rev(wes_palette("Zissou1", 10, type = "continuous"))
    
    topicCount <- model$settings$dim$K

    distanceMatrix <- topicDistances(exp(model$beta$logbeta[[1]]))
    distanceMatrix <- cbind(distanceMatrix, "size" = sapply(1:topicCount,
                                                            function(i) sum(model$theta[, i]) / model$settings$dim$N))
    distanceMatrix <- cbind(distanceMatrix, "topic" = c(1:topicCount))
    distanceMatrix <- cbind(distanceMatrix, "sentiment" = sapply(results$topicSentiment, function(e) e$percentage[10]))
    
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
  
  output$emotionSummary <- renderPlot({
    results <- resultsr()
    emotion <- results$sentiment[1:8, ]
    
    emotionPlot <- sentimentBarPlot(emotion,
                                    orderData = TRUE,
                                    plotTitle = "Emotion summary",
                                    plotSubtitle = "Emotion analysis using NRC lexicon")
    emotionPlot
  })
  
  output$sentimentSummary <- renderPlot({
    results <- resultsr()
    sentiment <- results$sentiment[9:10, ]
    
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

