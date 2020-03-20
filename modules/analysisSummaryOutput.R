analysisSummaryOutput <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h1("Analysis summary"),
    fluidRow(
      column(width = 10, offset = 1,
        div(
          plotOutput(outputId = ns("summary"), width = "100%", height = "700px")
        ),
        div(
          plotOutput(outputId = ns("topicSummary"), width = "100%", height = "700px")
        )
      )
    )
  )
}

analysisSummaryOutputFunction <- function(input, output, session, resultsr) {
  output$summary <- renderPlot({

    
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
    centersDataframe
    
    summaryPlot <- ggplot(data = reducedDimensionsDataframe) +
      coord_equal() + 
      geom_point(aes(
        x = x,
        y = y,
        size = size,
        group = factor(topic),
        color = factor(topic),
        alpha = 0.7
      )) +
      scale_size_continuous(range = c(1, 20)) + 
      
      geom_label(data = centersDataframe, aes(
        x = x,
        y = y,
        label = paste("Topic",topic),
        group = topic,
        color = factor(topic)
      )) + 
      
      theme_minimal() + 
      theme(legend.position = "none") + 
      ggtitle("Document map") + 
      xlab("") + ylab("") +
      labs(subtitle = "Basically how much each document relates to a topic")
    
    summaryPlot
    # ggplot this:
    # Calculate some kind of 2D topic distance map
    # Calculate topic sizes similar to LDAvis using their formula
    # Set topic colors based on overall topic sentiment
    # Name topics with numbers
    # Hover/click to show information about a topic
    #   - Like most relevant words
    #   - or emotion analysis, sentiment score etc
  })
  
  output$topicSummary <- renderPlot({
    results <- resultsr()
    model <- results$model
    
    topicCount <- model$settings$dim$K
    distanceMatrix <- topicDistances(exp(model$beta$logbeta[[1]]))
    distanceMatrix <- cbind(distanceMatrix, "size" = sapply(1:topicCount,
                                                            function(i) sum(model$theta[, i]) / model$settings$dim$N))
    distanceMatrix <- cbind(distanceMatrix, "topic" = c(1:topicCount))
    distanceMatrix <- cbind(distanceMatrix, "sentiment" = sapply(results$topicSentiment, function(e) e$percentage[10]))
    
    print(distanceMatrix)
    
    summaryPlot <- ggplot(data = distanceMatrix) +
      geom_point(aes(
        x = x,
        y = y,
        size = size,
        group = factor(topic),
        color = sentiment,
        alpha = 0.7
      )) + 
      scale_size(range = c(5, 40)) + 
      geom_text(aes(
        x = x,
        y = y,
        label = paste("Topic", topic)
      )) +
      lims(x = c(min(distanceMatrix$x) - 0.1,
                 max(distanceMatrix$x + 0.1)), 
           y = c(min(distanceMatrix$y) - 0.1,
                 max(distanceMatrix$y + 0.1))) + 
      coord_equal() + 
      scale_color_gradient2(low = "red",
                            mid = "grey",
                            high = "green",
                            na.value = "black",
                            limits = c(0, 1),
                            midpoint = 0.5) + 
      theme_minimal() + 
      theme() + 
      ggtitle("Topic distance map") + 
      xlab("") + ylab("") +
      labs(subtitle = "Basically how different/similar the topics are to each other")
    summaryPlot
    
  })
}

