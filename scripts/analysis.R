# Consts
Verbose = TRUE

sampleDocuments <- function(data, sampleSize) {
  dataSample <- sample_n(data, sampleSize)
  return(dataSample)
}

preprocessDocuments <- function(data, settings) {
  preprocessedDocuments <- textProcessor(data$documents, metadata = data,
                                         language = settings$language)
  preparedDocuments <- prepDocuments(preprocessedDocuments$documents,
                                     preprocessedDocuments$vocab,
                                     preprocessedDocuments$meta)
  return(preparedDocuments)
}

topicModelAnalysis <- function(data, settings) {
  
  topicCovariates <- names(data$meta %>% select(starts_with("topicCovariates")))
  
  topicCovariateFormula <- NULL
  if(length(topicCovariates) != 0) {
    topicCovariateFormula <- formula(paste0("~", topicCovariates[1]))
  }

  prevalenceCovariates <- names(data$meta %>% select(starts_with("prevalenceCovariates")))
  
  prevalenceCovariateFormula <- NULL
  if(length(prevalenceCovariates) != 0) {
    prevalenceCovariateFormula <- formula(paste0("~", paste(prevalenceCovariates, collapse = "+")))
  }
  
  model <- NULL
  if(settings$calculateTopics) {
    models <- selectModel(data$documents, data$vocab,
                        K = settings$topicCount,
                        max.em.its = settings$maxIters,
                        init.type = "LDA",
                        seed = 0,
                        data = data$meta,
                        verbose = Verbose,
                        runs = 50,
                        prevalence = prevalenceCovariateFormula,
                        content = topicCovariateFormula
    )
    semanticCoherences <- sapply(models$semcoh, sum)
    model <- models$runout[[which.max(semanticCoherences)]]
    print(paste("Selected model:", which.max(semanticCoherences)))
    print(str(model))
  } else {
    model <- stm(data$documents, data$vocab,
                 K = settings$topicCount,
                 max.em.its = settings$maxIters,
                 init.type = "Spectral",
                 seed = 0,
                 reportevery = 100,
                 data = data$meta,
                 verbose = Verbose,
                 prevalence = prevalenceCovariateFormula,
                 content = topicCovariateFormula
    )
  }
  
  return(model)
}

sentimentAnalysis <- function(documents, language) {
  # Analysis
  documentVector <- get_sentences(paste(documents, collapse = "\n"))
  nrcVector <- get_nrc_sentiment(documentVector, language = syuzhetLanguage(language))
  
  # Make ggplottable data frame
  emotionDataframe <- data.frame(t(nrcVector[, 1:8]))
  sentimentDataframe <- data.frame(t(nrcVector[, 9:10]))
  
  emotionSumDataframe <- data.frame(rowSums(prop.table(emotionDataframe)))
  sentimentSumDataframe <- data.frame(rowSums(prop.table(sentimentDataframe)))
  
  colnames(sentimentSumDataframe) <- colnames(emotionSumDataframe)
  emotionSumDataframe <- rbind(emotionSumDataframe, sentimentSumDataframe)
  
  names(emotionSumDataframe)[1] <- "percentage"
  emotionSumDataframe <- cbind("emotion" = rownames(emotionSumDataframe), emotionSumDataframe)
  rownames(emotionSumDataframe) <- NULL
  
  documentCount <- length(strsplit(as.character(documents), "\n\n", fixed = TRUE)[[1]])
  emotionSumDataframe <- cbind(emotionSumDataframe, "documentCount" = documentCount)
  
  return(emotionSumDataframe)
}

topicDocuments <- function(model, data, settings) {
  topicProportions <- round(colSums(model$theta), 0)

  documentList <- vector(mode = "list", length = model$settings$dim$K)
  
  for(i in 1:model$settings$dim$N) {
    index <- which.max(model$theta[i, ])
    documentList[[index]] <- paste(documentList[[index]], data$meta$documents[[i]], sep = "\n\n")
  }
  return(documentList)
}

syuzhetLanguage <- function(language) {
  return (tolower(names(Languages[Languages == language])))
}

topicDistances <- function(topicTerms) {
  # Rewritten from LDA vis
  distances <- proxy::dist(x = topicTerms, method = function(x, y) {
    m <- 0.5 * (x + y)
    lhs <- ifelse(x == 0, 0, x * (log(x) - log(m)))
    rhs <- ifelse(y == 0, 0, y * (log(y) - log(m)))
    distance <- 0.5 * sum(lhs) + 0.5 * sum(rhs)
    return(distance)
  })

  if(length(distances) < 2) {
    return(data.frame("x" = c(distances / 2, -distances / 2),
                      "y" = c(0, 0)))
  }
  # Dirty fix for Inf edge cases
  # All values *should* be between (0, 1) so replace Inf with max value
  distances[distances == Inf] <- 1
  reducedDimensionsDistances <- cmdscale(distances, k = 2)
  return(data.frame("x" = reducedDimensionsDistances[, 1],
                    "y" = reducedDimensionsDistances[, 2]))
}