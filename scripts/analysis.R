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
  
  return(emotionSumDataframe)
}

topicDocuments <- function(model, data, settings) {
  topicProportions <- round(colSums(model$theta), 0)

  documentList <- vector(mode = "list", length = model$settings$dim$K)
  
  for(i in 1:length(topicProportions)) {
    documents <- findThoughts(model, texts = as.character(data$meta$documents), topics = i, n = topicProportions[i])
    documentList[[i]] <- documents$docs
    names(documentList[[i]]) <- paste0("topic", i)
  }
  return(documentList)
}

syuzhetLanguage <- function(language) {
  return (tolower(names(Languages[Languages == language])))
}