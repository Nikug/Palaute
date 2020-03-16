performAnalysis <- function(data, settings) {
  # Sample
  # Preprocess
  # Topic modeling
  # Sentiment analysis
  # Topic specific sentiment analysis
}

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
               prevalence = prevalenceCovariateFormula,
               content = topicCovariateFormula
  )
  
  return(model)
                              
}