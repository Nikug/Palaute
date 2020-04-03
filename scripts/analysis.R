# Consts
Verbose = FALSE
STM <- list(
  "seed" = 0,
  "runs" = 50,
  "reportEvery" = 100
)

AnalysisSettings <- list(
  "updateProgressStep" = 2,
  "useStemmedFinnish" = TRUE
)

loadStemmedNRC <- function() {
  lexicon <- read.csv("data/stemmedFinnishNRC.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
  lexicon
}

stemmedFinnishNRCLexicon <- loadStemmedNRC()

nrcColumnOrder <- c(
  "anger",
  "anticipation",
  "disgust",
  "fear",
  "joy",
  "sadness",
  "surprise",
  "trust",
  "negative",
  "positive"
)

sampleDocuments <- function(data, sampleSize) {
  dataSample <- sample_n(data, sampleSize)
  return(dataSample)
}

preprocessDocuments <- function(data, settings) {
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(0, message = "Preprocessing text: ",
               detail = paste0("Preprocessing",
                               "\nThis can take minutes..."))
  
  stemmedDocuments <- text_tokens(data$documents, stemmer = settings$language)
  data$stemmedDocuments <- sapply(stemmedDocuments, paste, collapse = " ")
  
  preprocessedDocuments <- textProcessor(data$stemmedDocuments,
                                         metadata = data,
                                         language = settings$language,
                                         verbose = Verbose,
                                         stem = FALSE)
  progress$inc(0.5, detail = paste0("Preparing documents",
                                    "\nThis can take minutes..."))
  preparedDocuments <- prepDocuments(preprocessedDocuments$documents,
                                     preprocessedDocuments$vocab,
                                     preprocessedDocuments$meta,
                                     verbose = Verbose)
  return(preparedDocuments)
}

createModel <- function(data, settings, iterations, prevalenceCovariateFormula, topicCovariateFormula) {
  model <- stm(data$documents, data$vocab,
               K = settings$topicCount,
               max.em.its = iterations,
               init.type = "Spectral",
               seed = STM$seed,
               reportevery = STM$reportEvery,
               data = data$meta,
               verbose = Verbose,
               prevalence = prevalenceCovariateFormula,
               content = topicCovariateFormula
  )
  return(model)
}

iterateModel <- function(data, model, iterations, prevalenceCovariateFormula, topicCovariateFormula) {
  model <- stm(data$documents, data$vocab,
               K = model$settings$dim$K,
               max.em.its = model$settings$convergence$max.em.its + iterations,
               init.type = model$settings$init$mode,
               seed = model$settings$seed,
               reportevery = model$settings$topicreportevery,
               data = data$meta,
               verbose = Verbose,
               prevalence = prevalenceCovariateFormula,
               content = topicCovariateFormula,
               model = model
  )
  return(model)
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
  progress <- shiny::Progress$new()
  on.exit(progress$close())
  progress$set(message = "Structural topic model:", value = 0)
  
  if(settings$calculateTopics) {
    allModels <- list("out" = list(), "exclusivity" = list(), "semcoh" = list())

    iterations <- settings$rangeEnd - settings$rangeStart + 1
    for(i in settings$rangeStart : settings$rangeEnd) {
      
      if(i == settings$rangeStart) {
        progress$inc(0,
                     detail = paste0("Calculating models with ", i, " topics",
                                     "\nThis can take multiple minutes..."))
      } else {
        progress$inc(1 / iterations,
                     detail = paste0("Calculating models with ", i, " topics",
                                     "\nEstimated time left: ",
                                     timeDifference(startTime, endTime, settings$rangeEnd - i + 1)))
      }
      
      startTime <- Sys.time()
      arguments <- list(data$documents, data$vocab,
                    K = i,
                    max.em.its = settings$maxIters,
                    init.type = "LDA",
                    seed = STM$seed,
                    data = data$meta,
                    verbose = Verbose,
                    netverbose = Verbose,
                    reportevery = STM$reportEvery,
                    runs = STM$runs,
                    prevalence = prevalenceCovariateFormula,
                    content = NULL
                    )
      if(Verbose) {
        models <- do.call(manyTopics, arguments)
      } else {
        capture.output(
          models <- do.call(manyTopics, arguments)
        )
      }
      allModels$out <- rbind(allModels$out, models$out)
      allModels$exclusivity <- rbind(allModels$exclusivity, models$exclusivity)
      allModels$semcoh <- rbind(allModels$semcoh, models$semcoh)
      endTime <- Sys.time()
    }
    
    
    # Calculate best model as a max mean of semantic coherence and exclusivity
    semanticCoherences <- rescale(sapply(allModels$semcoh, sum), to = c(0, 1))
    exclusitivities <- rescale(sapply(allModels$exclusivity, sum), to = c(0, 1))
    model <- allModels$out[[which.max((semanticCoherences + exclusitivities) / 2)]]

  } else {
    progress$inc(0, detail = paste0("Iteration: 0/", settings$maxIters))
    startTime <- Sys.time()
    model <- createModel(data, settings, iterations = 1, prevalenceCovariateFormula, topicCovariateFormula)
    endTime <- Sys.time()
    i <- settings$maxIters - model$convergence$its
    while(i > 0) {
      iterations <- AnalysisSettings$updateProgressStep
      if(i < iterations) {
        iterations <- i
      }
      i <- i - iterations
      
      if(length(model$convergence$bound) > 1) {
        last <- length(model$convergence$bound)
        change <- (model$convergence$bound[last] - model$convergence$bound[last - 1]) / abs(model$convergence$bound[last - 1])
      } else {
        change <- 0
      }
      
      progress$inc(iterations / settings$maxIters,
                   detail = paste0("Iteration: ", settings$maxIters - i, "/", settings$maxIters,
                                   "\nEstimated time left: ", timeDifference(startTime, endTime, i),
                                   "\nRelative change: ", sprintf("%.2e", change), " Threshold: ",
                                   sprintf("%.0e", model$settings$convergence$em.converge.thresh)
                                   ))
      
      startTime <- Sys.time()
      model <- iterateModel(data, model, iterations = iterations, prevalenceCovariateFormula, topicCovariateFormula)
      endTime <- Sys.time()
      if(model$convergence$converged) {
        break
      }
    }

  }
  
  return(model)
}

sentimentAnalysis <- function(documents, language) {
  # Analysis
  documentVector <- get_sentences(paste(documents, collapse = "\n"))
  
  if(language == "fi" & AnalysisSettings$useStemmedFinnish) {
    stems <- text_tokens(documentVector, stemmer = syuzhetLanguage(language))
    stems <- sapply(stems, paste, collapse = " ")
    wordList <- strsplit(tolower(stems), "\\s|[[:punct:]]+")

    nrcData <- lapply(wordList,
                      get_nrc_values,
                      lexicon = stemmedFinnishNRCLexicon)
    
    nrcVector <- as.data.frame(do.call(rbind, nrcData), stringsAsFactors = FALSE)
    nrcVector <- nrcVector[, nrcColumnOrder]
  } else {
    nrcVector <- get_nrc_sentiment(documentVector, language = syuzhetLanguage(language))
  }
  return(nrcVector)
}
 
ggplottableSentimentFormat <- function(nrcVector) {
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

timeDifference <- function(start, end, iterationsLeft) {
  difference <- difftime(end, start) * iterationsLeft
  hours <- floor(as.numeric(difference, units = "hours"))
  minutes <- floor(as.numeric(difference, units = "mins"))
  seconds <- floor(as.numeric(difference, units = "secs"))
  text <- ""
  if(hours > 0) {
    text <- paste0(text, hours, "h ")
  }
  if(minutes > 0) {
    leftOverMinutes <- minutes - hours * 60
    text <- paste0(text, leftOverMinutes, "m ")
  }
  if(seconds >= 0) {
    leftOverSeconds <- seconds - minutes * 60
    text <- paste0(text, leftOverSeconds, "s ")
  }

  return(text)
}