sentimentAnalysisComparison <- function(documents, language) {
  if(language != "fi") {
    return()
  }
  
  # Analysis
  documentVector <- get_sentences(paste(documents, collapse = "\n"))
  
  stems <- text_tokens(documentVector, stemmer = syuzhetLanguage(language))
  stems <- sapply(stems, paste, collapse = " ")
  wordList <- strsplit(tolower(stems), "\\s|[[:punct:]]+")
  
  nrcData <- lapply(wordList,
                    get_nrc_values,
                    lexicon = stemmedFinnishNRCLexicon)
  
  nrcVectorStemmed <- as.data.frame(do.call(rbind, nrcData), stringsAsFactors = FALSE)
  nrcVectorStemmed <- nrcVectorStemmed[, nrcColumnOrder]
  
  stemmedColumnSums <- colSums(nrcVectorStemmed)
  print("Stemmed:")
  print(stemmedColumnSums)
  print(paste("Total matches:", sum(stemmedColumnSums)))
  
  nrcVector <- get_nrc_sentiment(documentVector, language = syuzhetLanguage(language))
  
  columnSums <- colSums(nrcVector)
  print("Original")
  print(columnSums)
  print(paste("Total matches:", sum(columnSums)))
  
  return(nrcVector)
}