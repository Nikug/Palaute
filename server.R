# Libraries
library("shiny")
library("dplyr")
library("stm")
library("syuzhet")
library("LDAvis")
library("ggplot2")
library("proxy")
library("wesanderson")
library("Rtsne")

# Global options
options(
  shiny.maxRequestSize = 500 * 1024^2 # 500Mb
)

# r suffix for reactive variables
server <- function(input, output, session) {
  csvr <- callModule(fileInputFunction, id = "fileInput")
  datar <- callModule(remapInputFunction, id = "remapInput", csvr)
  
  analysisSettings <- callModule(analysisSettingsFunction, id = "analysisSettings", datar)
  analysisResults <- callModule(analysisInputFunction, id = "analysisInput", reactive({analysisSettings}), datar)
  
  callModule(analysisSummaryOutputFunction, id = "analysisSummary", reactive({analysisResults$results}))
  callModule(analysisDetailsOutputFunction, id = "detailsSummary", reactive({analysisResults$results}))
  
}