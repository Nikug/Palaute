# Libraries
Packages <- c("dplyr",
              "stm",
              "syuzhet",
              "ggplot2",
              "proxy",
              "wesanderson",
              "Rtsne",
              "corpus",
              "scales",
              "uchardet",
              "shiny")

NewPackages <- Packages[!(Packages %in% installed.packages()[, "Package"])]
if(length(NewPackages)) {
  install.packages(NewPackages)
}
lapply(Packages, library, character.only = TRUE)

# Global options
options(
  shiny.maxRequestSize = 500 * 1024^2 # 500Mb
)

# r suffix for reactive variables
server <- function(input, output, session) {
  callModule(helpUIFunction, id = "helpUI")
  csvr <- callModule(fileInputFunction, id = "fileInput")
  datar <- callModule(remapInputFunction, id = "remapInput", csvr)
  
  analysisSettings <- callModule(analysisSettingsFunction, id = "analysisSettings", datar)
  analysisResults <- callModule(analysisInputFunction, id = "analysisInput", reactive({analysisSettings}), datar)
  
  callModule(analysisSummaryOutputFunction, id = "analysisSummary", reactive({analysisResults$results}))
  callModule(analysisDetailsOutputFunction, id = "detailsSummary", reactive({analysisResults$results}))
  
}