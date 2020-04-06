# Libraries
library("shinythemes")

# Modules
source("modules/helpUI.R")
source("modules/fileInput.R")
source("modules/analysisSettingsInput.R")
source("modules/analysisInput.R")
source("modules/remapInput.R")
source("modules/analysisSummaryOutput.R")
source("modules/analysisDetailsOutput.R")

# Scripts
source("scripts/analysis.R")
source("scripts/sentimentLexiconComparison.R")

ui <- tagList(
  navbarPage(theme = shinytheme("cosmo"),
    title = "Palaute",
    windowTitle = "Palaute",
    inverse = TRUE,
    position = ("fixed-top"),
    selected = "Main",
    
    
    tabPanel(title = "Main",
      fluidRow(
        column(width = 4,
          div(class = "fixed",
            wellPanel(class = "scaleWidth",
              analysisSettingsInput(id = "analysisSettings"),
              analysisInput(id = "analysisInput")
            )
          )
        ),
        column(width = 8,
          wellPanel(
            fileInputUI(id = "fileInput")
          ),
          wellPanel(
            remapInput(id = "remapInput")
          )
        )
      )
    ),
    tabPanel(title = "Summary",
      analysisSummaryOutput(id = "analysisSummary")
    ),
    tabPanel(title = "Details",
      analysisDetailsOutput(id = "detailsSummary")
    ),
    tabPanel(title = "Help",
      helpUI(id = "helpUI")        
    ),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    )
  )
)