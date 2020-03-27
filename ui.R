# Libraries
library("shinythemes")

# Modules
source("modules/fileInput.R")
source("modules/analysisSettingsInput.R")
source("modules/analysisInput.R")
source("modules/remapInput.R")
source("modules/analysisSummaryOutput.R")
source("modules/analysisDetailsOutput.R")

# Scripts
source("scripts/analysis.R")

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  navbarPage(theme = shinytheme("cosmo"),
    title = "Course Analysis",
    inverse = TRUE,
    
    
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
    )
  )
)