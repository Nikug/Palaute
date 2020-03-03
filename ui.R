# Libraries
library("shinythemes")

# Modules
source("modules/fileInput.R")
source("modules/analysisSettingsInput.R")

ui <- navbarPage(theme = shinytheme("cosmo"),
  title = "Course Analysis",
  inverse = TRUE,
  
  tabPanel(title = "Main",
    fluidRow(
      column(width = 4,
        wellPanel(
          analysisSettingsInput(id = "analysisSettings")
        )
      ),
      column(width = 8,
        wellPanel(
          fileInputUI(id = "fileInput")
        )
      )
    )
  ),
  tabPanel(title = "Summary"
           
  ),
  tabPanel(title = "Details"

  )
)