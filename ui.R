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
        analysisSettingsInput(id = "analysisSettings")
      ),
      column(width = 8,
        fileInputUI(id = "fileInput")
      )
    )
  ),
  tabPanel(title = "Summary"
           
  ),
  tabPanel(title = "Details"

  )
)