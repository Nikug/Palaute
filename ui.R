# Modules
source("modules/fileInput.R")
source("modules/analysisSettingsInput.R")
source("modules/analysisInput.R")
source("modules/reformatInput.R")

ui <- navbarPage(theme = shinytheme("cosmo"),
  title = "Course Analysis",
  inverse = TRUE,
  
  tabPanel(title = "Main",
    fluidRow(
      column(width = 4,
        wellPanel(
          analysisSettingsInput(id = "analysisSettings"),
          analysisInput(id = "analysisInput")
        )
      ),
      column(width = 8,
        wellPanel(
          fileInputUI(id = "fileInput")
        ),
        wellPanel(
          reformatInput(id = "reformatInput")
        )
      )
    )
  ),
  tabPanel(title = "Summary"
           
  ),
  tabPanel(title = "Details"

  )
)