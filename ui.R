# Modules
source("modules/fileInput.R")

ui <- navbarPage(title = "Course Analysis", inverse = TRUE,
  tabPanel(title = "Main",
    fileInputUI(id = "fileInput")
  ),
  tabPanel(title = "Summary"
           
  ),
  tabPanel(title = "Details"

  )
)