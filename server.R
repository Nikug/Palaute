library("shiny")

# Global options
options(
  shiny.maxRequestSize = 500 * 1024^2 # 500Mb
)

# r suffix for reactive variables
server <- function(input, output, session) {
  csvr <- callModule(fileInputFunction, id = "fileInput")
  analysisSettings <- callModule(analysisSettingsFunction, id = "analysisSettings")
  callModule(analysisInputFunction, id = "analysisInput", reactive({analysisSettings}))
  
}