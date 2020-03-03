library("shiny")

# Global options
options(
  shiny.maxRequestSize = 500 * 1024^2 # 500Mb
)

# r suffix for reactive variables
server <- function(input, output, session) {
  csvr <- callModule(fileInputFunction, id = "fileInput")
  analysisSettings <- callModule(analysisSettings, id = "analysisSettings")
  
}