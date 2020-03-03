analysisSettingsInput <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    checkboxInput(inputId = ns("useDefaults"),
                  label = "Use default options",
                  value = TRUE)
  )  
}