# Consts
Default <- list(
  topicCount = 6,
  topicCountMin = 2,
  topicCountMax = 100,
  
  maxIters = 75,
  maxItersMin = 1,
  maxItersMax = 1000000,
  
  sampleSize = 100,
  sampleSizeMin = 1,
  sampleSizeMax = 1000000,
  
  calculateTopics = TRUE,
  useSampling = FALSE,
  useDefaults = TRUE
)


analysisSettingsInput <- function(id) {
  ns <- NS(id)
  
  wellPanel(
    tags$h2("Analysis options"),
    checkboxInput(inputId = ns("useDefaults"),
      label = "Use default options",
      value = Default$useDefaults),
    
    conditionalPanel(condition = "input.useDefaults == false", ns = ns,
      
      tags$hr(),
      tags$h3("Options"),
      
      # Sampling
      fluidRow(
        column(width = 6,
          checkboxInput(inputId = ns("useSampling"), label = "Use sampling", value = Default$useSampling)
        ),
        column(width = 6,
          conditionalPanel(condition = "input.useSampling == true", ns = ns,
            numericInput(inputId = ns("sampleSize"), label = "Sample Size",
                         min = Default$sampleSizeMin, max = Default$sampleSizeMax, value = Default$sampleSize)
          )
        )
      ),
      
      # Topic count
      fluidRow(
        column(width = 6,
          checkboxInput(inputId = ns("calculateTopics"), label = "Calculate the number of topics",
                        value = Default$calculateTopics)
        ),
        column(width = 6,
          conditionalPanel(condition = "input.calculateTopics == false", ns = ns,
            numericInput(inputId = ns("topicCount"), label = "Topic count",
                         min = Default$topicCountMin, max = Default$topicCountMax, value = Default$topicCount)
          )
        )
      ),
      
      # Max iterations
      numericInput(inputId = ns("maxIterations"), label = "Maximum iterations",
                   min = Default$maxItersMin, max = Default$maxItersMax, value = Default$maxIters)
      
    ),
    
    tags$hr(),
    tags$h3("Options preview"),
    div(class = "text-info",
      verbatimTextOutput(outputId = ns("optionsPreview"))
    ),
    tags$hr()
  )
}

analysisSettings <- function(input, output, session) {
  # Init settings
  settings <- list(
    topicCount = Default$topicCount,
    maxIters = Default$maxIters,
    sampleSize = Default$sampleSize,
    
    calculateTopics = Default$calculateTopics,
    useSampling = Default$useSampling
  )
   
  # Options preview
  output$optionsPreview <- renderText({
    if(input$useDefaults) {
      text <- paste("Topic count: ", ifelse(Default$calculateTopics, "calculate", Default$topicCount),
                    "\nSample: ", ifelse(Default$useSampling, Default$sampleSize, "all"),
                    "\nMax iterations: ", Default$maxIters)
    } else {
      text <- paste("Topic count: ", ifelse(input$calculateTopics, "calculate", input$topicCount),
                    "\nSample: ", ifelse(input$useSampling, input$sampleSize, "all"),
                    "\nMax iterations: ", input$maxIterations)
    }
  })
  
  # Update settings
  observe({
    if(input$useDefaults) {
      settings$topicCount <- Default$topicCount
      settings$maxIters <- Default$maxIters
      settings$sampleSize <- Default$sampleSize
      
      settings$calculateTopics <- Default$calculateTopics
      settings$useSampling <- Default$useSampling
    } else {
      settings$topicCount <- clamp(input$topicCount, Default$topicCountMin, Default$topicCountMax)
      settings$maxIters <- clamp(input$maxIterations, Default$maxItersMin, Default$maxItersMax)
      settings$sampleSize <- clamp(input$sampleSize, Default$sampleSizeMin, Default$sampleSizeMax)
      
      settings$calculateTopics <- input$calculateTopics
      settings$useSampling <- input$useSampling
    }
  })
  
  return(settings)
}

clamp <- function(value, min, max) {
  if(value < min) { value = min }
  else if(value > max) { value = max }
  return(value)
}