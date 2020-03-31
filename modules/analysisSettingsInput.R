# Consts
Languages <- list(
  English = "en",
  Finnish = "fi"
)

Default <- list(
  language = Languages$fi,
  
  topicCount = 6,
  topicCountMin = 2,
  topicCountMax = 1000,
  
  rangeStart = 4,
  rangeEnd = 25,
  
  maxIters = 75,
  maxItersMin = 1,
  maxItersMax = 1000000,
  
  sampleSize = 100,
  sampleSizeMin = 1,
  sampleSizeMax = 1000000,
  
  calculateTopics = FALSE,
  useSampling = FALSE,
  useDefaults = TRUE
)

analysisSettingsInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$h2("Analysis options"),
    checkboxInput(inputId = ns("useDefaults"),
      label = "Use default options",
      value = Default$useDefaults),
    
    selectInput(inputId = ns("language"), label = "Data language",
                c("Finnish" = Languages$Finnish,
                  "English" = Languages$English)),
    
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
      fluidRow(
        conditionalPanel(condition = "input.calculateTopics == true", ns = ns,
          column(width = 6,
            numericInput(inputId = ns("rangeStart"), label = "Search start", 
                        min = Default$topicCountMin,
                        max = Default$topicCountMax,
                        value =  Default$rangeStart              
            )
          ),
          column(width = 6,
            numericInput(inputId = ns("rangeEnd"), label = "Search end", 
                        min = Default$topicCountMin,
                        max = Default$topicCountMax,
                        value =  Default$rangeEnd            
            )
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
    tags$hr(),
    tags$h3("Data preview"),
    div(class = "text-info",
        verbatimTextOutput(outputId = ns("dataPreview"))
    ),
    tags$hr()
  )
}

analysisSettingsFunction <- function(input, output, session, datar) {
  # Init settings
  settings <- reactiveValues()
   
  # Options preview
  output$optionsPreview <- renderText({
    if(input$useDefaults) {
      text <- paste("Data language: ", names(Languages[Languages == input$language]),
                    "\nTopic count: ", ifelse(Default$calculateTopics, "calculate", Default$topicCount),
                    "\nSample: ", ifelse(Default$useSampling, Default$sampleSize, "all"),
                    "\nMax iterations: ", Default$maxIters)
    } else {
      text <- paste("Data language: ", names(Languages[Languages == input$language]),
                    "\nTopic count: ", ifelse(input$calculateTopics, "calculate", input$topicCount),
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
      settings$rangeStart <- Default$rangeStart
      settings$rangeEnd <- Default$rangeEnd
      settings$useSampling <- Default$useSampling
      
      settings$language <- input$language
    } else {
      validate(
        need(is.numeric(input$topicCount), message = "Topic count is not numeric"),
        need(is.numeric(input$maxIterations), message = "Max iterations is not numeric"),
        need(is.numeric(input$sampleSize), message = "Sample size is not numeric"),
        need(is.numeric(input$rangeStart), message = "Search range start is not numeric"),
        need(is.numeric(input$rangeEnd), message = "Search range end is not numeric")
        )
      settings$topicCount <- clamp(input$topicCount, Default$topicCountMin, Default$topicCountMax)
      settings$maxIters <- clamp(input$maxIterations, Default$maxItersMin, Default$maxItersMax)
      settings$sampleSize <- clamp(input$sampleSize, Default$sampleSizeMin, Default$sampleSizeMax)
      
      settings$calculateTopics <- input$calculateTopics
      settings$rangeStart <- clamp(input$rangeStart, Default$topicCountMin, Default$topicCountMax)
      settings$rangeEnd <- clamp(input$rangeEnd, Default$topicCountMin, Default$topicCountMax)
      
      settings$useSampling <- input$useSampling
      
      settings$language <- input$language
    }
  })
  
  output$dataPreview <- renderText({
    data <- datar()
    rows <- nrow(data)
    cols <- ncol(data)
    
    if(rows == 0 | cols == 0){
      text <- paste("No data")
    } else {
      text <- paste("Data is ready\nRows:", rows, "\nColumns:", cols)
    }
    text
  })
  
  return(settings)
}

clamp <- function(value, min, max) {
  if(value < min) { value = min }
  else if(value > max) { value = max }
  return(value)
}