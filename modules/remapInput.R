# Constants
DefaultRemap <- list(
  inputRows = 5,
  truncate = 40,
  truncateHeader = 10,
  minInputRows = 2,
  maxInputRows = 1000000,
  factorDetectThreshold = 7
)

Types <- list(
  doc = "Document",
  pre = "Prevalence covariate",
  top = "Topic covariate",
  don = "Don't include"
)

remapInput <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 3,
        tags$h3("Input file")
      ),
      column(width = 3,
        numericInput(inputId = ns("inputRows"), label = "Example rows",
                     min = DefaultRemap$minInputRows,
                     max = DefaultRemap$maxInputRows,
                     value = DefaultRemap$inputRows)
      )
    ),
    div(class = "small", style = "overflow-x: scroll",
      tableOutput(outputId = ns("input"))
    ),
    tags$hr(),
    tags$h3("Remap"),
    fluidRow(
      div(id = "remapControls")
    ),
    tags$hr(),
    tags$h3("Mapped file")
  )
}

remapInputFunction <- function(input, output, session, csv) {
  # Update dataset
  datar <- reactive({
    data <- csv()
    noEmptyCols <- data[!sapply(data, function(x) all(is.na(x)))]
    colnames(noEmptyCols) <- sapply(colnames(noEmptyCols), function(x) substring(x,  1, DefaultRemap$truncateHeader))
    noEmptyCols
  })
  
  # Input table
  output$input <- renderTable({
    validate(
      need(input$inputRows >= DefaultRemap$minInputRows,
           message = paste("Need atleast", DefaultRemap$minInputRows,"rows")),
      need(input$inputRows <= nrow(datar()),
           message = paste("Can't be larger than the file. Rows in the file:", nrow(datar())))
      )
    
    
    firstRows <- datar()[1:input$inputRows, ]
    truncated <- sapply(firstRows, substring, 1, DefaultRemap$truncate)
    truncated
  })
  
  # Remap
  observeEvent(datar(), {
    data <- datar()
    
    names <- colnames(data)
    for(i in 1:length(names)) {
      uniques <- unique(data[, i])
      type = Types$doc
      
      # Guess the data type
      if(length(uniques) <= 1) {
        type = Types$don
      } else if(length(uniques) <= DefaultRemap$factorDetectThreshold) {
        type = Types$pre
      } else {
        type = Types$doc
      }
      
      if(is.numeric(data[, i]) & type == Types$doc) {
        type = Types$pre
      }
      
      # Generate UI component
      ui <- tagList(
        div(class = "text-nowrap",
          selectInput(inputId = session$ns(paste("map", i, sep = "")),
                      label = substring(names[i], 1, DefaultRemap$truncateHeader),
                      c("Document" = Types$doc,
                        "Prevalence covariate" = Types$pre,
                        "Topic covariate" = Types$top,
                        "Don't include" = Types$don),
                      selected = type
          )
        )
      )
      
      insertUI(selector = "#remapControls", where = "beforeEnd", ui = 
        column(width = 3,
          ui
        )
      )
    }
    
    
  })
  
}