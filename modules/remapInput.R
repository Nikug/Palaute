# Constants
DefaultRemap <- list(
  inputRows = 5,
  truncate = 30,
  truncateHeader = 10,
  minInputRows = 2,
  maxInputRows = 1000000
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
    div(style = "overflow-x: scroll",
      tableOutput(outputId = ns("input"))
    ),
    tags$hr(),
    tags$h3("Remap"),
    div(id = ns("remapControls")),
    tags$hr(),
    tags$h3("Mapped file")
  )
}

remapInputFunction <- function(input, output, session, csv) {
  output$input <- renderTable({
    data <- csv()
    validate(
      need(input$inputRows >= DefaultRemap$minInputRows,
           message = paste("Need atleast", DefaultRemap$minInputRows,"rows")),
      need(input$inputRows <= nrow(data),
           message = paste("Can't be larger than the file. Rows in the file:", nrow(data)))
      )
    
    noEmptyCols <- data[!sapply(data, function(x) all(is.na(x)))]
    colnames(noEmptyCols) <- sapply(colnames(noEmptyCols), substring, 1, DefaultRemap$truncateHeader)
    
    firstRows <- noEmptyCols[1:input$inputRows, ]
    truncated <- sapply(firstRows, substring, 1, DefaultRemap$truncate)
    truncated
  })
}