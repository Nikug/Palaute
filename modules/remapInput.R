# Constants
DefaultRemap <- list(
  inputRows = 5,
  truncate = 40,
  
  truncateHeader = 20,
  minTruncateHeader = 0,
  maxTruncateHeader = 1000000,
  
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
        numericInput(inputId = ns("inputRows"), label = "Show rows",
                     min = DefaultRemap$minInputRows,
                     max = DefaultRemap$maxInputRows,
                     value = DefaultRemap$inputRows)
      ),
      column(width = 3,
             numericInput(inputId = ns("truncateHeader"), label = "Header length",
                          min = DefaultRemap$minTruncateHeader,
                          max = DefaultRemap$maxTruncateHeader,
                          value = DefaultRemap$truncateHeader)
      )
    ),
    div(class = "small, table-responsive",
      tableOutput(outputId = ns("input"))
    ),
    
    tags$hr(),
    tags$h3("Remap"),
    fluidRow(
      div(id = "remapControls")
    ),
    
    tags$hr(),
    tags$h3("Mapped file"),
    fluidRow(
      column(width = 3,
      numericInput(inputId = ns("outputRows"), label = "Show rows",
                   min = DefaultRemap$minInputRows,
                   max = DefaultRemap$maxInputRows,
                   value = DefaultRemap$inputRows)
      )
    ),
    div(class = "small, table-responsive",
        tableOutput(outputId = ns("output"))
    )
  )
}

remapInputFunction <- function(input, output, session, csv) {
  # Update dataset
  datar <- reactive({
    data <- csv()
    noEmptyCols <- data[!sapply(data, function(x) all(is.na(x)))]
    colnames(noEmptyCols) <- sapply(1:length(colnames(noEmptyCols)),
                                    function(i)
                                      substring(
                                        paste(i, ". ", colnames(noEmptyCols)[i], sep = "")
                                      , 1, input$truncateHeader)
                                    )
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
  
  # Reactive input names
  remapInputNamesr <- reactive({
    data <- datar()
    names <- colnames(data)
    inputNames <- lapply(1:length(names), function(i) paste0("mapDropdown", i))
  })
  
  # Reactive inputs
  remapInputsr <- reactive({
    inputNames <- remapInputNamesr()
    inputs <- list()
    inputs <- lapply(inputNames, function(x) paste0(inputs, input[[x]]))
  })
  
  # Remap
  observeEvent(remapInputNamesr(), {
    data <- datar()
    
    removeUI(selector = "#remapControls > ", multiple = TRUE)
    
    names <- colnames(data)
    ids <- remapInputNamesr()
    for(i in 1:length(names)) {
      uniques <- unique(data[, i])
      type = Types$doc
      inputName = ids[i]
      
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
      ui <- selectInput(inputId = session$ns(inputName),
                        label = substring(names[i], 1, input$truncateHeader),
                        c("Document" = Types$doc,
                          "Prevalence covariate" = Types$pre,
                          "Topic covariate" = Types$top,
                          "Don't include" = Types$don),
                        selected = type
      )
      
      # Insert UI component
      insertUI(selector = "#remapControls", where = "beforeEnd", ui = 
        column(width = 3,
          ui
        )
      )
    }
  })
  
  # Mapped file
  observeEvent(remapInputsr(), ignoreInit = TRUE, {
    data <- datar()
    mappings <- remapInputsr()
    
    docs <- c()
    
    for(i in 1:length(mappings)) {
      if(mappings[i] == Types$doc) {
        docs <- c(docs, as.character(data[, i]))
      }
    }
    
    df <- data.frame(docs)
    print(str(df))
    
  })
  
}