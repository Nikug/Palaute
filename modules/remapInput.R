# Constants
DefaultRemap <- list(
  inputRows = 5,
  truncate = 40,
  
  truncateHeader = 100,
  minTruncateHeader = 0,
  maxTruncateHeader = 1000000,
  
  minInputRows = 2,
  maxInputRows = 1000000,
  factorDetectThreshold = 7,
  
  prevalenceCovariates = 2,
  
  automaticBuildLimit = 500,
  progressbarUpdateInterval = 1000
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
        tags$h3("Input data structure")
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
      ),
      column(width = 2, offset = 1,
        checkboxInput(inputId = ns("hideInput"), label = "Hide", value = FALSE)
      )
    ),
    conditionalPanel(condition = "input.hideInput == false", ns = ns,
      div(class = "small, table-responsive",
        tableOutput(outputId = ns("input"))
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(width = 3,
        tags$h3("Select column mapping")
      ),
      column(width = 2, offset = 7,
        checkboxInput(inputId = ns("hideControls"), label =  "Hide", value = FALSE)       
      )
    ),
    conditionalPanel(condition = "input.hideControls == false", ns = ns,
      fluidRow(
        column(width = 12,
          tags$p("Select how each column is used in the topic model analysis")       
        )
      ),
      fluidRow(
        column(width = 6,
          tags$h3("Text"),
          div(id = "remapTextControls")
        ),
        column(width = 6,
          tags$h3("Numeric"),
          div(id = "remapNumControls")
        )
      )
    ),
    
    tags$hr(),
    fluidRow(
      column(width = 3,
        tags$h3("Output data structure")
      ),
      column(width = 2,
        checkboxInput(inputId = ns("build"),
                      label = "Build the mapped file",
                      value = TRUE)
      ),
      column(width = 3,
      numericInput(inputId = ns("outputRows"), label = "Show rows",
                   min = DefaultRemap$minInputRows,
                   max = DefaultRemap$maxInputRows,
                   value = DefaultRemap$inputRows)
      ),
      column(width = 2,
        downloadButton(outputId = ns("download"), label = "Download as CSV")
      ),
      column(width = 2,
        checkboxInput(inputId = ns("hideOutput"), label = "Hide", value = FALSE)     
      )
    ),
    conditionalPanel(condition = "input.hideOutput == false", ns = ns,
      div(class = "small, table-responsive",
          tableOutput(outputId = ns("output"))
      )
    )
  )
}

remapInputFunction <- function(input, output, session, csv) {
  buildr <- reactiveVal(TRUE)
  
  # Update dataset
  datar <- reactive({
    data <- csv()
    noEmptyCols <- data[!sapply(data, function(x) all(is.na(x)))]
    colnames(noEmptyCols) <- sapply(1:length(colnames(noEmptyCols)),
                                    function(i)
                                      substring(
                                        paste(i, ". ", colnames(noEmptyCols)[i], sep = ""), 
                                        1, 
                                        input$truncateHeader)
                                    )
    noEmptyCols
  })
  
  observeEvent(datar(), {
    data <- datar()
    if(nrow(data) > DefaultRemap$automaticBuildLimit) {
      updateCheckboxInput(session, inputId = "build", value = FALSE)
      buildr(FALSE)
    }
  })
  
  observeEvent(input$build, {
    buildr(input$build)
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
    inputNames
  })
  
  # Reactive inputs
  remapInputsr <- reactive({
    inputNames <- remapInputNamesr()
    inputs <- list()
    inputs <- lapply(inputNames, function(inputName) paste0(inputs, input[[inputName]]))
    inputs
  })
  
  # Create remapping components
  observeEvent(remapInputNamesr(), {
    clearMappingControls()
    
    data <- datar()
    
    names <- colnames(data)
    ids <- remapInputNamesr()
    preCovCounter <- 0
    
    for(i in 1:length(names)) {
      uniques <- unique(data[, i])
      type = Types$doc
      inputName = ids[i]
      
      # Guess the data type
      if(length(uniques) <= 1) {
        type = Types$don
      } else if(length(uniques) <= DefaultRemap$factorDetectThreshold &
                preCovCounter < DefaultRemap$prevalenceCovariates) 
      {
        type = Types$pre
        preCovCounter <- preCovCounter + 1
      } else {
        type = Types$doc
      }
      
      if(is.numeric(data[, i]) &type == Types$doc) {
        if(preCovCounter < DefaultRemap$prevalenceCovariates) {
          type = Types$pre
          preCovCounter <- preCovCounter + 1
        } else {
          type = Types$don
        }
      }
      
      # Generate UI component
      label <- substring(names[i], 1, input$truncateHeader)
      ui <- selectInput(inputId = session$ns(inputName),
                        label = NULL,
                        c("Document" = Types$doc,
                          "Prevalence covariate" = Types$pre,
                          "Topic covariate" = Types$top,
                          "Don't include" = Types$don),
                        selected = type
      )
      
      # Insert UI component
      insertUI(selector = ifelse(is.numeric(data[, i]), "#remapNumControls", "#remapTextControls"),
               where = "beforeEnd", ui = 
        fluidRow(
          column(width = 6,
            tags$label(label)
          ),
          column(width = 6,
            ui
          )
        )
      )
    }
  })
  
  # Mapped file
  remapData <- reactive({
    mappings <- remapInputsr()
    data <- isolate(datar())
    dataRows <- nrow(data)
    
    validate(
      need(input$outputRows >= DefaultRemap$minInputRows,
           message = paste("Need atleast", DefaultRemap$minInputRows,"rows")),
      need(length(mappings) > 0,
           message = "Mappings have not loaded yet"),
      need(dataRows > 0,
           message = "There is no data"),
      need(buildr(), message = "You need to build the data")
    )
    
    
    # Create dataframe names
    dfnames <- c("documents")
    pres <- sum(mappings == Types$pre)
    tops <- sum(mappings == Types$top)
    documents <- sum(mappings == Types$doc)
    
    if(tops > 0) {
      dfnames <- c(dfnames, sapply(1:tops, function(i) paste0("topicCovariates", i)))
    }
    if(pres > 0) {
      dfnames <- c(dfnames, sapply(1:pres, function(i) paste0("prevalenceCovariates", i)))
    }
    
    mappedMatrix <- matrix(nrow = (dataRows * documents), ncol = length(dfnames))
    
    validate(
      need(nrow(mappedMatrix) > 0, message = "There is no data")
    )
    
    indicateProgress <- dataRows > DefaultRemap$automaticBuildLimit
    if(indicateProgress) {
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(0, message = "Building the mapped data: ")
      increment <- DefaultRemap$progressbarUpdateInterval / dataRows
    }
    
    # Create the new data frame
    iter <- 1
    for(r in 1:dataRows) {
      if(indicateProgress) {
        if(r %% DefaultRemap$progressbarUpdateInterval == 0) {
          progress$inc(increment, detail = paste0("Document: ", r, "/", dataRows))
        }
      }
      
      dataRow <- data[r, ]
      docs <- c()
      preCovs <- c()
      topCovs <- c()
      
      for(i in 1:length(mappings)) {
        if(mappings[i] == Types$doc) {
          doc <- as.character(dataRow[[i]])
          if(is.na(doc)) {
            next
          }
          
          if(nchar(doc) > 0) {
            docs <- c(docs, doc)
          } else {
            next
          }
        } else if(mappings[i] == Types$pre) {
          preCovs <- c(preCovs, dataRow[[i]])
        } else if(mappings[i] == Types$top) {
          topCovs <- c(topCovs, dataRow[[i]])
        }
      }
      
      for(d in docs) {
        newRow <- c(d, topCovs, preCovs)
        mappedMatrix[iter, ] <- newRow
        iter <- iter + 1
      }
    }
    
    mappedMatrix <- mappedMatrix[complete.cases(mappedMatrix), , drop = FALSE]
    mappedData <- as.data.frame(mappedMatrix[1:nrow(mappedMatrix), ])
    colnames(mappedData) <- dfnames
    mappedData
  })
  
  # The output table
  output$output <- renderTable({
    firstRows <- remapData()[1:input$outputRows, , drop = FALSE]
    firstRows
  })
  
  # File download
  output$download <- downloadHandler(
    filename = "Documents.csv",
    contentType = "text/csv",
    
    content = function(file) {
      data <- remapData()
      validate(
        need(data, message = "Nothing to download"),
        need(nrow(data) > 0, message = "Nothing to download")
      )
      write.csv(data, file, row.names = FALSE, encoding = "UTF-8")
    }
  )
  
  # Return value
  return(remapData)
}

clearMappingControls <- function() {
  removeUI(selector = "#remapNumControls > ", multiple = TRUE)
  removeUI(selector = "#remapTextControls > ", multiple = TRUE)
}