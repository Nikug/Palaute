 fileInputUI <- function(id) {
   ns <- NS(id)
   
   tagList(
    fluidRow(
      column(width = 6,
        fileInput(inputId = ns("file"), label = "File input", accept = c(
          "text/csv",
          "text/comma-separated-values, text/plain",
          ".csv"
        ))
      ),
      column(width = 3,
        textInput(inputId = ns("delimiter"), label = "CSV delimiter", value = ";")
      ),
      column(width = 3,
        checkboxInput(inputId = ns("header"), label = "Header", value = TRUE)
      )
    ),
    div(class = "text-info",
      textOutput(outputId = ns("fileStats"))
    )
   )
 }
 
 fileInputFunction <- function(input, output, session) {
   file <- reactive({
     validate(need(input$file, message = "There is no data"),
              need(input$delimiter, message = "No delimiter"))
     input$file
   })
   
   observe({
    updateTextInput(session, inputId = "delimiter", value = substring(input$delimiter, 1, 1))
   })
   
   csv <- reactive({
     validate(
       need(input$file, message = "There is no file")
       )
     tryCatch({
       fileEncoding <- detect_file_enc(file()$datapath)
       csv <- read.csv(file()$datapath,
                sep = substring(input$delimiter, 1, 1),
                header = input$header,
                encoding = fileEncoding,
                check.names = FALSE)
       csv
     },
     error = function(e) {
       removeUI(selector = "#remapNumControls > ", multiple = TRUE)
       removeUI(selector = "#remapTextControls > ", multiple = TRUE)
       validate("File is in incorrect format")
     },
     warning = function(w) {
       removeUI(selector = "#remapNumControls > ", multiple = TRUE)
       removeUI(selector = "#remapTextControls > ", multiple = TRUE)
       validate("There was an issue with reading the file")
       
     })
   })
   
   observeEvent(csv(), {
     output$fileStats <- renderText({
       paste("File size: ", round(file()$size / 1024^2, 1), "Mb, Lines: ", nrow(csv()), sep = "")
     })
   })
   
   return(csv)
 }