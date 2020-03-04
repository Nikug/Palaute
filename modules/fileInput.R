 fileInputUI <- function(id) {
   ns <- NS(id)
   
   tagList(
    fluidRow(
      column(width = 6,
        fileInput(inputId = ns("file"), label = "File input")
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
     validate(need(input$file, message = FALSE),
              need(input$delimiter, message = FALSE))
     input$file
   })
   
   observe({
    updateTextInput(session, inputId = "delimiter", value = substring(input$delimiter, 1, 1))
   })
   
   csv <- reactive({
     read.csv(file()$datapath,
              sep = substring(input$delimiter, 1, 1),
              header = input$header,
              encoding = "UTF-8",
              check.names = FALSE)
   })
   
   observeEvent(csv(), {
     output$fileStats <- renderText({
       paste("File size: ", round(file()$size / 1024^2, 1), "Mb, Lines: ", nrow(csv()), sep = "")
     })
   })
   
   return(csv)
 }