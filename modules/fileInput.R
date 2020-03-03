 fileInputUI <- function(id) {
   ns <- NS(id)
   
   wellPanel(
     fileInput(inputId = ns("file"), label = "File input"),
     textOutput(outputId = ns("fileStats"))
   )
 }
 
 fileInputFunction <- function(input, output, session) {
   file <- reactive({
     validate(need(input$file, message = FALSE))
     input$file
   })
   
   csv <- reactive({
     read.csv(file()$datapath)
   })
   
   observeEvent(csv(), {
     output$fileStats <- renderText({
       paste("File size: ", round(file()$size / 1024^2, 1), "Mb, Lines: ", nrow(csv()), sep = "")
     })
   })
   
   return(csv)
 }