 fileInputUI <- function(id) {
   ns <- NS(id)
   
   tagList(
    fileInput(inputId = ns("file"), label = "File input"),
    div(class = "text-info",
      textOutput(outputId = ns("fileStats"))
    )
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