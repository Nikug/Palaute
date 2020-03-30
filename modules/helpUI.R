helpUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 10, offset = 1,
        tags$h1("About"),
        tags$p("The source code for this project can be found below:"),
        tags$a("Project repository", href = "https://github.com/Nikug/course-analysis"),
        tags$hr()
      )
    )
  )
}

helpUIFunction <- function(input, output, session) {
  
}