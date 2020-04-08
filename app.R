library("shiny")

source("server.R")
source("ui.R")

shinyApp(server = server, ui = ui)