library(shiny)

source("ui/ui.R")
source("server/server.R")

shinyApp(ui = app_ui, server = app_server)