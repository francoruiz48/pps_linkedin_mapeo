source("global.R")
source("server.R")
source("ui/ui.R")

library(shiny)

shinyApp(ui=ui, server=server)