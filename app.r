library(shiny)

source("ui.r", local = TRUE)
source("server.r", local = TRUE)


shinyApp(
  ui, 
  server
)