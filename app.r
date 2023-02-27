library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "RDashboard"),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)