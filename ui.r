library(shiny)
library(shinyalert)
library(summarytools)

source("data_tab.r", local = TRUE)
source("fe_tab.r", local = TRUE)
source("ml_tab.r", local = TRUE)

source("ui_common.r", local = TRUE)

ui <- fluidPage(
  navbarPage(
    "RDashboard",
    tabPanel( "Data", data_tab),
    tabPanel("Feature Engineering", fe_tab),
    tabPanel("Model", ml_tab),
  )
)
