library(shiny)
library(shinyalert)
library(summarytools)

source("data_tab.r", local = TRUE)
source("fe_tab.r", local = TRUE)

# MACHINE LEARNING TABS
source("ml_ols_tab.r", local = TRUE)


ui <- fluidPage(
  navbarPage(
    "RDashboard",
    tabPanel( "Data", data_tab),
    tabPanel("Feature Engineering", fe_tab),
    navbarMenu("Machine Learning",
               "Supervised Learning",
               tabPanel("Logistic Regression"),
               tabPanel("Linear Regression"),
               "----",
               "Unsupervised Learning",
               tabPanel("K-Means Clustering"),
               tabPanel("DBScans"),
               tabPanel("Hierarchical Clustering Analysis (HCA)")
               
    )
    
  )
)
