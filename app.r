library(shiny)
library(shinydashboard)
library(shinyWidgets)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Load Data", icon = icon("bar-chart-o"),
             # Input: Select a file ----
             fileInput("file1", "Choose CSV File",
                       multiple = FALSE,
                       accept = c("text/csv",
                                  "text/comma-separated-values,text/plain",
                                  ".csv")),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Checkbox if file has header ----
             checkboxInput("header", "Header", TRUE),
             
             # Input: Select separator ----
             radioButtons("sep", "Separator",
                          choices = c(Comma = ",",
                                      Semicolon = ";",
                                      Tab = "\t"),
                          selected = ","),
             
             # Input: Select quotes ----
             radioButtons("quote", "Quote",
                          choices = c(None = "",
                                      "Double Quote" = '"',
                                      "Single Quote" = "'"),
                          selected = '"'),
             
             # Horizontal line ----
             tags$hr(),
             
             # Input: Select number of rows to display ----
             radioButtons("disp", "Display",
                          choices = c(Head = "head",
                                      All = "all"),
                          selected = "head")
    ),
    menuItem("EDA", tabName = "eda_tab", icon = icon("dashboard")),
    menuItem("Machine Learning", icon = icon("th"), tabName = "ml_tab",
             badgeLabel = "new", badgeColor = "green")
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "eda_tab",
            h2("EDA tab content"),
            tableOutput("contents")
    ),
    
    tabItem(tabName = "ml_tab",
            h2("Machine Learning tab content")
    )
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "Simple tabs"),
  sidebar,
  body
)

# Put them together into a dashboardPage
ui <- dashboardPage(
  dashboardHeader(title = "RDashboard"),
  sidebar,
  body
)

server <- function(input, output) { 
  dataset <- reactive({
    req(input$file1)
    
    tryCatch(
      {
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  output$contents <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    dataset()
    
  })
}

shinyApp(ui, server)