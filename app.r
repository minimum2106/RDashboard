library(shiny)

# Define UI for data upload app ----
ui <- fluidPage(
  navbarPage("Navbar!",
             tabPanel("Data",
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Select a file ----
                          fileInput("dataFile", "Choose CSV File",
                                    multiple = FALSE,
                                    accept = c("text/csv",
                                               "text/comma-separated-values,text/plain",
                                               ".csv")),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Checkbox if file has header ----
                          checkboxInput("dataHeader", "Header", TRUE),
                          
                          # Input: Select separator ----
                          radioButtons("dataSep", "Separator",
                                       choices = c(Comma = ",",
                                                   Semicolon = ";",
                                                   Tab = "\t"),
                                       selected = ","),
                          
                          # Input: Select quotes ----
                          radioButtons("dataQuote", "Quote",
                                       choices = c(None = "",
                                                   "Double Quote" = '"',
                                                   "Single Quote" = "'"),
                                       selected = '"'),
                          
                          # Horizontal line ----
                          tags$hr(),
                          
                          # Input: Select number of rows to display ----
                          radioButtons("dataDisp", "Display",
                                       choices = c(Head = "head",
                                                   All = "all"),
                                       selected = "head")
                          
                        ),
                        mainPanel(
                          # Output: Data file ----
                          tableOutput("contents")
                          
                        )
                      ),
                      
             ),
             tabPanel("Machine Learning",
                      h2("hello"))
             
             # App title ----
             
             # Sidebar layout with input and output definitions ----
             
             # Main panel for displaying outputs ----
             
  )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  dataset <- reactive({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$dataFile)
    
    tryCatch(
      {
        df <- read.csv(input$dataFile$datapath,
                       header = input$dataHeader,
                       sep = input$dataSep,
                       quote = input$dataQuote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$dataDisp == "head") {
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

# Create Shiny app ----
shinyApp(ui, server)