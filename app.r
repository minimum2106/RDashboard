library(shiny)
# library(summarytools)

data_page <- tabsetPanel(type = "tabs",
                         tabPanel("View", tableOutput("contents")),
                         tabPanel("Summary", verbatimTextOutput("summary")),
                         tabPanel("Visualizations"),
                         id = "data_panels"
)

sidebar_data <- conditionalPanel(condition="input.data_panels == 'View'",       
                                 fileInput("data_file", "Choose CSV File",
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 
                                 # Horizontal line ----
                                 tags$hr(),
                                 
                                 # Input: Checkbox if file has header ----
                                 checkboxInput("data_header", "Header", TRUE),
                                 
                                 # Input: Select separator ----
                                 radioButtons("data_sep", "Separator",
                                              choices = c(Comma = ",",
                                                          Semicolon = ";",
                                                          Tab = "\t"),
                                              selected = ","),
                                 
                                 # Input: Select quotes ----
                                 radioButtons("data_quote", "Quote",
                                              choices = c(None = "",
                                                          "Double Quote" = '"',
                                                          "Single Quote" = "'"),
                                              selected = '"'),
                                 
                                 # Horizontal line ----
                                 tags$hr(),
                                 
                                 # Input: Select number of rows to display ----
                                 radioButtons("data_disp", "Display",
                                              choices = c(Head = "head",
                                                          All = "all"),
                                              selected = "head")
                        
)

sidebar_viz <- conditionalPanel(condition="input.data_panels == 'Visualizations'",
                                checkboxInput("dataHeader", "Header", TRUE),)


# Define UI for data upload app ----
ui <- fluidPage(
  navbarPage("RDashboard",
             tabPanel("Data",
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          sidebar_data,
                          sidebar_viz
                          # Input: Select a file ----
                         
                        ),
                        mainPanel(
                          # Output: Data file ----
                          data_page
                          
                          
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
    req(input$data_file)
    
    tryCatch(
      {
        df <- read.csv(input$data_file$datapath,
                       header = input$data_header,
                       sep = input$data_sep,
                       quote = input$data_quote)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    if(input$data_disp == "head") {
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
  
  # output$summary_table <- renderPlot({
  #   summarytools::dfSummary(dataset())
  # })
  
}


# Create Shiny app ----
shinyApp(ui, server)