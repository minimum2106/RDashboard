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

sidebar_data_univar_density <- conditionalPanel(
  condition = "input.viz_type == 'Density'",
  selectInput("viz_type", "Viz Type", choices = ""),
  
)

sidebar_viz <- conditionalPanel(
  condition="input.data_panels == 'Visualizations'",
  selectInput("viz_group", "General Type", choices = c("Univariate", "Bivariate")),
  selectInput("viz_type", "Viz Type", choices = ""),
  selectizeInput("viz_target", "Columns", choices = "", multiple = TRUE),
  sidebar_data_univar_density
  )





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
server <- function(session, input, output) {
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
  
  observeEvent(
    input$viz_group,
    updateSelectInput(session, "viz_type", "Viz Type", 
                      choices = {
                        viz_univar_choices <- c("Distribution", "Density", "Boxplot")
                        viz_bivar_choices <- c("Scatter", "Boxplot", "Correlation")
                        
                        if (input$viz_group == 'Univariate') {
                          viz_univar_choices
                        }else {
                          viz_bivar_choices
                        }
                      })
  )
  
  
  observe({
    if(input$viz_group == "Univariate"){
      updateSelectizeInput(session,
                           inputId = "viz_target",
                           choices = colnames(dataset()),
                           selected = NULL,
                           options = list(maxItems = 1))
    }
    else if(input$viz_group == "Bivariate"){
      updateSelectizeInput(session,
                           inputId = "viz_target",
                           choices = colnames(dataset()),
                           selected = NULL,
                           options = list(maxItems = ncol(dataset())))
    }
  })
  
  
  
  output$contents <- renderTable({
  
    dataset()
    
  })
  
 
  
  # output$summary_table <- renderPlot({
  #   summarytools::dfSummary(dataset())
  # })
  
}


# Create Shiny app ----
shinyApp(ui, server)