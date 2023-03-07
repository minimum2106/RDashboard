library(shiny)
library(shinyalert)
library(stringr)

#shinyalert
# library(summarytools)

data_page <- tabsetPanel(type = "tabs",
                         tabPanel("View", tableOutput("contents")),
                         tabPanel("Summary", verbatimTextOutput("summary")),
                         tabPanel("Visualizations"),
                         id = "data_panels"
)

sidebar_data <- conditionalPanel(
  condition="input.data_panels == 'View'",       
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



sidebar_fe <- sidebarPanel(
  selectInput(
    "fe_dataset",
    "Choose Dataset",
    choices = ""
  ),
  actionButton("fe_add_dataset", "Add this dataset"),
  tags$hr(),
  selectInput(
    "fe_options", 
    "Transformation", 
    choices = c("Filter", "Normalize/Standardize", "Encoding", "Delete")
  ),
  actionButton("fe_transform", "Transform"),
  
)

# Define UI for data upload app ----
ui <- fluidPage(
  navbarPage("RDashboard",
             tabPanel("Data",
                      sidebarLayout(
                        sidebarPanel(
                          sidebar_data,
                          sidebar_viz
                        ),
                        mainPanel(data_page)
                      ),
                      
             ),
             tabPanel("Feature Engineering",
                      sidebarLayout(sidebar_fe, mainPanel(h3("hello")))),
             
             navbarMenu("Machine Learning",
                        "Supervised Learning",
                        tabPanel("Logistic Regression", 
                                 h3("test")),
                        tabPanel("Linear Regression"),
                        "----",
                        "Unsupervised Learning",
                        tabPanel("K-Means Clustering"),
                        tabPanel("DBScans"),
                        tabPanel("Hierarchical Clustering Analysis (HCA)")
                        
             )
             
  )
)


server <- function(session, input, output) {
  dataset <- reactive({
    req(input$data_file)
    
    tryCatch(
      {
        df <- read.csv(input$data_file$datapath,
                       header = input$data_header,
                       sep = input$data_sep,
                       quote = input$data_quote)
      },
      error = function(e) {
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
  
  chosen_dataset <- reactive({
    
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
  
  dataset_config <- reactiveValues(
    names = c("Original"),
    transformations = list()
  )
  
  dataset_test <- reactiveValues()
  
  
  observeEvent(
    input$data_file,
    updateSelectInput(
      session, "fe_dataset", "Choose Dataset",
      choices = dataset_config$names)
  )

  observeEvent(
    input$fe_add_dataset,
    shinyalert('hello', type='input', callbackR = callback_add_dataset)
  )
  
  temporary_transformation <- reactiveValues()

  callback_add_dataset <- function(value) {
    trimmed_value <- str_trim(value, side="both")
    
    dataset_config$names <- append(dataset_config$names, c(trimmed_value))
    dataset_config$transformations[[gsub(" ", "_", trimmed_value)]] <- list(
      "hello" = "hi"
    )
    
    updateSelectInput(
      session, "fe_dataset", "Choose Dataset",
      choices = dataset_config$names)
    
    print(dataset_config$transformations[[1]]$hello)
    
  }
  
  
  
  observeEvent(
    input$fe_transform,{
      temporary_transformation[[]] <- {
        
      }
    }
    
  )
  
}


# Create Shiny app ----
shinyApp(ui, server)