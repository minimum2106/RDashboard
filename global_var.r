library(shiny)

source("data_tab.r", local = TRUE)

temporary_dataset <- reactiveValues(data = data.frame(), transformations = list())

original_dataset <- reactive({
    req(input$data_file)
    
    tryCatch(
      {
        df <- read.csv(input$data_file$datapath,
                       header = input$data_header,
                       sep = input$data_sep,
                       quote = input$data_quote)
        
        temporary_dataset$data <- df 
      },
      error = function(e) {
        stop(safeError(e))
      }
      
    )
})


dataset_collection <- reactiveValues(
    names = c("Originals"),
    dataset_config = list(Originals = list())
)
  
  