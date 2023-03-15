library(shiny)

train_dataset <- reactiveVal()
test_dataset <- reactiveVal()

observe({
  cols <- colnames(temporary_dataset$data)
  
  updateSelectInput(
    session, inputId = "ml_target_options",
    choices = cols
    )
  
  updateSelectInput(
    session, inputId = "ml_var_options",
    choices = cols
    )
})

observeEvent(
  input$ml_spliting_options, 
  {
    req(input$data_file)
    
    if (input$ml_spliting_options == "Cross Validation") {
      
    }
    
    if (input$ml_spliting_options == "Train Test Validation") {
      sample <- sample(
        c(TRUE, FALSE), 
        nrow(temporary_dataset$data), 
        replace=TRUE, prob=c(input$ml_train_percentage, 1 - input$ml_train_percentage)
      )
      
      train_dataset(temporary_dataset$data[sample, ]) 
      test_dataset(temporary_dataset$data[!sample, ])
    }
  }
)