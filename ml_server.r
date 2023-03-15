library(shiny)

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