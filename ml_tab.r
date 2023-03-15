library(shiny)

source("ui_common.r", local = TRUE)

sidebar_ml_split_percentage <-  conditionalPanel(
  condition = "input.ml_spliting_options == 'Train Test Validation'",
  numericInput(
    "ml_train_percentage", 
    "Training Dataset Percentage:", 
    value = 0.7, 
    min = 0, 
    max = 1, 
    step = 0.1)
)

sidebar_ml <- sidebarPanel(
  selectInput(
    "ml_dataset",
    "Choose Dataset",
    choices = ""
  ),
  
  selectInput(
    "ml_target_options",
    "Target Column",
    choices = ""
  ),
  
  selectInput(
    "ml_var_options",
    "Explanatory Variables:",
    choices = "",
    multiple = TRUE,
  ),

  selectInput(
    "ml_model_options", 
    "Model", 
    choices = c(
      "Naive Bayes", 
      "Logistic Regression", 
      "SVM"
      )
  ),
  
  selectInput(
    "ml_spliting_options",
    "Train / Test Spliting Method:",
    choices = c("Train Test Validation", "Cross Validation")
  ),
  
  sidebar_ml_split_percentage,
  
  actionButton(
    "ml_run_model", "Run Model", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
  
)


ml_tab <-  sidebarLayout(
  sidebar_ml,
  mainPanel()
)