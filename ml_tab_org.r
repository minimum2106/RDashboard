library(shiny)

source("ui_common.r", local = TRUE)

sidebar_ml_split_percentage <-  conditionalPanel(
  condition = "input.ml_spliting_options == 'Cross Validation'",
  numericInput(
    "ml_cross_validation_k",
    "Number of K:",
    value = 4,
  )
 
)

sidebar_ml_cross_validation <- conditionalPanel(
  condition = "input.ml_spliting_options == 'Cross Validation'",
)

sidebar_ml_svm_degree <- conditionalPanel(
  condition = "input.ml_model_options == 'SVM' & input.ml_svm_kernel == 'polynomial'",
  numericInput(
    "ml_svm_degree",
    "Degree",
    value = 3,
  ),
)

sidebar_ml_svm_gamma <- conditionalPanel(
  condition = "input.ml_svm_kernel != 'linear'",
  numericInput(
    "ml_svm_gamma",
    "Gamma",
    value = 0.2,
    min = 0.1,
    max = 1,
    step = 0.1
  ),
)

sidebar_ml_svm_coef0 <- conditionalPanel(
  condition = "input.ml_svm_kernel == 'polynomial' || input.ml_svm_kernel == 'sigmoid'",
  numericInput(
    "ml_svm_coef0",
    "Coefficient",
    value = 0,
  ),
)

sidebar_ml_svm <- conditionalPanel(
  condition = "input.ml_model_options == 'SVM'",
  selectInput(
    "ml_svm_kernel",
    "Kernel:",
    choices = c(
      "linear",
      "polynomial",
      "radial basis",
      "sigmoid"
    )
  ),
  
  numericInput(
    "ml_svm_cost",
    "Cost",
    value = 1,
  ),
  
  sidebar_ml_svm_degree,
  sidebar_ml_svm_gamma,
  sidebar_ml_svm_coef0
)

sidebar_ml <- conditionalPanel(
  condition = "input.ml_panels == 'Summary'",
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
  
  numericInput(
    "ml_train_percentage", 
    "Training Dataset Percentage:", 
    value = 0.7, 
    min = 0, 
    max = 1, 
    step = 0.1
  ),
  
  sidebar_ml_split_percentage,
  
  sidebar_ml_svm,
  
  actionButton(
    "ml_run_model", "Run Model", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
)

sidebar_ml_viz_svm <- conditionalPanel(
  condition = "input.ml_model_options == 'SVM'",
  selectInput(
    "ml_svm_viz_option_1",
    "X:",
    choices = ""
  ),

  selectInput(
    "ml_svm_viz_option_2",
    "Y:",
    choices = ""
  ),
  
  actionButton(
    "ml_svm_viz_generate", "Generate Visualization", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
)

sidebar_ml_viz <- conditionalPanel(
  condition = "input.ml_panels == 'Visualizations'",
  
  sidebar_ml_viz_svm
)

ml_content <- tabsetPanel(
  type = "tabs",
  tabPanel("Summary", verbatimTextOutput("ml_summary")),
  tabPanel("Visualizations", plotOutput("ml_viz")),
  id = "ml_panels"
)


ml_tab <-  sidebarLayout(
  sidebarPanel(
    sidebar_ml,
    sidebar_ml_viz
  ),
  mainPanel(ml_content)
)