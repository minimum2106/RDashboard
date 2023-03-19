library(shiny)

sidebar_svm_viz <- conditionalPanel(
  condition = "input.ml_panels == 'Visualizations' & input.ml_method == 'SVM'",
  selectInput(
    "svm_viz_var_1",
    "Choose Variable 1",
    choices = ""
  ),

  selectInput(
    "svm_viz_var_2",
    "Choose Variable 2",
    choices = ""
  ),

  actionButton(
    "svm_viz_gen",
    "Generate Visualization"
  )
)

svm_content <- conditionalPanel(
  condition = "input.ml_method == 'SVM'",
  plotOutput("svm_viz")
)

rf_content <- conditionalPanel(
  condition = "input.ml_method == 'Random Forest'",
  plotOutput("viz_rf_imp"),
  plotOutput("viz_roc")
)

ml_content <- tabsetPanel(
  type = "tabs",
  tabPanel("Summary", verbatimTextOutput("ml_summary")),
  tabPanel(
    "Visualizations", 
    svm_content,
    rf_content
  ),
  id = "ml_panels"
)



ml_tab <-  sidebarLayout(
  sidebarPanel(
    verbatimTextOutput("ml_info"),
    selectInput(
      "ml_method",
      "Machine Learning Model:",
      choices = c(
        "SVM",
        "Random Forest"
      )
    ),
    sidebar_svm_viz
  ),
  mainPanel(ml_content)
)

