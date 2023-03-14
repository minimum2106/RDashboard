sidebar_ml_ols <- sidebarPanel(
  selectInput(
    "fe_dataset",
    "Choose Dataset",
    choices = ""
  ),
  actionButton("ml_ols_estimate", "Estimate Model"),
  selectizeInput("ml_ols_columns", "Columns", choices = "", multiple = TRUE),
)

ml_content <- tabsetPanel(
  type = "tabs",
  tabPanel("Summary", tableOutput("contents")),
  tabPanel("Predict", verbatimTextOutput("summary")),
  tabPanel("Visualizations"),
  id = "data_panels"
)

ml_ols_tab <-  sidebarLayout(
  sidebar_ml_ols,
  mainPanel(ml_content)
)