library(DT)
library(summarytools)

sidebar_data <- conditionalPanel(
  condition="input.data_panels == 'View'",       
  fileInput(
    "data_file", "Choose CSV File",
    multiple = FALSE,
    accept = c("text/csv",
               "text/comma-separated-values,text/plain",
               ".csv"
    )
  ),
  
  selectInput(
    "view_dataset",
    "Choose Dataset",
    choices = ""
  ),
  
  tags$hr(),
  checkboxInput("data_header", "Header", TRUE),
  radioButtons(
    "data_sep", "Separator",
    choices = c(
      Comma = ",", 
      Semicolon = ";",
      Tab = "\t"
    ),
    selected = ","
  ),
  
  # Input: Select quotes ----
  radioButtons(
    "data_quote", "Quote",
    choices = c(
      None = "",
      "Double Quote" = '"',
      "Single Quote" = "'"),
    selected = '"'),
  
  # Horizontal line ----
  tags$hr(),
  
  # Input: Select number of rows to display ----
  radioButtons(
    "data_disp", "Display",
    choices = c(
      Head = "head",
      All = "all"
    ),
    selected = "head"
  )
)

sidebar_summary <- conditionalPanel(
  condition = "input.data_panels == 'Summary'",
  selectInput(
    "summary_dataset",
    "Choose Dataset",
    choices = ""
  ),
)

sidebar_data_univar_density <- conditionalPanel(
  condition = "input.viz_type == 'Density'",
  
)

sidebar_data_univar_dist <- conditionalPanel(
  condition = "input.viz_type == 'Distribution'",
)

sidebar_data_scatter <- conditionalPanel(
  condition = "input.viz_type == 'Scatter'",
  selectizeInput("viz_color", "Color column:", choices = ""),
  selectizeInput("viz_size", "Size Column", choices = ""),
)


sidebar_viz <- conditionalPanel(
  condition="input.data_panels == 'Visualizations'",
  
  selectInput(
    "viz_dataset",
    "Choose Dataset",
    choices = ""
  ),
  selectInput(
    "viz_type", "Viz Type", 
    choices = c(
      "Distribution",
      "Density",
      "Boxplot",
      "Scatter",
      "Density"
    )),
  selectizeInput("viz_var_1", "Variable 1:", choices = ""),
  selectizeInput("viz_var_2", "Variable 2:", choices = ""),
  
  sidebar_data_univar_dist,
  sidebar_data_univar_density,
  sidebar_data_scatter,
  
  actionButton(
    "viz_generate", "Generate Visualization", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
)



data_content <- tabsetPanel(
  type = "tabs",
  tabPanel("View", DT::dataTableOutput("contents")),
  tabPanel("Summary", htmlOutput("summary")),
  tabPanel("Visualizations", plotOutput("data_viz")),
  id = "data_panels"
)

data_tab <-  sidebarLayout(
  sidebarPanel(
    sidebar_data,
    sidebar_summary,
    sidebar_viz
  ),
  mainPanel(data_content)
)