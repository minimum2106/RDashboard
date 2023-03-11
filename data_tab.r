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

sidebar_data_univar_density <- conditionalPanel(
  condition = "input.viz_type == 'Density'",
  selectInput("viz_type", "Viz Type", choices = "")
)

sidebar_viz <- conditionalPanel(
  condition="input.data_panels == 'Visualizations'",
  selectInput("viz_group", "General Type", choices = c("Univariate", "Bivariate")),
  selectInput("viz_type", "Viz Type", choices = ""),
  selectizeInput("viz_target", "Columns", choices = "", multiple = TRUE),
  sidebar_data_univar_density
)


data_content <- tabsetPanel(
  type = "tabs",
  tabPanel("View", tableOutput("contents")),
  tabPanel("Summary", verbatimTextOutput("summary")),
  tabPanel("Visualizations"),
  id = "data_panels"
)

data_tab <-  sidebarLayout(
  sidebarPanel(
    sidebar_data,
    sidebar_viz
  ),
  mainPanel(data_content)
)