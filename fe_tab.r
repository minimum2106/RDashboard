sidebar_fe_filter <- conditionalPanel(
  condition = "input.fe_options == 'Filter'",
  textInput(
    "fe_filter_condition", "Condition", 
    placeholder = "Enter predicament")
)

sidebar_fe_rename <- conditionalPanel(
  condition = "input.fe_options == 'Rename'",
  textInput("fe_rename_new_name", "New Name", placeholder = "Enter New Name of this Variable")
)

sidebar_fe_mutate <- conditionalPanel(
  condition = "input.fe_options == 'Mutate'",
  selectInput(
    'fe_mutate_options', "Function", 
    choices = c(
      "None", 
      "Log Transformation",
      "Standardization",
      "Normalization",
      "Exponential"
    )
  )
)

sidebar_fe_encoding <- conditionalPanel(
  condition = "input.fe_options == 'Encoding'",
  selectInput(
    "fe_encoding_options" , "Methods",
    choices = c(
      "One Hot Encoding",
      "Label Encoding"
    )
  )
)

# based sidebar for Feature Engineering page
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
    choices = c("Filter", "Mutate", "Encoding", "Rename")
  ),
  selectInput(
    "fe_columns", 
    "Columns", 
    choices = ""
  ),
  
  # additional sidebars for different mutating scenarios
  sidebar_fe_filter,
  sidebar_fe_mutate,
  sidebar_fe_encoding,
  sidebar_fe_rename,
  
  # launch the transformation
  actionButton("fe_transform", "Transform")
  
)


fe_tab <- sidebarLayout(sidebar_fe, mainPanel(
  tableOutput("fe_datatable")
))
