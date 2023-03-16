library(DT)

source("ui_common.r", local = TRUE)

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

sidebar_fe_mv_cat_customize <- conditionalPanel(
  condition = "input.fe_mutate_mv == 'Customize'",
  textInput(
    'fe_mutate_mv_customize', "Value Names", 
    placeholder = 'Enter the value that you want to replace missing value with')
)

sidebar_fe_mv <- conditionalPanel(
  condition = "input.fe_options == 'Handle Missing Values'",
  selectInput(
    "fe_mutate_mv", "Methods",
    choices = ""
  ),
  sidebar_fe_mv_cat_customize
)

sidebar_fe_not_filter <- conditionalPanel(
  condition = "input.fe_options != 'Filter'",
  selectInput(
    "fe_columns", 
    "Columns", 
    choices = ""
  ),
)

sidebar_fe_cdatatype <- conditionalPanel(
  condition = "input.fe_options == 'Change Datatype'",
  selectInput(
    "fe_cd_methods",
    "Change to:",
    choices = c(
      "Integer",
      "Numeric",
      "Factor",
      "Character"
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
  
  actionButton(
    "fe_add_dataset", "Add this dataset", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
  
  tags$hr(),
  selectInput(
    "fe_options", 
    "Transformation", 
    choices = c(
      "Filter", 
      "Mutate", 
      "Encoding", 
      "Rename", 
      "Handle Missing Values",
      "Remove Column",
      "Change Datatype")
  )
  , 
  sidebar_fe_not_filter,
  
  # additional sidebars for different mutating scenarios
  sidebar_fe_filter,
  sidebar_fe_mutate,
  sidebar_fe_encoding,
  sidebar_fe_rename,
  sidebar_fe_mv,
  sidebar_fe_cdatatype,
  
  # launch the transformation
  actionButton(
    "fe_transform", "Transform", width = "100%",
    style="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  ),
  verbatimTextOutput(
    "fe_transformation_log",
  ),
  tags$head(tags$style("#fe_transformation_log{ overflow-y:scroll; max-height: 300px}"))
)


fe_tab <- sidebarLayout(
  sidebar_fe, 
  mainPanel(
    DT::dataTableOutput("fe_datatable")
))
