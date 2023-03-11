library(shinyalert)
library(tidyverse)
library(stringr)

source("global.r", local = TRUE)

server <- function(session, input, output) {
  
  # --------------- GLOBAL ------------------------
  temporary_dataset <- reactiveValues(data = "", transformations = list())
  
  original_dataset <- reactive({
    req(input$data_file)
    
    tryCatch(
      {
        df <- read.csv(input$data_file$datapath,
                       header = input$data_header,
                       sep = input$data_sep,
                       quote = input$data_quote)
        
        temporary_dataset$data <- df 
      },
      error = function(e) {
        stop(safeError(e))
      }
      
    )
  })
  
  dataset_collection <- reactiveValues(
    names = c("Originals"),
    dataset_config = list(Originals = list())
  )
  
  
  
  # --------------- DATA ---------------------------
  
  observeEvent(
    input$viz_group,
    updateSelectInput(
      session, "viz_type", "Viz Type", 
      choices = {
        viz_univar_choices <- c("Distribution", "Density", "Boxplot")
        viz_bivar_choices <- c("Scatter", "Boxplot", "Correlation")
        
        if (input$viz_group == 'Univariate') {
          viz_univar_choices
        }else {
          viz_bivar_choices
        }
      })
  )
  
  observeEvent(
    input$data_file,
    updateSelectInput(
      session, inputId = "fe_columns",
      choices = colnames(original_dataset()))
  )
  
  
  observe({
    if(input$viz_group == "Univariate"){
      updateSelectizeInput(
        session, inputId = "viz_target",
        choices = colnames(original_dataset()),
        selected = NULL,
        options = list(maxItems = 1))
    }
    
    if(input$viz_group == "Bivariate"){
      updateSelectizeInput(
        session,
        inputId = "viz_target",
        choices = colnames(original_dataset()),
        selected = NULL,
        options = list(maxItems = ncol(original_dataset())))
    }
  })
  
  output$contents <- renderTable({
    if(input$data_disp == "head") {
      return(head(original_dataset()))
    }
    
    return(original_dataset())
  })
  
  # output$summary_table <- renderPlot({
  #   summarytools::dfSummary(dataset())
  # })
  
  observeEvent(
    input$data_file,
    updateSelectInput(
      session, "fe_dataset", "Choose Dataset",
      choices = dataset_collection$names)
  )
  
  observeEvent(
    input$fe_add_dataset,
    shinyalert('hello', type='input', callbackR = callback_add_dataset)
  )
  
  callback_add_dataset <- function(value) {
    trimmed_value <- str_trim(value, side="both")
    
    dataset_collection$names <- append(dataset_collection$names, c(trimmed_value))
    dataset_collection$dataset_config[[convert_name(trimmed_value)]] <- temporary_dataset$transformations
    
    
    updateSelectInput(
      session, "fe_dataset", "Choose Dataset",
      choices = dataset_collection$names)
  }
  
  observeEvent(
    input$fe_dataset,
    {
      temporary_dataset$transformations <- dataset_collection$dataset_config[[convert_name(input$fe_dataset)]]
      # transformations <- dataset_collection$dataset_config[[convert_name(input$fe_dataset)]]
      temp_dataset <- original_dataset()
      
      
      for (transformation in temporary_dataset$transformations) {
        if (transformation$name == 'Rename') {
          temp_dataset <- temp_dataset %>% 
            rename(!!transformation$new_name := !!transformation$old_name)
        }
        
        if (transformation$name == 'Filter') {
          temp_dataset <- temp_dataset %>% 
            filter(!! rlang::parse_expr(transformation$condition))
        }
      }
      
      temporary_dataset$data <- temp_dataset
    }
  )
  
  output$fe_datatable <- renderTable({
    temporary_dataset$data
  })
  
  observeEvent(
    input$fe_transform, {
      new_dataset <- temporary_dataset$data
      
      if (input$fe_options == "Rename") {
        new_dataset <- new_dataset  %>%
          rename(!!input$fe_rename_new_name := !!input$fe_columns)
        
        temporary_dataset$transformations <- append(
          temporary_dataset$transformations,
          list(list(name = input$fe_options, old_name = input$fe_columns, new_name = input$fe_rename_new_name))
        )
        
        
        updateSelectInput(
          session, "fe_columns",
          choices = colnames(new_dataset)
        )
      }
      
      if (input$fe_options == "Filter") {
        new_dataset <- temporary_dataset$data %>% 
          filter(!! rlang::parse_expr(input$fe_filter_condition))
        
        temporary_dataset$transformations <- append(
          temporary_dataset$transformations,
          list(list(name = input$fe_options, condition = input$fe_filter_condition))
        )
        
        # temporary_dataset$data <- new_dataset
      }
      
      if (input$fe_options == "Encoding") {
        new_dataset <- temporary_dataset$data
        
        if (input$fe_encoding_options == "One Hot Encoding") {
          unique_values <- list(unique(new_dataset[[input$fe_columns]]))
          
          for (unique_value in unique_values) {
            new_dataset <- new_dataset %>%
              mutate(
                !!paste(input$fe_columns, unique_value, sep="_") := ifelse(!!input$fe_columns == unique_value)
              )
          }
          
          new_dataset <- new_dataset %>%
            select(-!!input$fe_columns)
        }
        
        if (input$fe_encoding_options == "Label Encoding") {
          new_dataset[[input$fe_columns]] <- as.numeric(factor(new_dataset[[input$fe_columns]]))
        }
        
        temporary_dataset$transformations <- append(
          temporary_dataset$transformations,
          list(list(name = input$fe_options))
        )
        
        # temporary_dataset$data <- new_dataset
        
        updateSelectInput(
          session, "fe_columns",
          choices = colnames(new_dataset)
        )
      }
      
      if (input$fe_options == "Mutate") {
        new_dataset <- temporary_dataset$data
        if (input$fe_mutate_options == "Standardization") {
          new_dataset <- new_dataset %>% 
            mutate(!!input$fe_columns := standardize(new_dataset[[input$fe_columns]]))
          
          # temporary_dataset$data <- new_dataset  
        }
        
        if (input$fe_mutate_options == "Log Transformation") {
          new_dataset <- new_dataset %>% 
            mutate(!!input$fe_columns := normalize(new_dataset[[input$fe_columns]]))
          
          # temporary_dataset$data <- new_dataset  
        }
        
        if (input$fe_mutate_options == "Normalization") {
          
        }
        
        if (input$fe_mutate_options == "Exponential") {
          
        }
      }
      
      temporary_dataset$data <- new_dataset
    }
  )
  
}
