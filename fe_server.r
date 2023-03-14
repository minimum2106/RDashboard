  library(shiny)
  library(mltools)
  
  source("global.r", local = TRUE)
  
  # observeEvent(
  #   input$data_file, {
  #     updateSelectInput(
  #       session, "fe_dataset", "Choose Dataset",
  #       choices = dataset_collection$names)
  #     
  #     updateSelectInput(
  #       session, "view_dataset", "Choose Dataset",
  #       choices = dataset_collection$names) 
  #     
  #     updateSelectInput(
  #       session, "summary_dataset", "Choose Dataset",
  #       choices = dataset_collection$names) 
  #     
  #     updateSelectInput(
  #       session, "viz_dataset", "Choose Dataset",
  #       choices = dataset_collection$names) 
  #     
  #     updateSelectInput(
  #       session, "ml_dataset", "Choose Dataset",
  #       choices = dataset_collection$names) 
  #   }
  # )
  
  observeEvent(
    input$fe_add_dataset,
    shinyalert('Enter a new dataset name', type='input', callbackR = callback_add_dataset)
  )
  
  callback_add_dataset <- function(value) {
    trimmed_value <- str_trim(value, side="both")
    
    if (trimmed_value %in% dataset_collection$names) {
      trimmed_value <- paste(trimmed_value, "_", sep="")
    }
    
    dataset_collection$names <- append(dataset_collection$names, c(trimmed_value))
    dataset_collection$dataset_config[[convert_name(trimmed_value)]] <- temporary_dataset$transformations
    
    
    updateSelectInput(
      session, "fe_dataset", "Choose Dataset",
      choices = dataset_collection$names)
    
    # updateSelectInput(
    #   session, "view_dataset", "Choose Dataset",
    #   choices = dataset_collection$names) 
    # 
    # updateSelectInput(
    #   session, "summary_dataset", "Choose Dataset",
    #   choices = dataset_collection$names) 
    # 
    # updateSelectInput(
    #   session, "viz_dataset", "Choose Dataset",
    #   choices = dataset_collection$names) 
    # 
    # updateSelectInput(
    #   session, "ml_dataset", "Choose Dataset",
    #   choices = dataset_collection$names) 
  }
  
  observeEvent(
    input$fe_dataset
    ,
    {
      
      temp_dataset <- original_dataset()
      temporary_dataset$transformations <- dataset_collection$dataset_config[[convert_name(input$fe_dataset)]]
      
      
      for (transformation in temporary_dataset$transformations) {
        if (transformation$name == 'Rename') {
          temp_dataset <- temp_dataset %>% 
            rename(!!transformation$new_name := !!transformation$old_name)
        }
        
        if (transformation$name == 'Filter') {
          temp_dataset <- temp_dataset %>% 
            filter(!! rlang::parse_expr(transformation$condition))
        }
        
        if (transformation$name == 'Handle Missing Values') {
          
          if (transformation$method == 'Mean') {
            temp_dataset <- temp_dataset %>% 
              mutate_at(c(transformation$column), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
          }
          
          if (transformation$method == 'Median') {
            temp_dataset <- temp_dataset %>% 
              mutate_at(c(transformation$column), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
          }
          
          if (transformation$method == 'Most Frequent Value') {
            temp_dataset <- temp_dataset %>% 
              mutate_at(c(transformation$column), ~na_if(., '')) %>% 
              mutate_at(c(transformation$column), ~ifelse(is.na(.x), transformation$value, .x))
          }
          
          if (transformation$method == 'Customize') {
            temp_dataset <- temp_dataset %>% 
              mutate_at(c(transformation$column), ~na_if(., '')) %>% 
              mutate_at(c(transformation$column), ~ifelse(is.na(.x), transformation$value, .x))
          }
          
          if (transformation$method == 'Remove') {
            temp_dataset <- temp_dataset %>% 
              mutate_at(c(transformation$column), ~na_if(., '')) %>% 
              drop_na(!!transformation$column)
          }
            
        }
        
        if (transformation$name == 'Mutate') {
          if (transformation$method == "Log Transformation") {
            temp_dataset <- temp_dataset %>% 
              mutate(!!transformation$column := log_transform(temp_datasett[[transformation$column]]))
          }
          
          if (transformation$method == "Standardization") {
            temp_dataset <- temp_dataset %>% 
              mutate(!!transformation$column := standardize(temp_datasett[[transformation$column]]))
          }
          
          if (transformation$method == "Exponential") {
            temp_dataset <- temp_dataset %>% 
              mutate(!!transformation$column := exponential(temp_datasett[[transformation$column]]))
          }
          
          if (transformation$method == "Normalization") {
            temp_dataset <- temp_dataset %>% 
              mutate(!!transformation$column := normalize(temp_datasett[[transformation$column]]))
          }
        }
        
        if (transformation$name == 'Encoding') {
          if (transformation$method == "One Hot Encoding") {
            uniq_values <- unique(temp_dataset[[transformation$column]])
            
            for (uniq_value in uniq_values) {
              temp_dataset <- temp_dataset %>% 
                mutate(
                  !!paste(transformation$column, uniq_value, sep="_") := match_or_not(temp_dataset[[transformation$column]], uniq_value))
            }
            
            temp_dataset <- temp_dataset %>% 
              select(-as.symbol(transformation$column))
          }
          
          if (transformation$method == "Label Encoding") {
            temp_dataset[[input$fe_columns]] <- as.numeric(factor(temp_dataset[[transformation$column]]))
          }
        }
      }
      
      temporary_dataset$data <- temp_dataset
    }
  )
  
  output$fe_datatable <- renderTable({
    temporary_dataset$data
  })
  
  observeEvent({
    input$fe_options
    input$fe_columns
    },
    {
      req(input$data_file)
      
      if (input$fe_options == "Handle Missing Values") {
        updateSelectInput(
          session, "fe_mutate_mv", "Methods",
          choices ={
            categorical_options = c("Most Frequent Value", "Customize", "Remove")
            numerical_options = c("Mean", "Median", "Remove")
            
            if (is.numeric(temporary_dataset$data[[input$fe_columns]])) {
              numerical_options
            }else {
              categorical_options
            }
          }
        )
      }
    }
  )
  
  
  observeEvent(
    input$fe_transform, {
      new_dataset <- temporary_dataset$data
      
      if (input$fe_options == "Rename") {
        tryCatch({
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
        }, error = function(e) {}
        )
      }
      
      if (input$fe_options == "Filter") {
        tryCatch({
          new_dataset <- new_dataset %>% 
            filter(!! rlang::parse_expr(input$fe_filter_condition))
          
          temporary_dataset$transformations <- append(
            temporary_dataset$transformations,
            list(list(name = input$fe_options, condition = input$fe_filter_condition))
          )
        }, error = function(e) {}
        
        ) 
        # temporary_dataset$data <- new_dataset
      }
      
      if (input$fe_options == "Encoding") {
        tryCatch({
          if (input$fe_encoding_options == "One Hot Encoding") {
            uniq_values <- unique(new_dataset[[input$fe_columns]])
            
            for (uniq_value in uniq_values) {
              new_dataset <- new_dataset %>% 
                mutate(
                  !!paste(input$fe_columns, uniq_value, sep="_") := match_or_not(new_dataset[[input$fe_columns]], uniq_value))
            }
            
            new_dataset <- new_dataset %>% 
              select(-as.symbol(input$fe_columns))
              
          }
          
          if (input$fe_encoding_options == "Label Encoding") {
            new_dataset[[input$fe_columns]] <- as.numeric(factor(new_dataset[[input$fe_columns]]))
          }
          
          temporary_dataset$transformations <- append(
            temporary_dataset$transformations,
            list(list(name = input$fe_options))
          )
          
          temporary_dataset$transformations <- append(
            temporary_dataset$transformations,
            list(list(
              name = input$fe_options, 
              method = input$fe_encoding_options,
              column = input$fe_columns
            )
            )
          )
          
          updateSelectInput(
            session, "fe_columns",
            choices = colnames(new_dataset)
          )
          
        }, error = function(e) {print(e)}
        )        
      }
      
      if (input$fe_options == "Mutate") {
        tryCatch({
          if (input$fe_mutate_options == "Standardization") {
            new_dataset <- new_dataset %>% 
              mutate(!!input$fe_columns := standardize(new_dataset[[input$fe_columns]]))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_options,
                column = input$fe_columns
              )
              )
            )
          }
          
          if (input$fe_mutate_options == "Log Transformation") {
            new_dataset <- new_dataset %>% 
              mutate(!!input$fe_columns := log_transform(new_dataset[[input$fe_columns]]))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_options,
                column = input$fe_columns
              )
              )
            )
          }
          
          if (input$fe_mutate_options == "Normalization") {
            new_dataset <- new_dataset %>% 
              mutate(!!input$fe_columns := normalize(
                new_dataset[[input$fe_columns]], 
                min(new_dataset[[input$fe_columns]]), 
                max(new_dataset[[input$fe_columns]])
              )
            )
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_options,
                column = input$fe_columns
              )
              )
            )
            
            print("done normalizing")
          }
          
          if (input$fe_mutate_options == "Exponential") {
            new_dataset <- new_dataset %>% 
              mutate(!!input$fe_columns := exponential(new_dataset[[input$fe_columns]]))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_options,
                column = input$fe_columns
              )
              )
            )
            
            print("done exponential")
          }
        }, error = function(e) {
          print(e)
        })
        
      
      }
      
      if (input$fe_options == "Handle Missing Values") {
        tryCatch({
          if (input$fe_mutate_mv == 'Mean') {
            new_dataset <- new_dataset %>% 
              mutate_at(c(input$fe_columns), ~ifelse(is.na(.x), mean(.x, na.rm = TRUE), .x))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_mv,
                column = input$fe_columns
                )
              )
            )
          }
          
          if (input$fe_mutate_mv == "Median") {
            new_dataset <- new_dataset %>% 
              mutate_at(c(input$fe_columns), ~ifelse(is.na(.x), median(.x, na.rm = TRUE), .x))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_mv,
                column = input$fe_columns, 
                )
              )
            )
            
          }
          
          if (input$fe_mutate_mv == "Remove") {
            new_dataset <- new_dataset %>% 
              mutate_at(c(input$fe_columns), ~na_if(., '')) %>% 
              drop_na(!!input$fe_columns)
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_mv, 
                column = input$fe_columns))
            )
          }
          
          if (input$fe_mutate_mv == "Most Frequent Value") {
            mod_dataset <- new_dataset %>% 
              mutate_at(c(input$fe_columns), ~na_if(., ''))
            
            most_freq_value <- names(which.max(table(mod_dataset[[input$fe_columns]])))
            
            new_dataset <- mod_dataset %>% 
              mutate_at(c(input$fe_columns), ~ifelse(is.na(.x), most_freq_value, .x))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_mv,
                column = input$fe_columns, 
                value = most_freq_value
                )
              )
            )
          }
          
          if (input$fe_mutate_mv == "Customize") {
            new_dataset <- new_dataset %>% 
              mutate_at(c(input$fe_columns), ~na_if(., '')) %>% 
              mutate_at(c(input$fe_columns), ~ifelse(is.na(.x), input$fe_mutate_mv_customize, .x))
            
            temporary_dataset$transformations <- append(
              temporary_dataset$transformations,
              list(list(
                name = input$fe_options, 
                method = input$fe_mutate_mv, 
                column = input$fe_columns, 
                value = input$fe_mutate_mv_customize))
            )
            
          }
        }, error = function(e) {print(e)}
        )
      }
    
      temporary_dataset$data <- new_dataset
    }
  )
