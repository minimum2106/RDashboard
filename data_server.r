  library(shiny)
  library(shinyalert)
  library(tidyverse)
  library(stringr)
  
  library(DT)
  library(ggplot2)
  
  source("global_var.r", local = TRUE)
  
  
  output$summary <- renderUI({
    summarytools::view(summarytools::dfSummary(temporary_dataset$data, justify = "center"), method = "render")
  })
  
  observeEvent(
    input$data_file, {
      updateSelectInput(
        session, inputId = "fe_columns",
        choices = colnames(original_dataset()))
      
      columns <- prepend(colnames(original_dataset()), "NONE")
      
      if (input$viz_type %in% c("Distribution", "Density")) {
        
        non_numeric_options <-  colnames(original_dataset() %>% select(where(non_numeric)))
        non_numeric_options <-  prepend(non_numeric_options, "NONE")
        
        numeric_options <-  colnames(original_dataset() %>% select(where(is.numeric)))
        numeric_options <-  prepend(numeric_options, "NONE")
        
        updateSelectizeInput(
          session, inputId = "viz_var_1",
          choices = numeric_options,
          selected = NULL,
          options = list(maxItems = 1))
        
        updateSelectizeInput(
          session, inputId = "viz_var_2",
          choices = non_numeric_options,
          selected = NULL,
          options = list(maxItems = 1))
      }else {
        updateSelectizeInput(
          session, inputId = "viz_var_1",
          choices = columns,
          selected = NULL,
          options = list(maxItems = 1))
        
        updateSelectizeInput(
          session, inputId = "viz_var_2",
          choices = columns,
          selected = NULL,
          options = list(maxItems = 1)) 
      }
      
      updateSelectizeInput(
        session, inputId = "viz_color",
        choices = columns,
        selected = NULL,
        options = list(maxItems = 1))
      
      updateSelectizeInput(
        session, inputId = "viz_size",
        choices = columns,
        selected = NULL,
        options = list(maxItems = 1))
      
    }
    
  )
  
 
  
  observe({
    updateSelectInput(
      session, inputId = "fe_columns",
      choices = colnames(temporary_dataset$data))
    
    columns <- prepend(colnames(temporary_dataset$data), "NONE")
    
    if (input$viz_type %in% c("Distribution", "Density")) {
      
      non_numeric_options <-  colnames(temporary_dataset$data %>% select(where(non_numeric)))
      non_numeric_options <-  prepend(non_numeric_options, "NONE")
      
      numeric_options <-  colnames(temporary_dataset$data %>% select(where(is.numeric)))
      numeric_options <-  prepend(numeric_options, "NONE")
      
      updateSelectizeInput(
        session, inputId = "viz_var_1",
        choices = numeric_options,
        selected = NULL,
        options = list(maxItems = 1))
      
      updateSelectizeInput(
        session, inputId = "viz_var_2",
        choices = non_numeric_options,
        selected = NULL,
        options = list(maxItems = 1))
    }else {
      updateSelectizeInput(
        session, inputId = "viz_var_1",
        choices = columns,
        selected = NULL,
        options = list(maxItems = 1))
      
      updateSelectizeInput(
        session, inputId = "viz_var_2",
        choices = columns,
        selected = NULL,
        options = list(maxItems = 1)) 
    }
    
    updateSelectizeInput(
      session, inputId = "viz_color",
      choices = columns,
      selected = NULL,
      options = list(maxItems = 1))
    
    updateSelectizeInput(
      session, inputId = "viz_size",
      choices = columns,
      selected = NULL,
      options = list(maxItems = 1))
  })
  
  
  output$contents <- DT::renderDataTable({
    if(input$data_disp == "head") {
      data <- datatable(head(temporary_dataset$data), options = list(scrollX = T, scrollY= T))
    } else {
      data <- datatable(temporary_dataset$data, options = list(scrollX = T))
    }
    
    data
  })
  
  
  observeEvent(
    input$viz_generate,
    {
      
      viz <- temporary_dataset$data %>% 
        ggplot()
      
      
      if (input$viz_type == "Distribution") {
        tryCatch({
          if (input$viz_var_1 != "NONE" & input$viz_var_2 == "NONE") {
            viz <- temporary_dataset$data %>% 
              ggplot(aes_string(input$viz_var_1)) +
              geom_histogram(binwidth=.5, fill="lightblue")
          }
          
          if (input$viz_var_1 != "NONE" & input$viz_var_2 != "NONE") {
            viz <- temporary_dataset$data %>%
              ggplot(aes_string(x = input$viz_var_1, fill = input$viz_var_2)) +
              geom_histogram(binwidth=.5)
          }
        }, error = function(e) {print(e)})
      }
      
      if (input$viz_type == "Density") {
        tryCatch({
          if (input$viz_var_1 != "NONE" & input$viz_var_2 != "NONE") {
            viz <- temporary_dataset$data %>%
              ggplot(aes_string(x = input$viz_var_1, fill = input$viz_var_2)) +
              geom_density(fill="lightblue")
          }
          
          if (input$viz_var_1 != "NONE" & input$viz_var_2 == "NONE") {
            viz <- temporary_dataset$data %>%
              ggplot(aes_string(input$viz_var_1)) +
              geom_density()
          }
        }, error = function(e) {print(e)})
      }
      
      if (input$viz_type == "Boxplot") {
        tryCatch({
          if (input$viz_var_1 != "NONE" & input$viz_var_2 == "NONE") {
            viz <- temporary_dataset$data %>%
              ggplot(aes_string(input$viz_var_1)) +
              geom_boxplot()
          }
          
          if (input$viz_var_1 != "NONE" & input$viz_var_2 != "NONE") {
            viz <- temporary_dataset$data %>%
              ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
              geom_boxplot()
          }
        }, error = function(e) {print(e)})
      }
      
      
      if (input$viz_type == "Scatter") {
        tryCatch({
      
          if (input$viz_var_1 != "NONE" &
              input$viz_var_2 != "NONE" &
              input$viz_color == "NONE" &
              input$viz_size == "NONE") {
            
            viz <- temporary_dataset$data %>% 
              ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
              geom_point()
          }
          
          if (input$viz_var_1 != "NONE" &
              input$viz_var_2 != "NONE" &
              input$viz_color != "NONE" &
              input$viz_size == "NONE") {
            viz <- temporary_dataset$data %>% 
              ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
              geom_point(aes_string(color = input$viz_color))
          }
          
          if (input$viz_var_1 != "NONE" &
              input$viz_var_2 != "NONE" &
              input$viz_color == "NONE" &
              input$viz_size != "NONE") {
            viz <- temporary_dataset$data %>% 
              ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
              geom_point(aes_string(size = input$viz_size))
          }
          
          if (input$viz_var_1 != "NONE" & 
              input$viz_var_2 != "NONE" &
              input$viz_color != "NONE" &
              input$viz_size != "NONE") {
            viz <- temporary_dataset$data %>% 
              ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
              geom_point(aes_string(size = input$viz_size, color = input$viz_color))
          }
          
        }, error = function(e) {})
      }
      
      if (input$viz_type == "Correlation") {
        tryCatch({
          viz <- temporary_dataset$data %>% 
            ggpairs(aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
            theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
        },error = function(e){print(e)})
      }
      
      output$data_viz <- renderPlot({viz})
    }
  )