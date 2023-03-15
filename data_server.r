  library(shiny)
  library(shinyalert)
  library(tidyverse)
  library(stringr)
  
  library(DT)
  library(ggplot2)
  
  source("global_var.r", local = TRUE)
  
  # output$summary <- renderPrint({
  #   req(input$data_file)
  #   
  #   non_numeric_data <- temporary_dataset$data %>%
  #     select(where(non_numeric))
  #   
  #   numeric_data <- temporary_dataset$data %>%
  #     select(where(is.numeric))
  #   
  #   numeric_data %>% 
  #     summarise_all(list(mean, median , max, min))
  # })
  
  output$summary <- renderUI({
    summarytools::view(summarytools::dfSummary(temporary_dataset$data, justify = "center"), method = "render")
  })
  
  observeEvent(
    input$data_file, {
      updateSelectInput(
        session, inputId = "fe_columns",
        choices = colnames(original_dataset()))
      
      columns <- prepend(colnames(temporary_dataset$data), "")
      
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
  
  output$contents <- DT::renderDataTable({
    if(input$data_disp == "head") {
      data <- datatable(head(original_dataset()), options = list(scrollX = T, scrollY= T))
    } else {
      data <- datatable(original_dataset(), options = list(scrollX = T))
    }
    
    data
  })
  
  
  observeEvent(
    input$viz_generate,
    {
      viz <- temporary_dataset$data %>% 
        ggplot()
      
      if (input$viz_type == "Distribution") {
        
        if (input$viz_var_1 != "" & input$viz_var_2 == "") {
          viz <- temporary_dataset$data %>% 
            ggplot(aes_string(input$viz_var_1)) +
            geom_histogram(binwidth=.5)
        }
        
        if (input$viz_var_1 != "" & input$viz_var_2 != "") {
          viz <- temporary_dataset$data %>%
            ggplot(aes_string(x = input$viz_var_1, fill = input$viz_var_2)) +
            geom_histogram(binwidth=.5)
        }
      }
      
      if (input$viz_type == "Density") {
        
        if (input$viz_var_1 != "" & input$viz_var_2 != "") {
          viz <- temporary_dataset$data %>%
            ggplot(aes_string(x = input$viz_var_1, fill = input$viz_var_2)) +
            geom_density(alpha=.3)
        }
        
        if (input$viz_var_1 != "" & input$viz_var_2 == "") {
          viz <- temporary_dataset$data %>%
            ggplot(aes_string(input$viz_var_1)) +
            geom_density(alpha=.3)
        }
      }
      
      if (input$viz_type == "Boxplot") {
        if (input$viz_var_1 != "" & input$viz_var_2 == "") {
          viz <- temporary_dataset$data %>%
            ggplot(aes_string(input$viz_var_1)) +
            geom_boxplot()
        }
        
        if (input$viz_var_1 != "" & input$viz_var_2 != "") {
          viz <- temporary_dataset$data %>%
            ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
            geom_boxplot()
        }
      }
      
      
      if (input$viz_type == "Scatter") {
        if (input$viz_var_1 != "" &
            input$viz_var_2 != "" &
            input$viz_color == "" &
            input$viz_size == "") {
          viz <- temporary_dataset$data %>% 
            ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
            geom_point()
        }
        
        if (input$viz_var_1 != "" &
            input$viz_var_2 != "" &
            input$viz_color != "" &
            input$viz_size == "") {
          viz <- temporary_dataset$data %>% 
            ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
            geom_point(aes_string(color = input$viz_color))
        }
        
        if (input$viz_var_1 != "" &
            input$viz_var_2 != "" &
            input$viz_color == "" &
            input$viz_size != "") {
          viz <- temporary_dataset$data %>% 
            ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
            geom_point(aes_string(size = input$viz_size))
        }
        
        if (input$viz_var_1 != "" & 
            input$viz_var_2 != "" &
            input$viz_color != "" &
            input$viz_size != "") {
          viz <- temporary_dataset$data %>% 
            ggplot(aes_string(input$viz_var_1, input$viz_var_2)) +
            geom_point(aes_string(size = input$viz_size, color = input$viz_color))
        }

      }
      
     
      output$data_viz <- renderPlot({viz})
    }
  )