  library(shiny)
  library(shinyalert)
  library(tidyverse)
  library(stringr)
  
  library(DT)
  library(ggplot2)
  
  source("global_var.r", local = TRUE)
  
  output$summary <- renderPrint({
    req(input$data_file)
    
    non_numeric <- function(x) !is.numeric(x)
    
    non_numeric_data <- temporary_dataset$data %>%
      select(where(non_numeric))
    
    numeric_data <- temporary_dataset$data %>%
      select(where(is.numeric))
  })
  
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
  
  output$contents <- DT::renderDataTable({
    if(input$data_disp == "head") {
      return(head(original_dataset()))
    }
    
    return(original_dataset())
  },
  
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '200px', targets = c(1, 3)))
    )
  
  )
  
  
  observeEvent(
    input$viz_generate,
    {
      if (input$viz_type == "Distribution") {
        print(input$viz_target)
        output$data_viz <- renderPlot({
          ggplot(data=temporary_dataset$data, aes_string(input$viz_target)) +
            geom_histogram(binwidth=.5, colour="black", fill="white")
        })
      }
    }
  )