  library(shiny)
  
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
  
