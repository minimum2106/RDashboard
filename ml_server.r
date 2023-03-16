library(shiny)
library(e1071)
library(cowplot)

train_dataset <- reactiveVal()
test_dataset <- reactiveVal()

model <- reactiveVal()

observe({
  cols <- colnames(temporary_dataset$data)
  cols_list <- as.list(strsplit(paste(cols, collapse = ", "), ","))[[1]]
  
  updateSelectInput(
    session, inputId = "ml_target_options",
    choices = cols_list
    )
  
  updateSelectInput(
    session, inputId = "ml_svm_viz_option_1",
    choices = cols_list
  )
  
  updateSelectInput(
    session, inputId = "ml_svm_viz_option_2",
    choices = cols_list
  )
})


observeEvent(
  input$ml_run_model,
  {
    req(input$data_file)

    sample <- sample(
      c(TRUE, FALSE), 
      nrow(temporary_dataset$data), 
      replace=TRUE, prob=c(input$ml_train_percentage, 1 - input$ml_train_percentage)
    )
    

    data_to_process <- temporary_dataset$data
    data_to_process[, input$ml_target_options] <- as.factor(data_to_process[, input$ml_target_options])  
    
    train_dataset(data_to_process[sample, ]) 
    test_dataset(data_to_process[!sample, ])
  
    
    if (input$ml_model_options == "SVM") {
      tryCatch({
        if (input$ml_spliting_options == "Train Test Validation") k = 0 else k = input$ml_cross_validation_k
        
        if (input$ml_svm_kernel == "linear") {
          form <- paste(input$ml_target_options, "~.", sep="")

          svm_model <- svm(
            as.formula(form),
            data = train_dataset(),
            kernel = input$ml_svm_kernel,
            cost = input$ml_svm_cost,
            scale = TRUE,
            cross = k
          )
        }
        
        if (input$ml_svm_kernel == "polynomial") {
          svm_model <- svm(
            !!input$ml_target_options ~ .,
            data = train_dataset(),
            kernel = input$ml_svm_kernel,
            cost = input$ml_svm_cost,
            degree = input$ml_svm_degree,
            gamma = input$ml_svm_gamma,
            coef0 = input$ml_svm_coef0,
            scale = TRUE,
            cross = k
          )
        }
        
        if (input$ml_svm_kernel == "radial basis") {
          svm_model <- svm(
            !!input$ml_target_options ~ .,
            data = train_dataset(),
            kernel = input$ml_svm_kernel,
            cost = input$ml_svm_cost,
            gamma = input$ml_svm_gamma,
            scale = TRUE,
            cross = k
          )
        }
        
        if (input$ml_svm_kernel == "sigmoid") {
          svm_model <- svm(
            !!input$ml_target_options ~ .,
            data = train_dataset(),
            kernel = input$ml_svm_kernel,
            cost = input$ml_svm_cost,
            gamma = input$ml_svm_gamma,
            coef0 = input$ml_svm_coef0,
            scale = TRUE,
            cross = k
          )
        }
        
        model(svm_model)
    
        output$ml_summary <- renderPrint({summary(svm_model)})
      }, error = function(e) {print(e)})
    }
    
    if (input$ml_model_options == "Naive Bayes") {
      
    }
    
    if (input$ml_model_options == "Logistic Regression") {
      tryCatch({
        form <- paste(input$ml_target_options, "~.", sep="")
        
        logistic_model <- glm(
          as.formula(form), 
          data = train_dataset(), 
          family = 'binomial'
        )
        
        model(logistic_model)
        
        predicted.data <- data.frame(
          predicted_prob = logistic_model$fitted.values,
          label = train_dataset()[[input$ml_target_options]]
        )
        
        predicted.data <- predicted.data[
          order(predicted_prob, decreasing = FALSE)
        ]
        
        predicted.data$rank <- 1:nrow(predicted.data)
        
        viz <- predicted.data %>% 
          ggplot(aes(x=rank, y=predicted_prob)) + 
          geom_point(aes_string(color = input$ml_target_options), alpha = 1, shape = 4, stroke = 2) + 
          xlab("Index") + 
          ylab("Predicted Probability")
        
        output$ml_viz <- renderPlot({viz})
        
        output$ml_summary <- renderPrint({summary(logistic_model)})
      }, error = function(e) {print(e)})
    }
  }
)

observeEvent(
  input$ml_svm_viz_generate, {
    output$ml_viz <- renderPlot({
      viz_form <- paste(input$ml_svm_viz_option_1, '~' , input$ml_svm_viz_option_1, sep="")
      
      plot(model(), train_dataset(), Sex~Major3)
    })
  }
)

