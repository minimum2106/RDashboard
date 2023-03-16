library(shiny)
library(e1071)

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
})

observeEvent(
  input$ml_spliting_options, 
  {
    req(input$data_file)
 
  }
)

observeEvent(
  input$ml_target_options,
  {
    cols <- colnames(temporary_dataset$data)
    cols_list <- as.list(strsplit(paste(cols, collapse = ", "), ","))[[1]]

    updateSelectInput(
      session, inputId = "ml_var_options",
      choices = cols_list[cols_list != input$ml_target_options]
    )
    
  }
)


observeEvent(
  input$ml_run_model,
  {
    req(input$data_file)

    sample <- sample(
      c(TRUE, FALSE), 
      nrow(temporary_dataset$data), 
      replace=TRUE, prob=c(input$ml_train_percentage, 1 - input$ml_train_percentage)
    )
    
    data_to_process <- temporary_dataset$data %>% 
      select(.dots = input$ml_var_options)
    
    data_to_process[, input$ml_target_options] <- as.factor(mtcars[, input$ml_target_options])
    
    train_dataset(data_to_process[sample, ]) 
    test_dataset(data_to_process[!sample, ])
    
    if (input$ml_model_options == "SVM") {
      tryCatch({
        if (input$ml_spliting_options == "Train Test Validation") k = 0 else k = input$ml_cross_validation_k
        
        if (input$ml_svm_kernel == "linear") {
          svm_model <- svm(
            !!input$ml_target_options ~ .,
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
      }, error = function(e) {print(e)})
    }
  }
)

observeEvent(
  input$ml_run_model,
  {
    print("model models")
  }
)
