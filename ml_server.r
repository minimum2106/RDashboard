library(shiny)
library(caret)
library(tidyverse)
library(randomForest)
library(e1071)
library(kernlab)

# PREPROCESSING
wbcd_org <- read.csv("data.csv", header=T, stringsAsFactors=F)

wbcd <- wbcd_org %>%
  select(-X) %>%
  select(-id)

wbcd$diagnosis <- factor(wbcd$diagnosis)

training_ind <- createDataPartition(wbcd$diagnosis,
                                    p = 0.75,
                                    list = FALSE,
                                    times = 1)

training <- wbcd[training_ind, ]
test <- wbcd[-training_ind,]


# RANDOM FOREST ELEMENTS
recommended_mtry <- floor(sqrt(ncol(training[, -1*c(1)])))
rfGrid <- expand.grid(mtry = c(recommended_mtry-2, recommended_mtry,
                               recommended_mtry+2))
rfControl <- trainControl(method = "oob",
                          classProbs = TRUE)


rf_model <- train(x = training[, -1], y = training[, 1],
                  method = "rf",
                  tuneGrid = rfGrid,
                  trControl = rfControl,
                  importance = TRUE,
                  trace = FALSE)

# SVM ELEMENT
svm_model <- svm(diagnosis~., data=training)
pre_svm <- predict(svm_model, test[,-1])
class_probabilities <- predict(rf_model, newdata = test[, -1*c(1)], type = "prob")
test$M_prob <- class_probabilities$M



observe({
  updateSelectInput(
    session, inputId = "svm_viz_var_1",
    choices = colnames(training %>% select(-diagnosis))
  )
  
  updateSelectInput(
    session, inputId = "svm_viz_var_2",
    choices = colnames(training %>% select(-diagnosis))
  )
})

observeEvent(
  input$ml_method, {
    if (input$ml_method == "SVM") {
      output$ml_summary <- renderPrint({confusionMatrix(pre_svm, test$diagnosis)})
    }

    if (input$ml_method == "Random Forest") {

      output$ml_summary <- renderPrint({
        flux <- class_probabilities %>%
          select(M)
        colnames(flux)[1] = "Prob"

        predicted_values <- ifelse(flux >= 0.5, "M", "B")

        confusionMatrix(as.factor(predicted_values), test$diagnosis, positive = 'M')
      })

      output$viz_rf_imp <- renderPlot({
        rf_varImp <- varImp(rf_model, type = 2)
        plot(rf_varImp, top = 30)
      })

      output$viz_roc <- renderPlot({
        roc_data <- data.frame(threshold=seq(1,0,-0.01), fpr=0, tpr=0)
        for (i in roc_data$threshold) {
          over_threshold <- test[test$M_prob >= i, ]
          
          fpr <- sum(over_threshold$diagnosis=="B")/sum(test$diagnosis=="B")
          roc_data[roc_data$threshold==i, "fpr"] <- fpr
          
          tpr <- sum(over_threshold$diagnosis=="M")/sum(test$diagnosis=="M")
          roc_data[roc_data$threshold==i, "tpr"] <- tpr
        }

        viz <- ggplot() +
          geom_line(data = roc_data, aes(x = fpr, y = tpr, color = threshold), linewidth = 2) +
          scale_color_gradientn(colors = rainbow(3)) +
          geom_abline(intercept = 0, slope = 1, lty = 2) +
          geom_point(data = roc_data[seq(1, 101, 10), ], aes(x = fpr, y = tpr)) +
          geom_text(data = roc_data[seq(1, 101, 10), ],
                    aes(x = fpr, y = tpr, label = threshold, hjust = 1.2, vjust = -0.2))+
          labs( x = "False Positive Rate",
                y = "True Positive Rate",
                title = "ROC CURVE RF",
                subtitle = "AUC = 0.9914775")
        
        viz
      })
    }
  }
)

observeEvent(
  input$svm_viz_gen, {
    tryCatch({
      form <- paste(input$svm_viz_var_1, "~", input$svm_viz_var_2, sep="")

      output$svm_viz <- renderPlot({plot(svm_model, training, as.formula(form))})
    }, error = function(e) {
      shinyalert("Two variables should be different")
    })
 
  }
)

output$ml_info <- renderText({
  paste("Dataset: \n Breast Cancer Wisconsin")
})


