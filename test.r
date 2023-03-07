library(shiny)
library(shinyWidgets)
library(WDI)
library(DT)
library(dplyr)

foo <- data.frame(foo_name = c("A", "A", "B", "B", "C", "C"))
data <- cbind(head(mtcars), foo)

ui <- navbarPage(position = "static-top",
                 
                 tabPanel(title = "Base 1",
                          fluidRow(
                            dropdownButton(
                              
                              selectInput(
                                inputId = "choice",
                                label = "Type of data",
                                choices = c("Type X",
                                            "Type Y"),
                                selected = "Type X",
                                multiple = FALSE),
                              selectizeInput(inputId =  "test", 
                                             label = "Test", 
                                             choices = "",
                                             multiple = TRUE),
                              circle = TRUE, status = "primary", 
                              icon = icon("gear"), 
                              width = "300px",
                              tooltip = tooltipOptions(title = "Outils")
                            ),
                            column(width = 12,
                                   dataTableOutput("data"))
                          ))
                 
)


server <- function(input, output, session) {
  
  observe({
    if(input$choice == "Type X"){
      updateSelectizeInput(session,
                           inputId = "test",
                           choices = unique(data$foo_name),
                           selected = NULL)
    }
    else if(input$choice == "Type Y"){
      updateSelectizeInput(session,
                           inputId = "test",
                           choices = unique(data$foo_name),
                           selected = NULL,
                           options = list(maxItems = 1))
    }
  })
  
  output$data <- renderDataTable({
    if(input$choice == "Type X"){
      data2 <- data %>%
        filter(foo_name %in% input$test)
    }
    else if(input$choice == "Type Y"){
      data3 <- data %>%
        filter(foo_name %in% input$test)
    }
  })
  
}


shinyApp(ui = ui, server = server)