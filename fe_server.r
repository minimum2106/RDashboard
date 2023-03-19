  library(shiny)
  library(mltools)

  source("global.r", local = TRUE)

  ERROR <- reactiveVal("")
  observe({
    if (ERROR() == "") {
      return()
    }

    shinyalert(ERROR(), type="error")
  })

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

    updateSelectInput(
      session, "view_dataset", "Choose Dataset",
      choices = dataset_collection$names)

    updateSelectInput(
      session, "summary_dataset", "Choose Dataset",
      choices = dataset_collection$names)

    updateSelectInput(
      session, "viz_dataset", "Choose Dataset",
      choices = dataset_collection$names)

    updateSelectInput(
      session, "ml_dataset", "Choose Dataset",
      choices = dataset_collection$names)
  }

  observeEvent( {
    input$fe_dataset
    input$view_dataset
    input$viz_dataset
    input$summary_dataset
  }

    ,
    {
      new_dataset = "STOP"

      if (current_dataset() != input$fe_dataset) {
        new_dataset <- input$fe_dataset
      }

      if (current_dataset() != input$viz_dataset) {
        new_dataset <- input$viz_dataset
      }

      if (current_dataset() != input$view_dataset) {
        new_dataset <- input$view_dataset
      }

      if (current_dataset() != input$summary_dataset) {
        new_dataset <- input$summary_dataset
      }
      

      if (new_dataset == "STOP") {
        return()
      }

      updateSelectInput(
        session, "fe_dataset", "Choose Dataset",
        selected = new_dataset)

      updateSelectInput(
        session, "view_dataset", "Choose Dataset",
        selected = new_dataset)

      updateSelectInput(
        session, "summary_dataset", "Choose Dataset",
        selected = new_dataset)

      updateSelectInput(
        session, "viz_dataset", "Choose Dataset",
        selected = new_dataset)

      updateSelectInput(
        session, "ml_dataset", "Choose Dataset",
        selected = new_dataset)

      current_dataset(new_dataset)

      temp_dataset <- original_dataset()
      temporary_dataset$transformations <- dataset_collection$dataset_config[[convert_name(current_dataset())]]


      for (transformation in temporary_dataset$transformations) {
        if (transformation$name == 'Change Datatype') {
          if (transformation$method == "Integer") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := as_integer(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Numeric") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := as_numeric(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Character") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := as_character(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Factor") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := as_factor_self(temp_dataset[[transformation$column]]))
          }
        }

        if (transformation$name == "Remove Column") {
          temp_dataset <- temp_dataset %>%
            select(-as.symbol(transformation$old_name))
        }

        if (transformation$name == 'Rename') {
          temp_dataset <- temp_dataset %>%
            rename(!!transformation$new_name := !!transformation$old_name)
        }

        if (transformation$name == 'Filter') {
          temp_dataset <- temp_dataset %>%
            filter(!!rlang::parse_expr(transformation$condition))
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
              mutate(!!transformation$column := log_transform(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Standardization") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := standardize(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Exponential") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := exponential(temp_dataset[[transformation$column]]))
          }

          if (transformation$method == "Normalization") {
            temp_dataset <- temp_dataset %>%
              mutate(!!transformation$column := normalize(temp_dataset[[transformation$column]]))
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

  observeEvent(
    input$fe_dataset, {
      output$fe_transformation_log <- renderText({
        trans_format = ""

        line_break = "\n"

        trans_break = "============================================================="
        trans_name_placeholder = "Transformation's name:"
        trans_value_placeholder = "Value:"

        for (transformation in temporary_dataset$transformations){
          if (transformation$name == "Change Datatype") {
            trans_content <- paste(
              paste(trans_name_placeholder, transformation$name, sep=" "),
              paste("Column:", transformation$column, sep=" "),
              paste("Method:", transformation$method, sep=" "),
              sep = line_break
            )
          }

          if (transformation$name == "Remove Column") {
            trans_content <- paste(
              paste(trans_name_placeholder, transformation$name, sep=" "),
              paste("Column::", transformation$column, sep=" "),
              sep = line_break
            )
          }

          if (transformation$name == "Rename") {
            trans_content <- paste(
              paste(trans_name_placeholder, transformation$name, sep=" "),
              paste("Old Name:", transformation$old_name, sep=" "),
              paste("New Name:", transformation$new_name, sep=" "),
              sep = line_break
            )
          }

          if (transformation$name == "Filter") {
            trans_content <- paste(
              paste(trans_name_placeholder, transformation$name, sep=" "),
              paste("Condition:", transformation$condition, sep=" "),
              sep = line_break
            )
          }

          if (transformation$name == "Encoding") {
            trans_content <- paste(
              paste(trans_name_placeholder, transformation$name, sep=" "),
              paste("Column:", transformation$column, sep=" "),
              paste("Method:", transformation$method, sep=" "),
              sep = line_break
            )
          }

          if (transformation$name == "Mutate") {
            if (transformation$method == "Normalization") {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                sep = line_break
              )
            }

            if (transformation$method == "Log Transformation") {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                sep = line_break
              )
            }

            if (transformation$method == "Exponential") {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                sep = line_break
              )
            }

            if (transformation$method == "Standardization") {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                sep = line_break
              )
            }


          }

          if (transformation$name == "Handle Missing Values") {
            if (transformation$name %in% c("Customize", "Most Frequent Value")) {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                paste("Value:", transformation$value, sep=" "),
                sep = line_break
              )
            } else {
              trans_content <- paste(
                paste(trans_name_placeholder, transformation$name, sep=" "),
                paste("Column:", transformation$column, sep=" "),
                paste("Method:", transformation$method, sep=" "),
                sep = line_break
              )
            }
          }


          trans_format <- paste(trans_format, trans_break, trans_content, sep="\n")
        }

        trans_format
      }
  )})

output$fe_datatable <- DT::renderDataTable({
  datatable(temporary_dataset$data, options=list(scrollX = T, scrollY = T, lengthMenu = c(15, 25, 35)))
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

    if (input$fe_options == "Remove Column") {
      tryCatch({
        new_dataset <- new_dataset  %>%
          select(-as.symbol(!!input$fe_columns))

        temporary_dataset$transformations <- append(
          temporary_dataset$transformations,
          list(list(name = input$fe_options, column = input$fe_columns))
        )

        updateSelectInput(
          session, "fe_columns",
          choices = colnames(new_dataset)
        )
      }, error = function(e) {ERROR("Invalid Transformation")}
      )
    }

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
      }, error = function(e) {ERROR("Invalid Name")}
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

      }, error = function(e) {ERROR("Invalid condition")}
      )
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

        }, error = function(e) {ERROR("Invalid Encoding Process")}
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
          }
        }, error = function(e) {ERROR("Unsuccessful Mutation")})


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
        }, error = function(e) {ERROR("Invalid Condition")}
        )
      }



      if (input$fe_options == "Change Datatype") {
        tryCatch({
          if (input$fe_cd_methods == "Factor") {
            new_dataset <- new_dataset %>%
              mutate(!!input$fe_columns := as_factor(new_dataset[[input$fe_columns]]))
          }

          if (input$fe_cd_methods == "Integer") {
            new_dataset <- new_dataset %>%
              mutate(!!input$fe_columns := as_integer(new_dataset[[input$fe_columns]]))
          }

          if (input$fe_cd_methods == "Numeric") {
            new_dataset <- new_dataset %>%
              mutate(!!input$fe_columns := as_numeric(new_dataset[[input$fe_columns]]))
          }

          if (input$fe_cd_methods == "Character") {
            new_dataset <- new_dataset %>%
              mutate(!!input$fe_columns := as_character(new_dataset[[input$fe_columns]]))
          }

          temporary_dataset$transformations <- append(
            temporary_dataset$transformations,
            list(list(
              name = input$fe_options,
              method = input$fe_cd_methods,
              column = input$fe_columns
            )
            )
          )
        },  error = function(e) {ERROR("Invalid Transformation")})
      }

      temporary_dataset$data <- new_dataset
    }
  )
