library(shinyalert)
library(tidyverse)
library(stringr)

source("global.r", local = TRUE)


server <- function(session, input, output) {
  source("global_var.r", local = TRUE)
  source("data_server.r", local = TRUE)
  source("fe_server.r", local = TRUE)
}
