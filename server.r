library(shinyalert)
library(tidyverse)
library(stringr)

server <- function(session, input, output) {
  source("data_server.r", local = TRUE)
  source("fe_server.r", local = TRUE)
  source("ml_server.r", local = TRUE)
}
