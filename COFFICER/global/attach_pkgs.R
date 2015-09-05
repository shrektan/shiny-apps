library(COFFICER)
library(shiny)
library(shinythemes)
# library(shinydashboard)
library(DT)
library(shinyBS)
library(shinyjs)
library(rmarkdown)
Sys.setenv("RSTUDIO_PANDOC" = "D:/app/RStudio/bin/pandoc")
options(DT.options = list(
  dom = "lftip",
  ordering = TRUE,
  pageLength = 10,
  autoWidth = FALSE # if TRUE, the Responsive would be kind of action weird.
))
