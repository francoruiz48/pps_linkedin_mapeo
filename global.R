source("./modules/preprocesamiento.R")

options(shiny.minified = TRUE)

required_libraries <- c(
  "shiny", 
  "shinydashboard",
  "ggplot2",
  "tidyr",
  "tidyverse", 
  "DT", 
  "dbscan",
  "readxl", 
  "dplyr", 
  "cluster",
  "stringr",
  "writexl", 
  "kableExtra", 
  "pagedown", 
  "htmltools", 
  "plotly", 
  "stringdist", 
  "stats", 
  "text2vec",
  "shinyjs",
  "shinycssloaders"
  )

options(repos = c(CRAN = "https://cloud.r-project.org"))

for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}
