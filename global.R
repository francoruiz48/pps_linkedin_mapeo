#global constants
RUTA_EXCEL <- "./content/combined_linkedin_jobs_no_duplicates.xlsx"
RUTA_RDS <- "./content/df_procesado.rds"
RUTA_SECTORES <- "./content/reglas-sectores.csv"
RUTA_TECNOLOGIAS <- "./content/reglas-tecnologias.csv"
RUTA_CATEGORIAS <- "./content/reglas-categorias.csv"
INDICADORES <- FALSE

#Libraries
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
  "shinycssloaders",
  "conflicted"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
options(shiny.minified = TRUE)
options(shiny.autoreload = FALSE)

for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}

#conflicts of libraries
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
conflict_prefer("layout", "graphics")
conflict_prefer("group_rows", "kableExtra")
conflict_prefer("runExample", "shiny")
conflicts_prefer(DT::dataTableOutput)
conflicts_prefer(shiny::dataTableOutput)

