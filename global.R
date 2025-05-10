source("./modules/leer_tecnologias.R")

options(shiny.minified = TRUE)

required_libraries <- c("shiny", "shinydashboard","ggplot2","tidyr","tidyverse", "DT", "readxl", "dplyr", "stringr","writexl", "kableExtra", "pagedown", "htmltools", "plotly")


for (lib in required_libraries) {
  if (!requireNamespace(lib, quietly = TRUE)) {
    install.packages(lib)
  }
  library(lib, character.only = TRUE)
}

# Leer y limpiar datos
df <- readxl::read_excel("./content/combined_linkedin_jobs_no_duplicates.xlsx") %>%
  mutate(sector = ifelse(is.na(sector), "Desconocido", sector)) %>%
  mutate(across(c(companyName, contractType, experienceLevel, sector, title), tolower)) %>%
  mutate(
    title = str_replace_all(title, "[^a-z0-9 ]", " "),
    title = str_squish(title)
  )



df <- df %>%
  rowwise() %>%
  mutate(
    tech_tags = detectar_tecnologias(title)
  ) %>%
  ungroup()


# ===============================
# 📌 CATEGORÍAS
# ===============================

reglas <- read.csv("./content/reglas-categorias.csv", stringsAsFactors = FALSE)

clasificar_desde_reglas <- function(titulo, reglas) {
  for (i in 1:nrow(reglas)) {
    categoria <- reglas$categoria[i]
    keywords <- reglas$keywords[i]

    if (is.na(keywords) || keywords == "") next

    # Separar palabras clave y crear patrón
    palabras <- unlist(strsplit(keywords, ",\\s*"))
    pattern <- paste0("\\b(", paste(palabras, collapse = "|"), ")\\b")

    # Si hay coincidencia, devolver la categoría
    if (str_detect(titulo, regex(pattern, ignore_case = TRUE))) {
      return(categoria)
    }
  }
  return("Otro")
}

df <- df %>%
  mutate(categoria = sapply(title, \(x) clasificar_desde_reglas(tolower(x), reglas)))

# ===============================
# 📌 SECTORES
# ===============================

# Leer reglas de sector desde CSV
reglas_sector <- read.csv("./content/reglas-sectores.csv", stringsAsFactors = FALSE)

agrupar_sector_csv <- function(texto, reglas) {
  for (i in 1:nrow(reglas)) {
    categoria <- reglas$sector_general[i]
    keywords <- reglas$keywords[i]

    if (is.na(keywords) || keywords == "") next

    palabras <- unlist(strsplit(keywords, ",\\s*"))
    pattern <- paste0("\\b(", paste(palabras, collapse = "|"), ")\\b")

    if (str_detect(texto, regex(pattern, ignore_case = TRUE))) {
      return(categoria)
    }
  }
  return("Otros")
}

df <- df %>%
  mutate(sector = ifelse(is.na(sector), "Desconocido", sector)) %>%
  mutate(sector = tolower(sector)) %>%
  mutate(sector_general = sapply(sector, \(x) agrupar_sector_csv(x, reglas_sector)))

