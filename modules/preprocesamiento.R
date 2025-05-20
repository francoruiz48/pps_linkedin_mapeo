source("./modules/leer_tecnologias.R")
source("./modules/clustering.R")

procesar_df <- function(df) {
    debug_msg("Iniciando el preprocesamiento", "preprocesamiento.R")
    # Leer y limpiar datos
    df <- df %>%
        mutate(sector = ifelse(is.na(sector), "Desconocido", sector)) %>%
        mutate(across(c(companyName, contractType, experienceLevel, sector, title), tolower)) %>%
        mutate(
            title = str_replace_all(title, "[^a-z0-9 ]", " "),
            title = str_squish(title)
        )

    # ===============================
    # 📌 TECNOLOGIAS
    # ===============================
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


    # ===============================
    # 📌 PAISES Y CIUDADES
    # ===============================

    df <- df %>%
        mutate(
            location_parts = str_split(location, ",\\s*"),
            pais = sapply(location_parts, function(x) tail(x, 1)),
            ciudad = sapply(location_parts, function(x) head(x, 1))
        )

    mapping_ciudad <- agrupar_fuzzy(df$ciudad)

    if ("ciudad_cluster" %in% colnames(df)) {
        df <- df %>% select(-ciudad_cluster)
    }
    df <- df %>%
        left_join(mapping_ciudad, by = c("ciudad" = "original")) %>%
        rename(ciudad_cluster = grupo)


    mapping_pais <- agrupar_fuzzy(df$pais)

    if ("pais_cluster" %in% colnames(df)) {
        df <- df %>% select(-pais_cluster)
    }
    df <- df %>%
        left_join(mapping_pais, by = c("pais" = "original")) %>%
        rename(pais_cluster = grupo)

    df <- dbscan_titulos(df)

    debug_msg("🟢Finalizo el preprocesamiento del dataframe con éxito!", "preprocesamiento.R")
    return(df)
}
