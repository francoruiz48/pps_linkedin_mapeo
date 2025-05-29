source("./modules/leer_tecnologias.R")
source("./modules/clustering.R")

procesar_df <- function(df) {
    tryCatch(
        {
            showNotification("Inicio de procesamiento de archivo.", type = "message")
            # Leer y limpiar datos
            tryCatch(
                {
                    df <- df %>%
                        mutate(sector = ifelse(is.na(sector), "Desconocido", sector)) %>%
                        mutate(across(c(companyName, contractType, experienceLevel, sector, title), tolower)) %>%
                        mutate(
                            title = str_replace_all(title, "[^a-z0-9 ]", " "),
                            title = str_squish(title)
                        )
                },
                error = function(e) {
                    showNotification("Error al leer y limpiar los datos", type = "error")
                    return("")
                }
            )

            # ===============================
            # 📌 TECNOLOGIAS
            # ===============================
            tryCatch(
                {
                    df <- df %>%
                        rowwise() %>%
                        mutate(
                            tech_tags = detectar_tecnologias(title)
                        ) %>%
                        ungroup()
                },
                error = function(e) {
                    showNotification("Error al detectar tecnologias", type = "error")
                    return("")
                }
            )

            # ===============================
            # 📌 CATEGORÍAS
            # ===============================

            tryCatch(
                {
                    reglas <- read.csv(RUTA_CATEGORIAS, stringsAsFactors = FALSE)

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
                },
                error = function(e) {
                    showNotification("Error al clasificar reglas", type = "error")
                    return("")
                }
            )



            # ===============================
            # 📌 SECTORES
            # ===============================

            tryCatch(
                { # Leer reglas de sector desde CSV
                    reglas_sector <- read.csv(RUTA_SECTORES, stringsAsFactors = FALSE)

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
                },
                error = function(e) {
                    showNotification("Error al clasificar sectores", type = "error")
                    return("")
                }
            )


            # ===============================
            # 📌 PAISES Y CIUDADES
            # ===============================

            tryCatch(
                {
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
                },
                error = function(e) {
                    showNotification("Error al clasificar paises y ciudades", type = "error")
                    return("")
                }
            )

            df <- dbscan_titulos(df)
            showNotification("✅ Archivo reprocesado correctamente.", type = "message")
            return(df)
        },
        error = function(e) {
            showNotification("❌ Error al preprocesar datos: ", e$message, type = "error")
            return("")
        }
    )
}
