source("./modules/clustering.R")


clasificar_desde_reglas <- function(titulo, reglas, entity, multiple = FALSE) {
    resultados <- c()
    for (i in 1:nrow(reglas)) {
        valor <- reglas[[entity]][i]
        keywords <- reglas$keywords[i]

        if (is.na(keywords) || keywords == "") next

        # Separar palabras clave y crear patrón
        palabras <- unlist(strsplit(keywords, ",\\s*"))
        pattern <- paste0("\\b(", paste(palabras, collapse = "|"), ")\\b")

        # Si hay coincidencia, devolver la entidad
        if (str_detect(titulo, regex(pattern, ignore_case = TRUE))) {
            if (multiple) {
                resultados <- c(resultados, valor)
            } else {
                return(valor)
            }
        }
    }
    if (multiple) {
        return(paste(unique(resultados), collapse = ", "))
    } else {
        return("Otro")
    }
}

procesar_df <- function(df) {
    tryCatch(
        {
            shinyjs::show("loader_div")
            showNotification("Inicio de procesamiento de archivo.", type = "message")

            requeridas <- c("title", "companyName", "sector", "location")
            if (!all(requeridas %in% names(df))) {
                showNotification("❌ El archivo no contiene las columnas necesarias.", type = "error")
                return("")
            }


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

            detectar_tecnologias <- function(titulo) {
                tryCatch(
                    {
                        if (length(titulo) != 1 || is.na(titulo) || titulo == "") {
                            return("")
                        }

                        reglas <- read.csv(RUTA_TECNOLOGIAS, stringsAsFactors = FALSE)
                        return(clasificar_desde_reglas(titulo, reglas, entity = "tecnologia", multiple = TRUE))
                    },
                    error = function(e) {
                        message("❌ Error en detectar_tecnologias(): ", e$message)
                        return("")
                    }
                )
            }


            df <- df %>%
                rowwise() %>%
                mutate(
                    tech_tags = detectar_tecnologias(title)
                ) %>%
                ungroup()

            # ===============================
            # 📌 CATEGORÍAS
            # ===============================

            tryCatch(
                {
                    reglas <- read.csv(RUTA_CATEGORIAS, stringsAsFactors = FALSE)

                    df <- df %>%
                        mutate(categoria = sapply(title, \(x) clasificar_desde_reglas(x, reglas, entity = "categoria")))
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

                    df <- df %>%
                        mutate(sector_general = sapply(sector, \(x) clasificar_desde_reglas(x, reglas_sector, entity = "sector_general")))
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
        }, finally = {
            shinyjs::hide("loader_div")
        }
    )
}
