# Función para clusterizar nombres similares
agrupar_fuzzy <- function(lista, max_dist = 0.2, label = "valor") {
  lista <- unique(lista)
  dist_matrix <- stringdist::stringdistmatrix(lista, lista, method = "jw")
  hc <- hclust(as.dist(dist_matrix), method = "average")
  grupos <- cutree(hc, h = max_dist)

  # Nombrar cada grupo con el valor más frecuente
  nombres_grupo <- tapply(lista, grupos, function(x) names(sort(table(x), decreasing = TRUE))[1])
  names(nombres_grupo) <- NULL
  mapping <- data.frame(original = lista, grupo = nombres_grupo[grupos], stringsAsFactors = FALSE)

  # Mostrar agrupaciones donde se agrupan al menos 2 elementos
  agrupaciones <- mapping %>%
    group_by(grupo) %>%
    filter(n() > 1) %>%
    summarise(grupo = first(grupo), miembros = paste(original, collapse = ", ")) %>%
    ungroup()


  return(mapping)
}

dbscan_titulos <- function(df, eps = 1.2, minPts = 5) {  
  cat("Paso 1: Preprocesando títulos\n")
  titles <- tolower(df$title)
  it <- itoken(titles, progressbar = FALSE)

  cat("Paso 2: Generando vocabulario\n")
  vocab <- create_vocabulary(it)

  cat("Paso 3: Creando vectorizador y DTM\n")
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)

  cat("Paso 4: Transformando con TF-IDF\n")
  tfidf_transformer <- TfIdf$new()
  tfidf_matrix <- tfidf_transformer$fit_transform(dtm)

  cat("Dimensiones de la matriz TF-IDF: ", paste(dim(tfidf_matrix), collapse = " x "), "\n")
  if (any(is.na(tfidf_matrix))) {
    stop("Error: Hay NAs en la matriz TF-IDF")
  }

  if (nrow(tfidf_matrix) == 0 || ncol(tfidf_matrix) == 0) {
    stop("Error: La matriz TF-IDF está vacía")
  }

  # DBSCAN clustering
  cat("Paso 5: Ejecutando DBSCAN\n")
  set.seed(123)

  # Manejo de errores
  result <- tryCatch({
    dbscan_result <- dbscan::dbscan(as.matrix(tfidf_matrix), eps = eps, minPts = minPts)
    cat("DBSCAN ejecutado exitosamente\n")
    dbscan_result
  }, error = function(e) {
    cat("ERROR en DBSCAN: ", e$message, "\n")
    return(NULL)
  })

  if (is.null(result)) {
    warning("DBSCAN no se pudo ejecutar. Se devuelve el dataframe original.")
    df$cluster_titulo <- "Error"
    df$cluster_nombre <- "Error"
    return(df)
  }

  df$cluster_titulo <- as.character(result$cluster)
  df$cluster_titulo[df$cluster_titulo == "-1"] <- "Ruido"

  cat("Paso 6: Procesando nombres de clusters\n")
  cluster_names <- df %>%
    filter(cluster_titulo != "Ruido") %>%
    group_by(cluster_titulo) %>%
    summarise(text = paste(title, collapse = " "), .groups = "drop") %>%
    mutate(
      top_words = str_extract_all(text, "\\b\\w{4,}\\b"),
      top_words = lapply(top_words, function(words) {
        words <- tolower(words)
        words <- words[!words %in% stopwords::stopwords("es")]
        head(sort(table(words), decreasing = TRUE), 3) |> names()
      }),
      cluster_nombre = sapply(top_words, function(x) paste(x, collapse = "_"))
    ) %>%
    select(cluster_titulo, cluster_nombre)

  cat("Paso 7: Combinando resultados\n")
  df <- df %>% left_join(cluster_names, by = "cluster_titulo")

  cat("Proceso completado\n")
  return(df)
}


