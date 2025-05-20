source("./modules/debug.R")

# Función para clusterizar nombres similares
agrupar_fuzzy <- function(lista, max_dist = 0.2, label = "valor") {
  debug_msg("Comenzando agrupamiento fuzzy", "clustering.R")
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

  debug_msg("🟢Finalizo el proceso de agrupación fuzzy con éxito!", "clustering.R")

  return(mapping)
}

dbscan_titulos <- function(df, eps = 1.2, minPts = 5) {
  debug_msg("Iniciando proceso de dbscan de title", "clustering.R")
  # Verificar que df es válido
  if (!inherits(df, "data.frame") && !inherits(df, "tibble")) {
    stop("El objeto 'df' no es un data.frame ni un tibble.")
  }
  
  # Corpus
  titles <- tolower(df$title)
  it <- itoken(titles, progressbar = FALSE)

  # Vocabulario y TF-IDF
  vocab <- create_vocabulary(it)
  vectorizer <- vocab_vectorizer(vocab)
  dtm <- create_dtm(it, vectorizer)
  tfidf_transformer <- TfIdf$new()
  tfidf_matrix <- tfidf_transformer$fit_transform(dtm)

  # DBSCAN clustering
  set.seed(123)
  dbscan_result <- dbscan::dbscan(tfidf_matrix, eps = eps, minPts = minPts)

  # Asignar cluster a cada título (ruido = -1)
  df$cluster_titulo <- as.character(dbscan_result$cluster)
  df$cluster_titulo[df$cluster_titulo == "0"] <- "Ruido"

  cluster_names <- df %>%
    filter(cluster_titulo != -1) %>% # -1 es ruido en DBSCAN
    group_by(cluster_titulo) %>%
    summarise(text = paste(title, collapse = " ")) %>%
    mutate(
      # Paso 2: Extraer palabras más comunes (excepto stopwords)
      top_words = str_extract_all(text, "\\b\\w{4,}\\b"),
      top_words = lapply(top_words, function(words) {
        words <- tolower(words)
        words <- words[!words %in% stopwords::stopwords("es")]
        head(sort(table(words), decreasing = TRUE), 3) |> names()
      }),
      # Paso 3: Unir palabras frecuentes como nombre de cluster
      cluster_nombre = sapply(top_words, function(x) paste(x, collapse = "_"))
    ) %>%
    select(cluster_titulo, cluster_nombre)

  # Paso 4: Unir al dataframe original
  df <- df %>%
    left_join(cluster_names, by = "cluster_titulo")
  debug_msg("🟢Finalizo el proceso de dbscan con title con éxito!", "clustering.R")
  return(df)
}
