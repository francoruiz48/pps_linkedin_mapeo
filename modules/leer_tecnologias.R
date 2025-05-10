detectar_tecnologias <- function(titulo) {
  tryCatch({
    if (length(titulo) != 1 || is.na(titulo) || titulo == "") {
      return("")
    }

    tecnologias_detectadas <- c()
    reglas <- read.csv("content/reglas-tecnologias.csv", stringsAsFactors = FALSE)

    for (i in seq_len(nrow(reglas))) {
      tecnologia <- reglas$tecnologia[i]
      keywords <- reglas$keywords[i]

      if (!is.na(keywords) && keywords != "") {
        palabras <- unlist(strsplit(keywords, ",\\s*"))
        pattern <- paste0("\\b(", paste(palabras, collapse = "|"), ")\\b")

        if (str_detect(titulo, regex(pattern, ignore_case = TRUE))) {
          tecnologias_detectadas <- c(tecnologias_detectadas, tecnologia)
        }
      }
    }

    return(paste(unique(tecnologias_detectadas), collapse = ", "))
  }, error = function(e) {
    message("❌ Error en detectar_tecnologias(): ", e$message)
    return("")
  })
}
