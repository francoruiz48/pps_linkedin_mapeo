function_editar_categorias <- function(input, output, session) {
    ruta_diccionario <- "./content/reglas-categorias.csv"

    # Leer CSV como reactiveVal
    diccionario <- reactiveVal(read.csv(ruta_diccionario, stringsAsFactors = FALSE))

    output$categorias <- renderDT(
        {
            datatable(diccionario(),
                editable = list(target = "cell", disable = list(columns = NULL)),
                options = list(
                    dom = "frtip", # ✔ muestra barra de búsqueda, info y paginación
                    pageLength = 100,
                    lengthMenu = c(10, 15, 25, 50, 100) # menú de selección de filas
                ),
                rownames = FALSE
            )
        },
        server = TRUE
    )


    observeEvent(input$categorias_cell_edit, {
        info <- input$categorias_cell_edit
        df_dicc <- diccionario()

        row <- info$row
        col <- info$col
        value <- info$value

        if (!is.null(row) && !is.null(col) && !is.null(value)) {
            col_name <- colnames(df_dicc)[col + 1] # Corrige el error base-0
            df_dicc[row, col_name] <- as.character(value)
            diccionario(df_dicc) # Actualiza reactiveVal
        }
    })

    observeEvent(input$guardar_categorias, {
        tryCatch(
            {
                # Guardar el archivo CSV editado
                write.csv(diccionario(), ruta_diccionario, row.names = FALSE)
                showNotification("✅ Categorias guardado correctamente", type = "message")

                # 🔄 Releer reglas actualizadas
                nuevas_reglas <- read.csv(ruta_diccionario, stringsAsFactors = FALSE)

                # 🔁 Reclasificar categorías
                df <<- df %>%
                    mutate(categoria = sapply(title, function(x) clasificar_desde_reglas(tolower(x), nuevas_reglas)))

                # 🔄 Actualizar opciones del selectInput de categoría
                updateSelectInput(session, "categoria",
                    choices = c("Todos", sort(unique(df$categoria)))
                )
            },
            error = function(e) {
                showNotification(paste("❌ Error al guardar:", e$message), type = "error")
            }
        )
    })
}
