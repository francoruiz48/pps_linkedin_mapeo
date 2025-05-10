function_editar_sectores <- function(input, output, session) {
    ruta_diccionario <- "./content/reglas-sectores.csv"

    # Leer CSV como reactiveVal
    diccionario <- reactiveVal(read.csv(ruta_diccionario, stringsAsFactors = FALSE))

    output$sectores <- renderDT(
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


    observeEvent(input$sectores_cell_edit, {
        info <- input$sectores_cell_edit
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

    observeEvent(input$guardar_sectores, {
        tryCatch(
            {
                # Guardar el archivo CSV editado
                write.csv(diccionario(), ruta_diccionario, row.names = FALSE)
                showNotification("✅ Sectores guardado correctamente", type = "message")

                # 🔄 Releer reglas actualizadas
                nuevas_reglas <- read.csv(ruta_diccionario, stringsAsFactors = FALSE)

                # 🔁 Reclasificar
                df <<- df %>%
                    mutate(sector = sapply(title, function(x) clasificar_desde_reglas(tolower(x), nuevas_reglas)))

                # 🔄 Actualizar opciones del selectInput
                updateSelectInput(session, "sector",
                    choices = c("Todos", sort(unique(df$sector)))
                )
            },
            error = function(e) {
                showNotification(paste("❌ Error al guardar:", e$message), type = "error")
            }
        )
    })
}
