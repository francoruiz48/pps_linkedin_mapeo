function_editar_sectores <- function(input, output, session, datos_reactivos) {
    # Leer CSV como reactiveVal
    diccionario <- reactiveVal(read.csv(RUTA_SECTORES, stringsAsFactors = FALSE))

    output$sectores <- renderDT(
        {
            datatable(diccionario(),
                editable = list(target = "cell", disable = list(columns = NULL)),
                selection = "single",
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

    # Editar celdas
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

    # Agregar nuevo sector (con modal)
    observeEvent(input$agregar_sector, {
        showModal(modalDialog(
            title = "Agregar nuevo sector",
            textInput("nuevo_sector", "Nombre del Sector:"),
            textInput("nueva_keyword", "Palabras clave (separadas por coma):"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("confirmar_nuevo_sector", "Agregar")
            )
        ))
    })

    observeEvent(input$confirmar_nuevo_sector, {
        removeModal()
        nuevo_sector <- input$nuevo_sector
        nueva_keyword <- input$nueva_keyword

        if (!is.null(nuevo_sector) && nuevo_sector != "") {
            df_dicc <- diccionario()
            nueva_fila <- data.frame(sector_general = nuevo_sector, keywords = nueva_keyword, stringsAsFactors = FALSE)
            df_dicc <- bind_rows(df_dicc, nueva_fila)
            diccionario(df_dicc)

            # 🔄 Guardar automáticamente al agregar
            tryCatch(
                {
                    write.csv(df_dicc, RUTA_SECTORES, row.names = FALSE)
                    showNotification("✅ Sector agregado y guardado", type = "message")
                },
                error = function(e) {
                    showNotification(paste("❌ Error al guardar:", e$message), type = "error")
                }
            )
        } else {
            showNotification("⚠️ Debes ingresar un nombre para el sector", type = "warning")
        }
    })

    # Eliminar sector seleccionado
    observeEvent(input$borrar_sector, {
        fila <- input$sectores_rows_selected
        if (!is.null(fila)) {
            df_dicc <- diccionario()
            sector_borrado <- df_dicc[fila, "sector_general"]
            df_dicc <- df_dicc[-fila, ]
            diccionario(df_dicc)
            showNotification(paste("🗑 Sector eliminado:", sector_borrado), type = "message")
        } else {
            showNotification("⚠️ Debes seleccionar un sector para eliminar", type = "warning")
        }
    })

    observeEvent(input$guardar_sectores, {
        tryCatch(
            {
                # Guardar el archivo CSV editado # nolint
                write.csv(diccionario(), RUTA_SECTORES, row.names = FALSE)
                showNotification("✅ Sectores guardado correctamente", type = "message")
            },
            error = function(e) {
                showNotification(paste("❌ Error al guardar:", e$message), type = "error") # nolint: indentation_linter.
            }
        )
    })
}
