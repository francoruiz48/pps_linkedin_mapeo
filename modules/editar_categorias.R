function_editar_categorias <- function(input, output, session, datos_reactivos) {
    diccionario <- reactiveVal(read.csv(RUTA_CATEGORIAS, stringsAsFactors = FALSE))

    output$categorias <- renderDT(
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

    # Agregar nueva categoria (con modal)
    observeEvent(input$agregar_categoria, {
        showModal(modalDialog(
            title = "Agregar nueva categoría",
            textInput("nueva_categoria", "Nombre de la categoría:"),
            textInput("nueva_keyword", "Palabras clave (separadas por coma):"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancelar"),
                actionButton("confirmar_nueva_categoria", "Agregar")
            )
        ))
    })

    observeEvent(input$confirmar_nueva_categoria, {
        removeModal()
        nueva_categoria <- input$nueva_categoria
        nueva_keyword <- input$nueva_keyword

        if (!is.null(nueva_categoria) && nueva_categoria != "") {
            df_dicc <- diccionario()
            nueva_fila <- data.frame(categoria = nueva_categoria, keywords = nueva_keyword, stringsAsFactors = FALSE)
            df_dicc <- bind_rows(df_dicc, nueva_fila)
            diccionario(df_dicc)

            # 🔄 Guardar automáticamente al agregar
            tryCatch(
                {
                    write.csv(df_dicc, RUTA_CATEGORIAS, row.names = FALSE)
                    showNotification("✅ Categoría agregada y guardada", type = "message")
                },
                error = function(e) {
                    showNotification(paste("❌ Error al guardar:", e$message), type = "error")
                }
            )
        } else {
            showNotification("⚠️ Debes ingresar un nombre para la categoría", type = "warning")
        }
    })

    # Eliminar categoría seleccionada
    observeEvent(input$borrar_categoria, {
        fila <- input$categorias_rows_selected
        if (!is.null(fila)) {
            df_dicc <- diccionario()
            tech_borrada <- df_dicc[fila, "categoria"]
            df_dicc <- df_dicc[-fila, ]
            diccionario(df_dicc)
            showNotification(paste("🗑 Categoría eliminada:", tech_borrada), type = "message")
        } else {
            showNotification("⚠️ Debes seleccionar una categoría para eliminar", type = "warning")
        }
    })

    # Guardar CSV
    observeEvent(input$guardar_categorias, {
        # Guardar archivo CSV
        tryCatch(
            {
                # Guardar el archivo CSV editado
                write.csv(diccionario(), RUTA_CATEGORIAS, row.names = FALSE)
                showNotification("✅ Categorias guardado correctamente", type = "message")
            },
            error = function(e) {
                showNotification(paste("❌ Error al guardar:", e$message), type = "error")
            }
        )
    })
}
