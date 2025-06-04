
source("./modules/util.R")

function_editar_diccionario <- function(input, output, session,
                                        nombre_archivo,
                                        nombre_tabla,
                                        nombre_columna,
                                        nombre_input_agregar,
                                        nombre_input_nuevo,
                                        nombre_input_keywords,
                                        nombre_input_confirmar,
                                        nombre_input_borrar,
                                        nombre_input_guardar) {
    diccionario <- reactiveVal(read.csv(nombre_archivo, stringsAsFactors = FALSE))

    output[[nombre_tabla]] <- renderDT(
        {
            datatable(diccionario(),
                editable = list(target = "cell", disable = list(columns = NULL)),
                selection = "single",
                options = list(
                    dom = "frtip",
                    pageLength = 100,
                    lengthMenu = c(10, 15, 25, 50, 100)
                ),
                rownames = FALSE
            )
        },
        server = TRUE
    )

    observeEvent(input[[paste0(nombre_tabla, "_cell_edit")]], {
        info <- input[[paste0(nombre_tabla, "_cell_edit")]]
        df_dicc <- diccionario()
        row <- info$row
        col <- info$col
        value <- info$value

        if (!is.null(row) && !is.null(col) && !is.null(value)) {
            col_name <- colnames(df_dicc)[col + 1]
            df_dicc[row, col_name] <- as.character(value)
            diccionario(df_dicc)
        }
    })

    observeEvent(input[[nombre_input_agregar]], {
        showModal(modalDialog(
            title = paste("Agregar nueva", nombre_columna),
            textInput(nombre_input_nuevo, paste("Nombre de la", nombre_columna, ":")),
            textInput(nombre_input_keywords, "Palabras clave (separadas por coma):"),
            easyClose = TRUE,
            footer = tagList(
                modalButton("Cancelar"),
                actionButton(nombre_input_confirmar, "Agregar")
            )
        ))
    })

    observeEvent(input[[nombre_input_confirmar]], {
        removeModal()
        nuevo_nombre <- input[[nombre_input_nuevo]]
        nueva_keyword <- input[[nombre_input_keywords]]

        if (!is.null(nuevo_nombre) && nuevo_nombre != "") {
            df_dicc <- diccionario()
            nueva_fila <- data.frame(setNames(list(nuevo_nombre, nueva_keyword), c(nombre_columna, "keywords")),
                stringsAsFactors = FALSE
            )
            df_dicc <- bind_rows(df_dicc, nueva_fila)
            diccionario(df_dicc)
            guardar(diccionario, nombre_archivo)
        } else {
            showNotification(paste("⚠️ Debes ingresar un nombre para la", nombre_columna), type = "warning")
        }
    })

    observeEvent(input[[nombre_input_borrar]], {
        fila <- input[[paste0(nombre_tabla, "_rows_selected")]]
        if (!is.null(fila)) {
            df_dicc <- diccionario()
            elemento_borrado <- df_dicc[fila, nombre_columna]
            df_dicc <- df_dicc[-fila, ]
            diccionario(df_dicc)
            showNotification(paste("🗑", nombre_columna, "eliminada:", elemento_borrado), type = "message")
        } else {
            showNotification(paste("⚠️ Debes seleccionar una", nombre_columna, "para eliminar"), type = "warning")
        }
    })

    observeEvent(input[[nombre_input_guardar]], {
        guardar(diccionario, nombre_archivo)
    })
}
