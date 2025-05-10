function_editar_tecnologias <- function(input, output, session) {
  ruta_diccionario <- "./content/reglas-tecnologias.csv"
  diccionario <- reactiveVal(read.csv(ruta_diccionario, stringsAsFactors = FALSE))

  # Tabla editable con selección
  output$tecnologias <- renderDT(
    {
      datatable(diccionario(),
        editable = list(target = "cell", disable = list(columns = NULL)),
        selection = "single", # Selección de una fila
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

  # Editar celdas
  observeEvent(input$tecnologias_cell_edit, {
    info <- input$tecnologias_cell_edit
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

  # Agregar nueva tecnología (con modal)
  observeEvent(input$agregar_tecnologia, {
    showModal(modalDialog(
      title = "Agregar nueva tecnología",
      textInput("nueva_tecnologia", "Nombre de la tecnología:"),
      textInput("nueva_keyword", "Palabras clave (separadas por coma):"),
      easyClose = TRUE,
      footer = tagList(
        modalButton("Cancelar"),
        actionButton("confirmar_nueva_tecnologia", "Agregar")
      )
    ))
  })

  observeEvent(input$confirmar_nueva_tecnologia, {
    removeModal()
    nueva_tecnologia <- input$nueva_tecnologia
    nueva_keyword <- input$nueva_keyword

    if (!is.null(nueva_tecnologia) && nueva_tecnologia != "") {
      df_dicc <- diccionario()
      nueva_fila <- data.frame(tecnologia = nueva_tecnologia, keywords = nueva_keyword, stringsAsFactors = FALSE)
      diccionario(bind_rows(df_dicc, nueva_fila))
      showNotification("✅ Tecnología agregada", type = "message")
    } else {
      showNotification("⚠️ Debes ingresar un nombre para la tecnología", type = "warning")
    }
  })

  # Eliminar tecnología seleccionada
  observeEvent(input$borrar_tecnologia, {
    fila <- input$tecnologias_rows_selected
    if (!is.null(fila)) {
      df_dicc <- diccionario()
      tech_borrada <- df_dicc[fila, "tecnologia"]
      df_dicc <- df_dicc[-fila, ]
      diccionario(df_dicc)
      showNotification(paste("🗑 Tecnología eliminada:", tech_borrada), type = "message")
    } else {
      showNotification("⚠️ Debes seleccionar una tecnología para eliminar", type = "warning")
    }
  })

  # Guardar CSV
  observeEvent(input$guardar_tecnologias, {
    # Guardar archivo CSV
    tryCatch(
      {
        write.csv(diccionario(), ruta_diccionario, row.names = FALSE)
        showNotification("✅ Tecnologías guardadas correctamente", type = "message")
      },
      error = function(e) {
        showNotification(paste("❌ Error al guardar el archivo:", e$message), type = "error")
      }
    )

    # Reclasificar tecnologías
    tryCatch(
      {
        df <<- df %>% mutate(
          tech_tags = sapply(title, function(x) {
            if (is.na(x)) {
              return("")
            }
            detectar_tecnologias(x)
          })
        )


        updateSelectInput(session, "tecnologia",
          choices = c("Todas", sort(unique(unlist(strsplit(df$tech_tags, ",\\s*")))))
        )
      },
      error = function(e) {
        showNotification(paste("❌ Error al reclasificar:", e$message), type = "error")
      }
    )
  })
}
