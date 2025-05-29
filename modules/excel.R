source("./modules/preprocesamiento.R")

exportar_excel <- function(input, output, datos_filtrados) {
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("tabla_resumen_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shinyjs::show("loader_div")
      datos <- datos_filtrados()

      showNotification("Iniciando proceso de exportación de excel", type = "message")

      if (is.null(input$agrupacion) || length(input$agrupacion) == 0) {
        write_xlsx(datos, path = file)
      } else {
        resumen <- datos %>%
          group_by(across(all_of(input$agrupacion))) %>%
          summarise(Cantidad = n(), .groups = "drop") %>%
          arrange(desc(Cantidad)) %>%
          rename_with(
            ~ recode(.x,
              title = "Título",
              cluster_nombre = "Cluster",
              companyName = "Empresa",
              categoria = "Categoría",
              tech_tags = "Tecnologías",
              ciudad_cluster = "Ciudad",
              pais_cluster = "País"
            ),
            .cols = input$agrupacion
          )

        write_xlsx(resumen, path = file)
        
      }
      showNotification("✅ Finalizo el proceso de exportación de excel con éxito!", type = "message")
      shinyjs::hide("loader_div")
    }
    
  )
}

importar_excel <- function(input, output, session, datos_reactivos) {
  
  observeEvent(input$cargar_archivo, {
    req(input$archivo_excel)

    # Mostrar pantalla de carga
    shinyjs::show("loader_div")
    tryCatch(
      {
        showNotification("Iniciando proceso de importación de excel", type = "message")
        nuevo_df <- readxl::read_excel(input$archivo_excel$datapath)
        nuevo_df <- procesar_df(nuevo_df)
        datos_reactivos(nuevo_df)
        saveRDS(nuevo_df, RUTA_RDS)
        showNotification("✅ Archivo cargado y procesado con éxito.", type = "message")
      },
      error = function(e) {
        showNotification(paste("❌ Error al cargar archivo:", e$message), type = "error")
      },
      finally = {
        # Ocultar pantalla de carga
        shinyjs::hide("loader_div")
      }
    )
  })
  
}
