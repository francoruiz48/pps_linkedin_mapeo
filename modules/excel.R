source("./modules/preprocesamiento.R")
source("./modules/debug.R")

exportar_excel <- function(input, output, datos_filtrados) {
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("tabla_resumen_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      shinyjs::show("loader_div")
      datos <- datos_filtrados()
      debug_msg("Iniciando proceso de exportación de excel", "excel.R")
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
      debug_msg("🟢Finalizo el proceso de exportación de excel con éxito!", "excel.R")
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
        debug_msg("Iniciando proceso de importación de excel", "excel.R")
        nuevo_df <- readxl::read_excel(input$archivo_excel$datapath)
        nuevo_df <- procesar_df(nuevo_df)
        datos_reactivos(nuevo_df)
        showNotification("✅ Archivo cargado y procesado con éxito.", type = "message")
        debug_msg("🟢Finalizo el proceso de importación de excel con éxito!", "excel.R")
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
