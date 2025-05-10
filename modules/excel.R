function_excel <- function(output, datos_filtrados) {
    # Descargar como Excel
  output$descargar_excel <- downloadHandler(
    filename = function() {
      paste0("datos_filtrados_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(datos_filtrados(), path = file)
    }
  )
}