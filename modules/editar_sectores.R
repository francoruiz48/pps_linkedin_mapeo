source("./modules/editar_diccionario.R")

function_editar_sectores <- function(input, output, session, datos_reactivos) {
    function_editar_diccionario(
        input, output, session,
        nombre_archivo = RUTA_SECTORES,
        nombre_tabla = "sectores",
        nombre_columna = "sector_general",
        nombre_input_agregar = "agregar_sector",
        nombre_input_nuevo = "nuevo_sector",
        nombre_input_keywords = "nueva_keyword",
        nombre_input_confirmar = "confirmar_nuevo_sector",
        nombre_input_borrar = "borrar_sector",
        nombre_input_guardar = "guardar_sectores"
    )
}
