source("./modules/editar_diccionario.R")

function_editar_categorias <- function(input, output, session, datos_reactivos) {
    function_editar_diccionario(
        input, output, session,
        nombre_archivo = RUTA_CATEGORIAS,
        nombre_tabla = "categorias",
        nombre_columna = "categoria",
        nombre_input_agregar = "agregar_categoria",
        nombre_input_nuevo = "nueva_categoria",
        nombre_input_keywords = "nueva_keyword",
        nombre_input_confirmar = "confirmar_nueva_categoria",
        nombre_input_borrar = "borrar_categoria",
        nombre_input_guardar = "guardar_categorias"
    )
}
