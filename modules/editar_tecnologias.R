source("./modules/editar_diccionario.R")

function_editar_tecnologias <- function(input, output, session, datos_reactivos) {
  function_editar_diccionario(
    input, output, session,
    nombre_archivo = RUTA_TECNOLOGIAS,
    nombre_tabla = "tecnologias",
    nombre_columna = "tecnologia",
    nombre_input_agregar = "agregar_tecnologia",
    nombre_input_nuevo = "nueva_tecnologia",
    nombre_input_keywords = "nueva_keyword",
    nombre_input_confirmar = "confirmar_nueva_tecnologia",
    nombre_input_borrar = "borrar_tecnologia",
    nombre_input_guardar = "guardar_tecnologias"
  )
}
