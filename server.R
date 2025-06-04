source("./modules/excel.R")
source("./modules/graficos.R")
source("./modules/editar_categorias.R")
source("./modules/editar_sectores.R")
source("./modules/editar_tecnologias.R")
source("./modules/preprocesamiento.R")
source("./modules/filtros.R")
source("./modules/tabla.R")
source("./modules/load_data.R")

server <- function(input, output, session) {
  #Procesado de datos
  datos_reactivos <- check_data()
  observeEvent(input$re_procesar, {
    datos_reactivos <- check_data(TRUE)
  })

  #Dashboard
  datos_filtrados <- filtros_server(input, output, session, datos_reactivos)
  tabla_resumen(input, output, datos_filtrados, datos)
  function_grafico_barras(input, output, datos_filtrados) 

  #CRUD
  function_editar_categorias(input, output, session, datos_reactivos)
  function_editar_sectores(input, output, session, datos_reactivos)
  function_editar_tecnologias(input, output, session, datos_reactivos) 

  #Excel
  exportar_excel(input, output, datos_filtrados)
  importar_excel(input, output, session, datos_reactivos)
}