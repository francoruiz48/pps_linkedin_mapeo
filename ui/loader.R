loader_ui <- div(
  id = "loader_div", "Procesando archivo...",
  tags$br(),
  tags$img(src = "LoaderSpinner.gif", height = "80px") # Ruta relativa desde www/
)
