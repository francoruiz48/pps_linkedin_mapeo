source("ui/loader.R")
source("ui/excel.R")
source("ui/crud.R")
source("ui/indicadores.R")
source("ui/grafico.R")
source("ui/filters.R")

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  loader_ui,
  navbarPage(
    "Análisis de Oportunidades en LinkedIn",
    tabPanel(
      "Dashboard",
      sidebarLayout(
        filter_ui,
        mainPanel(
          indicadores_ui,
          tabsetPanel(
            tabPanel(
              "Tabla",
              exportar_ui, br(),
              actionButton("re_procesar", "🔄 Reprocesar archivo")
            ),
            tabPanel(
              "Gráfico",
              grafico_ui
            )
          )
        )
      )
    ),
    crud_ui,
    importar_ui
  )
)
