importar_ui <- tabPanel(
    "📁 Importar",
    fileInput("archivo_excel", "Subir archivo Excel con oportunidades",
        accept = c(".xlsx", ".xls")
    ),
    actionButton("cargar_archivo", "Cargar archivo")
)
exportar_ui <- tabPanel(
    "Tabla",
    withSpinner(dataTableOutput("tabla")),
    downloadButton("descargar_excel", "Descargar Excel")
)
