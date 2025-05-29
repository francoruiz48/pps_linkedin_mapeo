indicadores_ui <- if (INDICADORES) {
    tagList(
        fluidRow(
            column(4, div(class = "card", div(class = "card-title", "Total Ofertas"), div(class = "card-body", textOutput("total_ofertas")))),
            column(4, div(class = "card", div(class = "card-title", "Sectores Representados"), div(class = "card-body", textOutput("sectores_representados")))),
            column(4, div(class = "card", div(class = "card-title", "Promedio Tecnologías"), div(class = "card-body", textOutput("promedio_tecnologias"))))
        ),
        fluidRow(
            column(4, div(class = "card", div(class = "card-title", "Tecnologías Únicas"), div(class = "card-body", textOutput("tecnologias_unicas")))),
            column(4, div(class = "card", div(class = "card-title", "Top 3 Tecnologías"), div(class = "card-body", textOutput("top_3_tecnologias")))),
            column(4, div(class = "card", div(class = "card-title", "Top 3 Categorías"), div(class = "card-body", textOutput("top_3_categorias"))))
        ),
        fluidRow(
            column(6, div(class = "card", div(class = "card-title", "Empresa con más Publicaciones"), div(class = "card-body", textOutput("empresa_destacada")))),
            column(6, div(class = "card", div(class = "card-title", "Sector con más Oportunidades"), div(class = "card-body", textOutput("sector_destacado"))))
        )
    )
}
