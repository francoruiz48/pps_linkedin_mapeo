grafico_ui <- tabPanel(
    "Gráfico",
    selectInput("tipo_grafico", "Mostrar gráfico por:",
        choices = c("Categoría" = "categoria", "Tecnología" = "tecnologia")
    ),
    plotOutput("grafico")
)
