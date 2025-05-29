crud_ui <- navbarMenu(
    "🛠 CRUD",
    tabPanel(
        "Categorías",
        h2("Reglas Heurísticas de Categorías"),
        div(
            class = "action-box",
            tags$h4("Acciones"),
            actionButton("agregar_categoria", "➕ Agregar"),
            actionButton("borrar_categoria", "🗑 Borrar"),
            actionButton("guardar_categoria", "💾 Guardar")
        ),
        br(), 
        p("Haz doble clic sobre la celda para editar, puedes asociar más un keyword separando por comas sin dejar espacio en el medio. Recuerda guardar siempre tus cambios."),
        dataTableOutput("categorias")
    ),
    tabPanel(
        "Tecnologías",
        h2("Reglas Heurísticas de Tecnologías"),
        div(
            class = "action-box",
            tags$h4("Acciones"),
            actionButton("agregar_tecnologia", "➕ Agregar"),
            actionButton("borrar_tecnologia", "🗑 Borrar"),
            actionButton("guardar_tecnologias", "💾 Guardar")
        ),
        br(), 
        p("Haz doble clic sobre la celda para editar, puedes asociar más un keyword separando por comas sin dejar espacio en el medio. Recuerda guardar siempre tus cambios."),
        DTOutput("tecnologias"),
    ),
    tabPanel(
        "Sectores",
        h2("Reglas Heurísticas de Sectores"),
        div(
            class = "action-box",
            tags$h4("Acciones"),
            actionButton("agregar_sector", "➕ Agregar"),
            actionButton("borrar_sector", "🗑 Borrar"),
            actionButton("guardar_sector", "💾 Guardar")
        ),
        br(), 
        p("Haz doble clic sobre la celda para editar, puedes asociar más un keyword separando por comas sin dejar espacio en el medio. Recuerda guardar siempre tus cambios."),
        dataTableOutput("sectores")
    )
)
