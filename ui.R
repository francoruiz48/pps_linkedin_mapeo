ui <- fluidPage(
  useShinyjs(),
  # 🔄 Pantalla de carga
  tags$head(
    tags$style(HTML("
      #loader_div {
        position: fixed;
        top: 0; left: 0;
        width: 100%;
        height: 100%;
        background: rgba(255,255,255,0.8);
        z-index: 9999;
        display: none;
        text-align: center;
        padding-top: 200px;
        font-size: 24px;
        color: #3f0373;
        font-weight: bold;
      }
    "))
  ),
  div(
    id = "loader_div",
    "Procesando archivo...",
    tags$br(),
    tags$img(src = "https://i.imgur.com/llF5iyg.gif", height = "80px") # Spinner de carga
  ),
  navbarPage(
    "Análisis de Oportunidades en LinkedIn",
    header = tags$head(
      tags$style(HTML("
        body {
          font-family: 'Segoe UI', sans-serif;
          background-color: #f4f4f8;
        }

        /* Navbar */
        .navbar {
          background: linear-gradient(90deg, #7b2cbf, #3f37c9);
          border: none;
          width: 100% !important;
        }

        .navbar .navbar-brand,
        .navbar-nav > li > a {
          color: white !important;
          font-weight: bold;
        }

        .navbar-nav > .active > a {
          background-color: rgba(255,255,255,0.2) !important;
          color: white !important;
        }

        /* Cards sin transparencia */
        .card {
          background-color: #7b2cbf; /* Fondo morado sólido */
          color: white;
          border: 1px solid rgba(123, 44, 191, 0.8); /* Borde morado suave */
          border-radius: 10px; /* Bordes redondeados */
          padding: 20px;
          margin-bottom: 15px;
          box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* Sombra sutil */
        }

        .card-title {
          font-size: 20px;
          font-weight: bold;
        }

        .card-body {
          font-size: 16px;
        }

        /* Tabs/Pestañas */
        .nav-tabs > li.active > a,
        .nav-tabs > li.active > a:focus,
        .nav-tabs > li.active > a:hover {
          background-color: #7b2cbf !important;
          color: white !important;
          border: none !important;
        }

        .nav-tabs > li > a {
          color: #7b2cbf;
          font-weight: 600;
        }

        /* Botón guardar, descargar, etc */
        .btn {
          background-color: #7b2cbf !important;
          color: white !important;
          border: none;
        }

        .btn:hover {
          background-color: #5a189a !important;
        }

        /* Selectores */
        .selectize-control.single .selectize-input {
          background-color: white;
          border-color: #7b2cbf;
        }

        .selectize-input.items.has-items {
          background-color: #e0d4f7 !important;
          border-color: #7b2cbf !important;
          color: #3f0373;
        }

        .selectize-dropdown {
          border-color: #7b2cbf !important;
        }
      "))
    ),
    tabPanel(
      "Dashboard",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "agrupacion",
            label = "Agrupar por:",
            choices = c(
              "Título" = "title",
              "Cluster" = "cluster_nombre",
              "Empresa" = "companyName",
              "Categoría" = "categoria",
              "Tecnologías" = "tech_tags",
              "Ciudad" = "ciudad_cluster",
              "País" = "pais_cluster"
            ),
            multiple = TRUE,
            selected = c("title") # Selección por defecto
          ),
          selectizeInput("companyName", "Selecciona la Compañía:", choices = NULL),
          selectInput("sector", "Selecciona el Sector:", choices = NULL),
          selectInput("categoria", "Selecciona la Categoría:", choices = NULL),
          selectInput("tecnologia", "Selecciona una Tecnología:", choices = NULL),
          selectInput("pais_cluster", "Selecciona el País:", choices = NULL),
          selectInput("ciudad_cluster", "Selecciona la Ciudad:", choices = NULL)
        ),
        mainPanel(
          fluidRow(
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Total Ofertas"),
                div(class = "card-body", textOutput("total_ofertas"))
              )
            ),
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Sectores Representados"),
                div(class = "card-body", textOutput("sectores_representados"))
              )
            ),
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Promedio Tecnologías"),
                div(class = "card-body", textOutput("promedio_tecnologias"))
              )
            )
          ),
          fluidRow(
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Tecnologías Únicas"),
                div(class = "card-body", textOutput("tecnologias_unicas"))
              )
            ),
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Top 3 Tecnologías"),
                div(class = "card-body", textOutput("top_3_tecnologias"))
              )
            ),
            column(
              4,
              div(
                class = "card",
                div(class = "card-title", "Top 3 Categorías"),
                div(class = "card-body", textOutput("top_3_categorias"))
              )
            )
          ),
          fluidRow(
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Empresa con más Publicaciones"),
                div(class = "card-body", textOutput("empresa_destacada"))
              )
            ),
            column(
              6,
              div(
                class = "card",
                div(class = "card-title", "Sector con más Oportunidades"),
                div(class = "card-body", textOutput("sector_destacado"))
              )
            )
          ),
          tabsetPanel(
            tabPanel(
              "Tabla",
              withSpinner(dataTableOutput("tabla")),
              downloadButton("descargar_excel", "Descargar Excel")
            ),
            tabPanel(
              "Gráfico",
              selectInput("tipo_grafico", "Mostrar gráfico por:",
                choices = c("Categoría" = "categoria", "Tecnología" = "tecnologia")
              ),
              plotOutput("grafico")
            )
          )
        )
      )
    ),
    navbarMenu(
      "🛠 CRUD",
      tabPanel(
        "Categorías",
        dataTableOutput("categorias"),
        actionButton("guardar_categorias", "💾 Guardar")
      ),
      tabPanel(
        "Tecnologías",
        br(), "Haz doble clic sobre la celda para editar. También podés agregar o eliminar tecnologías.",
        DTOutput("tecnologias"),
        br(),
        fluidRow(
          column(4, actionButton("agregar_tecnologia", "➕ Agregar")),
          column(4, actionButton("borrar_tecnologia", "🗑 Borrar")),
          column(4, actionButton("guardar_tecnologias", "💾 Guardar"))
        )
      ),
      tabPanel(
        "Sectores",
        dataTableOutput("sectores"),
        actionButton("guardar_sectores", "💾 Guardar")
      )
    ),
    tabPanel(
      '📁 Importar',
      fileInput("archivo_excel", "Subir archivo Excel con oportunidades", 
          accept = c(".xlsx", ".xls")),
      actionButton("cargar_archivo", "Cargar archivo")
    )
  )
)
