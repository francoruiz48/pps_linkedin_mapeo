filter_ui <- sidebarPanel(
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
)
