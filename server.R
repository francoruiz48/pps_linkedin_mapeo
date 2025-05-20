source("./modules/excel.R")
source("./modules/graficos.R")
source("./modules/editar_categorias.R")
source("./modules/editar_sectores.R")
source("./modules/editar_tecnologias.R")
source("./modules/leer_tecnologias.R")
source("./modules/preprocesamiento.R")


server <- function(input, output, session) {
  
  df <- readxl::read_excel("./content/combined_linkedin_jobs_no_duplicates.xlsx")

  df <- procesar_df(df)
  datos_reactivos <- reactiveVal(df)

  importar_excel(input, output, session, datos_reactivos)

  editar_categorias <- function_editar_categorias(input, output, session)
  editar_sectores <- function_editar_sectores(input, output, session)
  editar_tecnologias <- function_editar_tecnologias(input, output, session)

  # ✅ Esto se puede ejecutar una sola vez, no es reactivo a input$sector
  observe({
    datos <- datos_reactivos()
    updateSelectInput(session, "sector",
      choices = c("Todos", sort(unique(datos$sector_general)))
    )

    updateSelectInput(session, "categoria",
      choices = c("Todos", sort(unique(datos$categoria)))
    )

    updateSelectInput(session, "tecnologia",
      choices = c("Todas", sort(unique(unlist(strsplit(datos$tech_tags, ", ")))))
    )

    updateSelectInput(session, "pais_cluster",
      choices = c("Todos", sort(unique(datos$pais_cluster)))
    )

    updateSelectInput(session, "ciudad_cluster",
      choices = c("Todos", sort(unique(datos$ciudad_cluster)))
    )
  })

  # ✅ Este bloque se queda solo para actualizar el companyName según el sector
  observeEvent(input$sector, {
    datos <- datos_reactivos()
    if (input$sector == "Todos") {
      updateSelectizeInput(session, "companyName",
        choices = c("Todos", sort(unique(datos$companyName))),
        server = TRUE
      )
    } else {
      updateSelectizeInput(session, "companyName",
        choices = c("Todos", sort(unique(datos$companyName[datos$sector_general == input$sector]))),
        server = TRUE
      )
    }
  })

  observeEvent(input$pais_cluster, {
    datos <- datos_reactivos()
    if (input$pais_cluster == "Todos") {
      updateSelectInput(session, "ciudad_cluster",
        choices = c("Todos", sort(unique(datos$ciudad_cluster)))
      )
    } else {
      ciudades_filtradas <- datos %>%
        filter(pais_cluster == input$pais_cluster) %>%
        pull(ciudad_cluster) %>%
        unique() %>%
        sort()

      updateSelectInput(session, "ciudad_cluster",
        choices = c("Todos", ciudades_filtradas)
      )
    }
  })

  datos_filtrados <- reactive({
    datos <- datos_reactivos()
    if (input$sector != "Todos") {
      datos <- datos %>% filter(sector_general == input$sector)
    }

    if (input$companyName != "Todos") {
      datos <- datos %>% filter(companyName == input$companyName)
    }
    if (input$categoria != "Todos") {
      datos <- datos %>% filter(categoria == input$categoria)
    }
    if (input$tecnologia != "Todas") {
      datos <- datos %>% filter(str_detect(tech_tags, fixed(input$tecnologia)))
    }
    if (input$pais_cluster != "Todos") {
      datos <- datos %>% filter(pais_cluster == input$pais_cluster)
    }
    if (input$ciudad_cluster != "Todos") {
      datos <- datos %>% filter(ciudad_cluster == input$ciudad_cluster)
    }
    datos
  })

  # Indicadores
  output$total_ofertas <- renderText({
    datos <- datos_filtrados() 
    total_ofertas <- nrow(datos)
    paste(total_ofertas)
  })

  output$sectores_representados <- renderText({
    datos <- datos_filtrados() 
    sectores <- length(unique(datos$sector_general))
    paste(sectores)
  })

  output$promedio_tecnologias <- renderText({
    datos <- datos_filtrados() 
    tech_counts <- datos %>%
      filter(!is.na(tech_tags), tech_tags != "") %>%
      mutate(n_techs = sapply(strsplit(tech_tags, ",\\s*"), function(x) {
        if (length(x) == 1 && x[1] == "") {
          return(0)
        } # caso especial: una cadena vacía
        length(x)
      }))

    if (nrow(tech_counts) == 0 || all(is.na(tech_counts$n_techs))) {
      return("No hay datos suficientes para calcular el promedio de tecnologías.")
    }

    avg_techs <- mean(as.numeric(tech_counts$n_techs), na.rm = TRUE)
    paste("En promedio, cada oferta requiere", round(avg_techs, 2), "tecnologías.")
  })


  output$tecnologias_unicas <- renderText({
    datos <- datos_filtrados() 
    techs_unicas <- datos %>%
      filter(!is.na(tech_tags), tech_tags != "") %>%
      separate_rows(tech_tags, sep = ",\\s*") %>%
      filter(tech_tags != "") %>%
      distinct(tech_tags) %>%
      nrow()

    paste0(
      "En total se identificaron ", techs_unicas,
      " tecnologías únicas en las ofertas. Esto refleja la diversidad de herramientas requeridas en el mercado."
    )
  })


  output$top_3_tecnologias <- renderText({
    datos <- datos_filtrados() 
    top_3_tech <- datos %>%
      filter(!is.na(tech_tags), tech_tags != "") %>%
      separate_rows(tech_tags, sep = ",\\s*") %>%
      filter(tech_tags != "") %>%
      count(tech_tags, name = "Cantidad") %>%
      arrange(desc(Cantidad)) %>%
      head(3) %>%
      pull(tech_tags) %>%
      paste(collapse = ", ")
    paste(top_3_tech)
  })

  output$top_3_categorias <- renderText({
    datos <- datos_filtrados() 
    top_3_categorias <- datos %>%
      count(categoria, name = "Cantidad") %>%
      arrange(desc(Cantidad)) %>%
      head(3) %>%
      pull(categoria) %>%
      paste(collapse = ", ")
    paste(top_3_categorias)
  })

  output$empresa_destacada <- renderText({
    datos <- datos_filtrados() 
    empresa_top <- datos %>%
      count(companyName, name = "Cantidad") %>%
      arrange(desc(Cantidad)) %>%
      slice(1)

    if (nrow(empresa_top) > 0) {
      empresa_nombre <- empresa_top$companyName[[1]]
      empresa_cantidad <- empresa_top$Cantidad[[1]]

      paste0(
        "La empresa que publicó más ofertas es ", empresa_nombre,
        ", con un total de ", empresa_cantidad,
        " oportunidades disponibles."
      )
    } else {
      "No se encontraron datos para determinar la empresa destacada."
    }
  })


  output$sector_destacado <- renderText({
    datos <- datos_filtrados() 
    sector_top <- datos %>%
      count(sector, name = "Cantidad") %>%
      arrange(desc(Cantidad)) %>%
      slice(1)

    if (nrow(sector_top) > 0) {
      paste0(
        "El sector con más oportunidades es ", sector_top$sector,
        ", que concentra ", sector_top$Cantidad, " ofertas en total."
      )
    } else {
      "No se encontraron datos para determinar el sector destacado."
    }
  })

  # Tabla resumen
  output$tabla <- renderDT({
    datos <- datos_filtrados() 

    # Si no se selecciona nada, usar todo el dataset sin agrupar
    if (is.null(input$agrupacion) || length(input$agrupacion) == 0) {
      return(datatable(datos))
    }

    datos %>%
      group_by(across(all_of(input$agrupacion))) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad)) %>%
      rename_with(
        ~ recode(.x,
          title = "Título",
          cluster_nombre = "Cluster",
          companyName = "Empresa",
          categoria = "Categoría",
          tech_tags = "Tecnologías",
          ciudad_cluster = "Ciudad",
          pais_cluster = "País"
        ),
        .cols = input$agrupacion
      )
  })




  output$grafico <- renderPlot({
    datos <- datos_filtrados() 
    req(nrow(datos) > 0)

    if (input$tipo_grafico == "categoria") {
      req("categoria" %in% names(datos))

      datos %>%
        count(categoria, name = "Cantidad") %>%
        ggplot(aes(x = reorder(categoria, Cantidad), y = Cantidad)) +
        geom_bar(stat = "identity", fill = "#7b2cbf") +
        geom_text(aes(label = Cantidad), hjust = -0.2, size = 4, color = "black") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Distribución por Categoría", x = "", y = "") +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      req("tech_tags" %in% names(datos))

      datos %>%
        filter(!is.na(tech_tags), tech_tags != "") %>%
        separate_rows(tech_tags, sep = ",\\s*") %>%
        count(tech_tags, name = "Cantidad") %>%
        ggplot(aes(x = reorder(tech_tags, Cantidad), y = Cantidad)) +
        geom_bar(stat = "identity", fill = "#3f37c9") +
        geom_text(aes(label = Cantidad), hjust = -0.2, size = 4, color = "black") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Distribución por Tecnología", x = "", y = "") +
        theme(plot.title = element_text(hjust = 0.5))
    }
  })

  excel <- exportar_excel(input, output, datos_filtrados)
}
