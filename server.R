source("./modules/excel.R")
source("./modules/graficos.R")
source("./modules/editar_categorias.R")
source("./modules/editar_sectores.R")
source("./modules/editar_tecnologias.R")
source("./modules/leer_tecnologias.R")


server <- function(input, output, session) {
  editar_categorias <- function_editar_categorias(input, output, session)
  editar_sectores <- function_editar_sectores(input, output, session)
  editar_tecnologias <- function_editar_tecnologias(input, output, session)

  # ✅ Esto se puede ejecutar una sola vez, no es reactivo a input$sector
  observe({
    updateSelectInput(session, "sector",
      choices = c("Todos", sort(unique(df$sector_general)))
    )

    updateSelectInput(session, "categoria",
      choices = c("Todos", sort(unique(df$categoria)))
    )

    updateSelectInput(session, "tecnologia",
      choices = c("Todas", sort(unique(unlist(strsplit(df$tech_tags, ", ")))))
    )
  })

  # ✅ Este bloque se queda solo para actualizar el companyName según el sector
  observeEvent(input$sector, {
    if (input$sector == "Todos") {
      updateSelectizeInput(session, "companyName",
        choices = c("Todos", sort(unique(df$companyName))),
        server = TRUE
      )
    } else {
      updateSelectizeInput(session, "companyName",
        choices = c("Todos", sort(unique(df$companyName[df$sector_general == input$sector]))),
        server = TRUE
      )
    }
  })


  datos_filtrados <- reactive({
    datos <- df
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
    datos
  })

  # Indicadores
  output$total_ofertas <- renderText({
  total_ofertas <- nrow(datos_filtrados())
  paste(total_ofertas)
})

output$sectores_representados <- renderText({
  sectores <- length(unique(datos_filtrados()$sector_general))
  paste(sectores)
})

output$promedio_tecnologias <- renderText({
  tech_counts <- datos_filtrados() %>%
    filter(!is.na(tech_tags), tech_tags != "") %>%
    mutate(n_techs = sapply(strsplit(tech_tags, ",\\s*"), length))
  
  avg_techs <- mean(tech_counts$n_techs)
  paste("En promedio, cada oferta requiere ",round(avg_techs, 2)," tecnologías.")
})

output$tecnologias_unicas <- renderText({
  techs_unicas <- datos_filtrados() %>%
    filter(!is.na(tech_tags), tech_tags != "") %>%
    separate_rows(tech_tags, sep = ",\\s*") %>%
    filter(tech_tags != "") %>%
    distinct(tech_tags) %>%
    nrow()

  paste0("En total se identificaron ", techs_unicas, 
         " tecnologías únicas en las ofertas. Esto refleja la diversidad de herramientas requeridas en el mercado.")
})


output$top_3_tecnologias <- renderText({
  top_3_tech <- datos_filtrados() %>%
    filter(!is.na(tech_tags), tech_tags != "") %>%                         # elimina vacíos o NA
    separate_rows(tech_tags, sep = ",\\s*") %>%
    filter(tech_tags != "") %>%                                           # vuelve a filtrar tras separar
    count(tech_tags, name = "Cantidad") %>%
    arrange(desc(Cantidad)) %>%
    head(3) %>%
    pull(tech_tags) %>%
    paste(collapse = ", ")
  paste(top_3_tech)
})

output$top_3_categorias <- renderText({
  top_3_categorias <- datos_filtrados() %>%
    count(categoria, name = "Cantidad") %>%
    arrange(desc(Cantidad)) %>%
    head(3) %>%
    pull(categoria) %>%
    paste(collapse = ", ")
  paste(top_3_categorias)
})

output$empresa_destacada <- renderText({
  empresa_top <- datos_filtrados() %>%
    count(companyName, name = "Cantidad") %>%
    arrange(desc(Cantidad)) %>%
    slice(1)

  empresa_nombre <- empresa_top$companyName[[1]]
  empresa_cantidad <- empresa_top$Cantidad[[1]]

  paste0("La empresa que publicó más ofertas es ", empresa_nombre, 
         ", con un total de ", empresa_cantidad, 
         " oportunidades disponibles.")
})



output$sector_destacado <- renderText({
  sector_top <- datos_filtrados() %>%
    count(sector, name = "Cantidad") %>%
    arrange(desc(Cantidad)) %>%
    slice(1)

  paste0("El sector con más oportunidades es ", sector_top$sector, 
         ", que concentra ", sector_top$Cantidad, " ofertas en total.")
})



  # Tabla resumen
  output$tabla <- renderDT({
    datos_filtrados() %>%
      group_by(title, companyName, categoria, tech_tags) %>%
      summarise(Cantidad = n(), .groups = "drop") %>%
      arrange(desc(Cantidad))
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


  excel <- function_excel(output, datos_filtrados)
}
