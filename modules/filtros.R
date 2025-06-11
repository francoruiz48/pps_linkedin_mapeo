filtros_server <- function(input, output, session, datos_reactivos) {
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

    # ✅ Se actualiza companyName según el sector
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

    # ✅ Se actualiza ciudad según el pais
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

    return(datos_filtrados)
}
