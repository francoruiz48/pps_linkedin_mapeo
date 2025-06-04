tabla_resumen <- function(input, output, datos_filtrados, datos) {
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
}
