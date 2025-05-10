function_grafico_barras <- function(output, datos_filtrados) {
  output$grafico <- renderPlotly({
    datos <- datos_filtrados()

    if (input$grafico == "categoria") {
      df <- datos %>% count(categoria, name = "Cantidad")
      p <- ggplot(df, aes(x = reorder(categoria, Cantidad), y = Cantidad, text = paste("Categoría:", categoria, "<br>Cantidad:", Cantidad))) +
        geom_bar(stat = "identity", fill = "#7b2cbf") +
        geom_text(aes(label = Cantidad), hjust = -0.2, size = 4, color = "black") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Distribución por Categoría", x = "Cantidad", y = "") +
        theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- datos %>%
        separate_rows(tech_tags, sep = ",\\s*") %>%
        count(tech_tags, name = "Cantidad")
      p <- ggplot(df, aes(x = reorder(tech_tags, Cantidad), y = Cantidad, text = paste("Tecnología:", tech_tags, "<br>Cantidad:", Cantidad))) +
        geom_bar(stat = "identity", fill = "#3f37c9") +
        geom_text(aes(label = Cantidad), hjust = -0.2, size = 4, color = "black") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Distribución por Tecnología", x = "Cantidad", y = "Tecnología") +
        theme(plot.title = element_text(hjust = 0.5))
    }

    ggplotly(p, tooltip = "text") %>%
      layout(margin = list(r = 60))  
  })
}