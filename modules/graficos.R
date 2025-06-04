function_grafico_barras <- function(input, output, datos_filtrados) {
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
}
