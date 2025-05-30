guardar <- function(diccionario, ruta) {
    # Guardar archivo CSV
    tryCatch(
        {
            # Guardar el archivo CSV editado
            write.csv(diccionario(), ruta, row.names = FALSE)
            showNotification("✅ Guardado exitoso", type = "message")
        },
        error = function(e) {
            showNotification(paste("❌ Error al guardar:", e$message), type = "error")
        }
    )
}

