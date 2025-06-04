check_data <- function(re_procesar=FALSE) {
    if (file.exists(RUTA_RDS) && !re_procesar) {
        df <- readRDS(RUTA_RDS)
        
    } else {
        df <- readxl::read_excel(RUTA_EXCEL)
        df <- procesar_df(df)
        saveRDS(df, RUTA_RDS)
    }
    datos_reactivos <- reactiveVal(df)
    return(datos_reactivos)
}
