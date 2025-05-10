# 📊 Dashboard de Análisis de Ofertas Laborales

Este proyecto es una aplicación web desarrollada en **R** usando el paquete **Shiny**. Su objetivo es facilitar el análisis y visualización de una base de datos de ofertas laborales, permitiendo a los usuarios interactuar con los datos a través de filtros, tablas, gráficos y métricas clave.

## 🚀 Características

- Filtros dinámicos por:
  - Compañía
  - Sector
  - Categoría
  - Tecnología
- Indicadores clave:
  - Total de ofertas
  - Sectores representados
  - Promedio de tecnologías por oferta
  - Tecnologías únicas
  - Top 3 tecnologías
  - Top 3 categorías
  - Empresa con más publicaciones
  - Sector con más oportunidades
- Visualización de datos:
  - Tablas exportables a Excel
  - Gráficos por categoría o tecnología

## 🧱 Estructura del Proyecto

El proyecto está organizado en los siguientes componentes:

- `app.R`: archivo principal que ejecuta la aplicación integrando la UI y el servidor.
- `global.R`: contiene las librerías necesarias y variables globales.
- `ui.R`: define la interfaz de usuario.
- `server.R`: contiene la lógica reactiva del servidor.
- `modules/`: carpeta con módulos reutilizables para mantener una arquitectura modular y escalable.

## 📦 Requisitos

- R (>= 4.0)
- RStudio

## ▶️ Ejecución

1. Clona este repositorio:
   ```bash
   git clone https://github.com/tu-usuario/tu-repo.git
