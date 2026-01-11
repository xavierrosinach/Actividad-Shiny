# ------------------------------------------------------------------------------
# INSTRUCCIONES
# Contenido de texto con las instrucciones
# ------------------------------------------------------------------------------

library(shiny)

instructionsContent <- function() {
  tagList(
    
    tags$ol(
      tags$li("Carga un archivo CSV con separador ';', cabecera y codificación UTF-8, y confirma la carga con “Usar estos datos”."),
      tags$li("Accede a la vista de datos para explorar el dataset mediante paginación, ordenación y filtros por columna."),
      tags$li("Aplica los filtros u ordenaciones deseadas y pulsa “Usar datos” para emplear el dataset modificado en los gráficos."),
      tags$li("Selecciona el tipo de gráfico y las variables correspondientes, y personaliza títulos, colores y escalas si es necesario."),
      tags$li("Guarda el gráfico generado o reinicia la aplicación para cargar un nuevo conjunto de datos.")
    )
  )
}

