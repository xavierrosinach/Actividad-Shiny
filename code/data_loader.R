# ------------------------------------------------------------------------------
# CARGA DE DATOS
# El siguiente codigo permite la carga y la validación de datos.
# Permite únicamente CSVs con separadores ';' con cabecera automática.
# ------------------------------------------------------------------------------

library(shiny)

# UI
dataLoaderUI <- function(id) {
  ns <- NS(id)
  
  # Selección del archivo CSV
  tagList(
    fileInput(
      inputId = ns("file"),
      label = "Selecciona tu archivo CSV",
      accept = c(".csv")
    ),
    
    uiOutput(ns("load_status")),
    
    # Añadimos un botón para usar los datos una vez cargados
    actionButton(
      inputId = ns("use_data"),
      label = "Usar estos datos",
      class = "btn-primary"
    )
  )
}

# SERVER
dataLoaderServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Datos reactivos
    rv <- reactiveValues(
      df_temp = NULL,      # Dataframe temporal al cargar el archivo
      df = NULL,           # Dataframe confirmado
      configured = FALSE,  # Booleano que nos marca si el dataframe se ha configurado
      last_error = NULL    # Último error si hay para mostrar en pantalla
    )
    
    # Lectura del CSV al seleccionar el archivo
    observeEvent(input$file, {
      
      req(input$file)   # A partir de un fichero de entrada
      
      # Validación de la extensión CSV
      ext <- tools::file_ext(input$file$name)
      if (tolower(ext) != "csv") {
        rv$last_error <- "Se precisa de un archivo CSV"
        return()    # Si hay error no seguimos
      }
      
      # Lectura del archivo (a partir de la configuración definida)
      df_read <- tryCatch(
        read.csv(
          file = input$file$datapath,      # Lectura a partir del path del archivo de input
          sep = ";",                       # Separador ';'
          header = TRUE,                   # Leemos el header
          fileEncoding = "UTF-8",          # Encoding predefinido UTF8
          stringsAsFactors = FALSE,        # Evita que las columnas de texto se conviertan en factores
          check.names = TRUE               # Convierte los nombres de las columnas en nombres válidos (quita espacios por ejemplo)
        ),
        error = function(e) NULL
      )
      
      # Otras comprovaciones
      if (is.null(df_read)) {
        rv$last_error <- "El CSV es nulo. Verifica su formato o contenido."     # Possibilidad que sea nulo
        return()
      }
      if (ncol(df_read) == 0) {
        rv$last_error <- "El CSV no contiene columnas."       # Que no contenga columnas
        return()
      }
      
      rv$df_temp <- df_read          # El dataframe temporal sera el dataframe leído
    }, ignoreInit = TRUE)
    
    # Añadimos un mensaje de estado para ver cómo esta la lectura
    output$load_status <- renderUI({
      
      # Si no hay archivo cargado
      if (is.null(input$file)) {
        return(tags$div(
          style = "color:#ff6b6b; font-weight:700;",
          "No hay ningún archivo cargado."
        ))
      }
      
      # Si hay algun error, y mostramos el último error
      if (!is.null(rv$last_error)) {
        return(tags$div(
          style = "color:#ff6b6b; font-weight:700;",
          paste("Error al leer el CSV:", rv$last_error)
        ))
      }
      
      # Si nos encontramos en proceso de configuración del dataframe
      if (!isTRUE(rv$configured)) {
        if (is.null(rv$df_temp)) {
          return(tags$div(
            style = "color:#F2C14E; font-weight:700;",
            "Procesando CSV."
          ))
        }
        
        # Mensaje según los datos estan cargados
        return(tags$div(
          style = "color:#F2C14E; font-weight:700;",
          paste0(
            "Archivo CSV ", input$file$name, " cargado."
          )
        ))
      }
      
      # Mansaje si los datos ya se pueden usar
      tags$div(
        style = "color:#7CFC98; font-weight:800;",
        "Datos cargados."
      )
    })
    
    # Cargamos los datos del dataframe temporal al dataframe final
    observeEvent(input$use_data, {
      rv$df <- rv$df_temp
      rv$configured <- TRUE
    })
    
    # Retorno: si el CSV si ha leido y configurado, el dataframe
    reactive({
      list(configured = isTRUE(rv$configured), df = rv$df)
    })
  })
}