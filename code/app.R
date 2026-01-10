# ------------------------------------------------------------------------------
# APLICACIÓN PRINCIPAL
# Juntamos las distintas partes en un simple código
# ------------------------------------------------------------------------------

library(shiny)

# Carga de los ditintos códigos
source("style.R", local = TRUE)
source("data_loader.R", local = TRUE)
source("data_view.R", local = TRUE)
source("charts_view.R", local = TRUE)
source("instructions.R", local = TRUE)

# UI
ui <- fluidPage(
  tags$head(
    appTheme(),
    
    # Definimos un estilo HTML própio
    tags$style(HTML("
      .top-actions {  
        display: flex;
        justify-content: center;
        gap: 12px;
        margin: 10px 0 18px 0;
        flex-wrap: wrap;
      }
      .btn-small {
        padding: 7px 12px !important;
        font-size: 13px !important;
        font-weight: 800 !important;
      }
      
      .fixed-panel {
        background: rgba(255,255,255,0.04);
        border: 1px solid rgba(255,255,255,0.10);
        border-radius: 14px;
        padding: 14px 14px;
        text-align: left;
        margin-top: 14px;
      }
      .fixed-panel h3 {
        margin: 2px 0 10px 0;
        text-align: left;
      }

      #charts_fixed > details.dropdown > summary { display: none !important; }
      #charts_fixed details.dropdown {
        margin: 0 !important;
        padding: 0 !important;
        background: transparent !important;
        border: none !important;
      }
      #charts_fixed .dropdown-body { padding: 0 !important; }

      .modal-content, .modal-content * { color: #000000 !important; }
    "))
  ),
  
  div(
    class = "app-container",
    
    uiOutput("main_ui"),
    
    # FOOTER (usa el estilo ya definido en style.R)
    div(
      class = "footer",
      HTML(paste0(
        "Xavier Rosinach Capell | ",
        "<a href='https://github.com/xavierrosinach' target='_blank' rel='noopener'>GitHub</a> | ",
        "<a href='https://www.linkedin.com/in/xavierrosinach/' target='_blank' rel='noopener'>LinkedIn</a> | ",
        "Universidad Europea - Escuela Real Madrid"
      ))
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  # Valores reactivos
  rv <- reactiveValues(
    configured = FALSE,   # Booleano para marcar si el dataframe se ha configurado
    df = NULL             # Dataframe: nulo si aún no se ha cargado
  )
  
  # Cargamos el cargador de datos
  loader <- dataLoaderServer("loader")
  
  # Nos fijamos si el dataframe ha sido configurado y lo aplicamos
  observeEvent(loader()$configured, {
    if (isTRUE(loader()$configured)) {
      rv$configured <- TRUE
      rv$df <- loader()$df
    }
  }, ignoreInit = TRUE)
  
  # DF original
  df_reactive <- reactive({
    req(rv$configured, rv$df)
    rv$df
  })
  
  # Botón para mostrar las instruciones de uso
  observeEvent(input$show_instructions, {
    showModal(modalDialog(
      title = "Instrucciones de uso",
      instructionsContent(),
      easyClose = TRUE,
      footer = modalButton("Cerrar"),
      size = "l"
    ))
  })
  
  # Botón para mostrar los datos
  observeEvent(input$show_data, {
    if (!isTRUE(rv$configured)) return()
    showModal(modalDialog(
      title = "Datos",
      div(style="padding-top:6px;", dataViewUI("view")),
      easyClose = TRUE,
      footer = modalButton("Cerrar"),
      size = "l"
    ))
  })
  
  # UI de la página
  output$main_ui <- renderUI({
    
    # Título
    header <- div(
      class = "header-box",
      h1("Título"),
      p("Subtítulo")
    )
    
    # Botónes de datos e instrucciones
    buttons <- div(
      class = "top-actions",
      if (isTRUE(rv$configured)) actionButton("show_data", "Datos", class = "btn-warning btn-small"),
      actionButton("show_instructions", "Instrucciones", class = "btn-primary btn-small")
    )
    
    # En caso que no este configurado
    if (!isTRUE(rv$configured)) {
      return(tagList(
        header,
        buttons,
        div(class = "fixed-panel", dataLoaderUI("loader"))
      ))
    }
    
    # Título de gráficos
    tagList(
      header,
      buttons,
      div(
        class = "fixed-panel",
        h3("Gráficos"),
        div(id = "charts_fixed", chartsViewUI("charts"))
      )
    )
  })
  
  # Devuelve el estado con el dataframe con cambios
  view_state <- dataViewServer(
    "view",
    df = df_reactive,
    page_size = 20,
    on_reset = function() session$reload()
  )
  
  # Dataframe para gráficos, sea el original o el que tiene cambios
  df_for_charts <- reactive({
    req(rv$configured)
    st <- tryCatch(view_state(), error = function(e) NULL)
    if (!is.null(st) && isTRUE(st$applied) && !is.null(st$df_applied)) {
      return(st$df_applied)
    }
    df_reactive()
  })
  
  # Charts usa df_for_charts para cambiar
  chartsViewServer(
    "charts",
    df = df_for_charts
  )
}

shinyApp(ui, server)

