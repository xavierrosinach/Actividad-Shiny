# ------------------------------------------------------------------------------
# VISUALIZACIÓN DE DATOS
# Usa el dataset ordenado o filtrado para crear distintos tipos de graficos
# Cuatro tipos: bar chart, line chart, dispersión, y pie chart
# Opción de personalizar las visualizaciones con nombres, color, o escalas
# ------------------------------------------------------------------------------

library(shiny)
library(ggplot2)

# Operador coalescence
`%||%` <- function(a, b) if (!is.null(a) && !identical(a, "")) a else b

# Clasificadores de columnas numericas o categoricas
is_num <- function(x) is.numeric(x) || is.integer(x)
is_cat <- function(x) is.character(x) || is.factor(x) || is.logical(x)

# Devuelve un vector de N colores
get_colors_n <- function(n, mode, palette_name, manual_colors) {
  n <- max(1, as.integer(n))
  
  if (mode == "manual") {
    raw <- manual_colors %||% ""
    cols <- trimws(unlist(strsplit(raw, ",")))
    cols <- cols[nzchar(cols)]
    cols <- cols[grepl("^#[0-9A-Fa-f]{6}$", cols)]
    if (length(cols) == 0) cols <- c("#4C8DFF", "#F2C14E", "#E85D5D", "#7CFC98")
    return(rep_len(cols, n))
  }
  
  pal <- palette_name %||% "Dark 3"
  grDevices::hcl.colors(n, pal)     # Paletas disponibles
}

# UI
chartsViewUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("charts_body"))          # El UI es mínimo, porque se construye dinamicamente en el server
  )
}

# SERVER
chartsViewServer <- function(id, df) {
  moduleServer(id, function(input, output, session) {
    
    # Función para comprovar que hay datos
    hasData <- reactive({
      d <- tryCatch(df(), error = function(e) NULL)
      !is.null(d) && is.data.frame(d) && ncol(d) > 0 && nrow(d) > 0
    })
    
    # UI de toda la interfaz general
    output$charts_body <- renderUI({
      if (!isTRUE(hasData())) {               # Si no hay datos
        return(div(style="color:#ff6b6b; font-weight:700;", "No hay datos cargados."))
      }
      
      # Selector de distintos gráficos
      tagList(
        div(
          class = "select-row",
          div(
            selectInput(
              session$ns("chart_type"),
              "Tipo de gráfico:",
              choices = c(
                "— Selecciona —" = "",
                "Gráfico de barras"  = "bar",
                "Gráfico de líneas" = "line",
                "Gráfico de dispersión" = "scatter",
                "Gráfico circular"  = "pie"
              ),
              selected = ""
            )
          )
        ),
        
        uiOutput(session$ns("selectors_ui")),
        hr(),
        
        # 3 desplegables de personalización de texto, colores, y escalas (se crean más tarde)
        uiOutput(session$ns("text_custom_ui")),
        uiOutput(session$ns("colors_custom_ui")),
        uiOutput(session$ns("scales_custom_ui")),
        
        hr(),
        uiOutput(session$ns("plot_or_msg_ui")),
        br(),
        uiOutput(session$ns("download_ui"))
      )
    })
    
    # Según el tipo de gráfico, debemos cambiar los selectores
    output$selectors_ui <- renderUI({
      req(hasData())
      d <- df()
      
      cols <- names(d)
      num_cols <- cols[sapply(d, is_num)]
      cat_cols <- cols[sapply(d, is_cat)]
      
      type <- input$chart_type
      req(type)
      
      ph <- "— Selecciona —"      # Selector para "nada"
      
      # Si hay un bar chart, solo permito seleccionar las variables categoricas en X, también añado si se quiere saber la suma y la media
      if (type == "bar") {
        tagList(
          h4("Variables (Bar)"),
          div(
            class = "select-row",
            div(selectInput(session$ns("bar_x"), "Categoría (X):", choices = c(ph = "", cat_cols), selected = "")),
            div(selectInput(session$ns("bar_stat"), "Medida:", choices = c(ph = "", "Conteo"="count","Suma"="sum","Media"="mean"), selected = "")),
            div(selectInput(session$ns("bar_y"), "Valor (Y) (solo si Suma/Media):", choices = c(ph = "", num_cols), selected = ""))
          )
        )
        
      # Si es un line chart, permito todas las variables
      } else if (type == "line") {
        tagList(
          h4("Variables (Line)"),
          div(
            class = "select-row",
            div(selectInput(session$ns("line_x"), "X:", choices = c(ph = "", cols), selected = "")),
            div(selectInput(session$ns("line_y"), "Y (numérica):", choices = c(ph = "", num_cols), selected = ""))
          )
        )
        
      # Igual que con scatter, permito todas las variables
      } else if (type == "scatter") {
        tagList(
          h4("Variables (Dispersión)"),
          div(
            class = "select-row",
            div(selectInput(session$ns("sc_x"), "X (numérica):", choices = c(ph = "", num_cols), selected = "")),
            div(selectInput(session$ns("sc_y"), "Y (numérica):", choices = c(ph = "", num_cols), selected = ""))
          )
        )
        
      # PAra el pie chart, permito seleccionar el tipo de medida: si media o suma
      } else if (type == "pie") {
        tagList(
          h4("Variables (Pie)"),
          div(
            class = "select-row",
            div(selectInput(session$ns("pie_cat"), "Categoría:", choices = c(ph = "", cat_cols), selected = "")),
            div(selectInput(session$ns("pie_stat"), "Medida:", choices = c(ph = "", "Conteo"="count","Suma"="sum"), selected = "")),
            div(selectInput(session$ns("pie_val"), "Valor (solo si Suma):", choices = c(ph = "", num_cols), selected = ""))
          )
        )
      } else NULL
    })
    
    # Helpers para saber que ejes son numericos según la selección
    axis_flags <- reactive({
      req(hasData())
      d <- df()
      type <- input$chart_type
      req(type)
      
      x_is_num <- FALSE
      y_is_num <- FALSE
      
      if (type == "bar") {
        x_is_num <- FALSE
        y_is_num <- TRUE
      } else if (type == "line") {
        xcol <- input$line_x %||% ""
        ycol <- input$line_y %||% ""
        x_is_num <- nzchar(xcol) && is_num(d[[xcol]])
        y_is_num <- nzchar(ycol) && is_num(d[[ycol]])
      } else if (type == "scatter") {
        x_is_num <- TRUE
        y_is_num <- TRUE
      } else if (type == "pie") {
        x_is_num <- FALSE
        y_is_num <- FALSE
      }
      
      list(x_is_num = x_is_num, y_is_num = y_is_num)
    })
    
    # Personalización de texto: título, eje X y eje Y
    output$text_custom_ui <- renderUI({
      req(hasData())
      type <- input$chart_type
      req(type)
      
      body <- if (type == "bar") {
        div(
          class="select-row",
          div(textInput(session$ns("bar_title"), "Título:", value="")),
          div(textInput(session$ns("bar_xlab"),  "Título eje X:", value="")),
          div(textInput(session$ns("bar_ylab"),  "Título eje Y:", value=""))
        )
      } else if (type == "line") {
        div(
          class="select-row",
          div(textInput(session$ns("line_title"), "Título:", value="")),
          div(textInput(session$ns("line_xlab"),  "Título eje X:", value="")),
          div(textInput(session$ns("line_ylab"),  "Título eje Y:", value=""))
        )
      } else if (type == "scatter") {
        div(
          class="select-row",
          div(textInput(session$ns("sc_title"), "Título:", value="")),
          div(textInput(session$ns("sc_xlab"),  "Título eje X:", value="")),
          div(textInput(session$ns("sc_ylab"),  "Título eje Y:", value=""))
        )
      } else {
        div(
          class="select-row",
          div(textInput(session$ns("pie_title"), "Título:", value=""))
        )
      }
      
      tags$details(
        class = "dropdown",
        tags$summary("Personalización texto"),
        div(class="dropdown-body", body)
      )
    })
    
    # Personalización de colores (mostramos distitas opciones de paletas, o de forma manual con entrada de #HEX)
    output$colors_custom_ui <- renderUI({
      req(hasData())
      type <- input$chart_type
      req(type)
      
      # Leyenda: solo se necesita para los pie charts (en bars no se necesita, ya hay la información en el eje)
      legend_ui <- NULL
      if (type == "pie") {
        legend_ui <- div(
          class = "select-row",
          div(
            selectInput(
              session$ns("legend_pos"),
              "Leyenda:",
              choices = c("Derecha"="right","Izquierda"="left","Arriba"="top","Abajo"="bottom","Ocultar"="none"),   # Básicamente para la posición
              selected = "right"
            )
          )
        )
      }
      
      # Primero creamos un dropdown para saber si se quiere seleccionar una paleta, o de forma manual
      tags$details(
        class = "dropdown",
        tags$summary("Personalización colores"),
        div(
          class="dropdown-body",
          div(
            class="select-row",
            div(
              selectInput(
                session$ns("color_mode"),
                "Modo de color:",
                choices = c("Paleta"="palette", "Manual (hex)"="manual"),    # Selección inicial de una paleta
                selected = "palette"
              )
            )
          ),
          
          uiOutput(session$ns("colors_mode_ui")),
          
          if (!is.null(legend_ui)) tagList(hr(), legend_ui) else NULL
        )
      )
    })
    
    # Dependiendo si se ha seleccionado paleta o manual
    output$colors_mode_ui <- renderUI({
      req(hasData())
      mode <- input$color_mode %||% "palette"
      
      # Si se ha seleccionado las paletas, mostramos las disponibles (disponibles para categorias)
      if (mode == "palette") {
        div(
          class="select-row",
          div(
            selectInput(
              session$ns("palette_name"),
              "Paleta:",
              choices = grDevices::hcl.pals(type="qualitative"),
              selected = "Dark 3"          # Usamos Dark 3 como predeterminadas
            )
          )
        )
      } else {   # Sino, se deben escrivir los valores en formato #HEX separados por coma, añadimos cuatro para mostrar el ejemplo
        div(
          class="select-row",
          div(
            textInput(
              session$ns("manual_colors"),
              "Colores manuales (hex separados por coma):",
              value = "#4C8DFF, #F2C14E, #E85D5D, #7CFC98"
            )
          )
        )
      }
    })
    
    # Personalización de escalas (mínimo y máximo de los ejes) solo para ejes numericos
    output$scales_custom_ui <- renderUI({
      req(hasData())
      type <- input$chart_type
      req(type)
      
      # Obtenemos los flags de los ejes
      flags <- axis_flags()
      
      # En el pie chart no hay escalas
      if (type == "pie") return(NULL)
      
      # Construimos inputs solo para ejes numéricos
      x_scale_ui <- NULL
      y_scale_ui <- NULL
      
      # UI para eje X
      if (isTRUE(flags$x_is_num)) {
        x_scale_ui <- tagList(
          h4("Escala X"),
          div(
            class="select-row",
            div(numericInput(session$ns("x_min"), "X min:", value = NA)),
            div(numericInput(session$ns("x_max"), "X max:", value = NA))
          )
        )
      }
      
      # UI para eje Y
      if (isTRUE(flags$y_is_num)) {
        y_scale_ui <- tagList(
          h4("Escala Y"),
          div(
            class="select-row",
            div(numericInput(session$ns("y_min"), "Y min:", value = NA)),
            div(numericInput(session$ns("y_max"), "Y max:", value = NA))
          )
        )
      }
      
      # Si no hay nada que mostrar, no pintamos el desplegable (por ejemplo en bar chart para el eje X no se necesita mostrar)
      if (is.null(x_scale_ui) && is.null(y_scale_ui)) return(NULL)
      
      # Mostramos el dropdown
      tags$details(
        class="dropdown",
        tags$summary("Personalización escalas"),
        div(class="dropdown-body", x_scale_ui, if (!is.null(x_scale_ui) && !is.null(y_scale_ui)) hr() else NULL, y_scale_ui)
      )
    })
    
    # Aplicamos las escalas de mínimo y máximo por eje
    apply_scales <- function(p, x_is_num, y_is_num) {
      xmin <- suppressWarnings(as.numeric(input$x_min))
      xmax <- suppressWarnings(as.numeric(input$x_max))
      ymin <- suppressWarnings(as.numeric(input$y_min))
      ymax <- suppressWarnings(as.numeric(input$y_max))
      
      xlim <- NULL
      ylim <- NULL
      if (isTRUE(x_is_num) && is.finite(xmin) && is.finite(xmax)) xlim <- c(min(xmin, xmax), max(xmin, xmax))
      if (isTRUE(y_is_num) && is.finite(ymin) && is.finite(ymax)) ylim <- c(min(ymin, ymax), max(ymin, ymax))
      
      if (!is.null(xlim) || !is.null(ylim)) {
        p <- p + coord_cartesian(xlim = xlim, ylim = ylim)
      }
      p
    }
    
    # Validación de la selección mínima
    ready_to_plot <- reactive({
      req(hasData())
      type <- input$chart_type
      req(type)
      
      if (type == "bar") {
        ok <- nzchar(input$bar_x %||% "") && nzchar(input$bar_stat %||% "")
        if ((input$bar_stat %||% "") %in% c("sum", "mean")) ok <- ok && nzchar(input$bar_y %||% "")
        return(ok)
      }
      if (type == "line") return(nzchar(input$line_x %||% "") && nzchar(input$line_y %||% ""))
      if (type == "scatter") return(nzchar(input$sc_x %||% "") && nzchar(input$sc_y %||% ""))
      if (type == "pie") {
        ok <- nzchar(input$pie_cat %||% "") && nzchar(input$pie_stat %||% "")
        if ((input$pie_stat %||% "") == "sum") ok <- ok && nzchar(input$pie_val %||% "")
        return(ok)
      }
      FALSE
    })
    
    # Creación del gráfico
    plot_obj <- reactive({
      req(hasData())
      validate(need(isTRUE(ready_to_plot()), "Selecciona la información para crear un gráfico."))
      
      d <- df()
      type <- input$chart_type
      
      mode <- input$color_mode %||% "palette"
      pal_name <- input$palette_name %||% "Dark 3"
      manual_cols <- input$manual_colors %||% ""
      legend_pos <- input$legend_pos %||% "right"
      
      base_theme <- theme_minimal(base_size = 13) +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"))
      
      # Bar chart
      if (type == "bar") {
        xcol <- input$bar_x
        stat <- input$bar_stat
        ycol <- input$bar_y %||% ""
        
        ttl <- input$bar_title %||% "Gráfico de barras"
        xlab <- input$bar_xlab %||% xcol
        ylab <- input$bar_ylab %||% ycol
        
        dd <- d
        dd[[xcol]] <- as.factor(dd[[xcol]])
        
        if (stat == "count") {
          n <- nlevels(dd[[xcol]])
          cols <- get_colors_n(n, mode, pal_name, manual_cols)
          
          p <- ggplot(dd, aes(x = .data[[xcol]], fill = .data[[xcol]])) +
            geom_bar(show.legend = FALSE) +
            labs(title = ttl, x = xlab, y = ylab) +
            base_theme +
            scale_fill_manual(values = cols)
          
          # En bar chart solo creamos escala en eje Y
          p <- apply_scales(p, x_is_num = FALSE, y_is_num = TRUE)
          return(p)
        }
        
        validate(need(is_num(dd[[ycol]]), "La columna Valor (Y) debe ser numérica."))
        fun <- if (stat == "sum") sum else mean
        
        agg <- aggregate(dd[[ycol]], by = list(X = dd[[xcol]]), FUN = fun, na.rm = TRUE)
        names(agg) <- c("X", "Y")
        
        n <- nlevels(as.factor(agg$X))
        cols <- get_colors_n(n, mode, pal_name, manual_cols)
        
        p <- ggplot(agg, aes(x = X, y = Y, fill = X)) +    # Usamos ggplot
          geom_col(show.legend = FALSE) +
          labs(title = ttl, x = xlab, y = ylab) +
          base_theme +
          scale_fill_manual(values = cols)
        
        p <- apply_scales(p, x_is_num = FALSE, y_is_num = TRUE)
        return(p)
      }
      
      
      # Line chart
      if (type == "line") {
        xcol <- input$line_x
        ycol <- input$line_y
        validate(need(is_num(d[[ycol]]), "Para el gráfico de líneas, Y debe ser numérica."))
        
        ttl <- input$line_title %||% "Gráfico de líneas"
        xlab <- input$line_xlab %||% xcol
        ylab <- input$line_ylab %||% ycol
        
        one_col <- get_colors_n(1, mode, pal_name, manual_cols)[1]
        
        p <- ggplot(d, aes(x = .data[[xcol]], y = .data[[ycol]])) +
          geom_line(linewidth = 1, color = one_col) +
          labs(title = ttl, x = xlab, y = ylab) +
          base_theme
        
        p <- apply_scales(p, x_is_num = is_num(d[[xcol]]), y_is_num = TRUE)
        return(p)
      }
      
      # Gráfico de dispersión
      if (type == "scatter") {
        xcol <- input$sc_x
        ycol <- input$sc_y
        validate(need(is_num(d[[xcol]]) && is_num(d[[ycol]]), "Dispersión requiere X e Y numéricas."))
        
        ttl <- input$sc_title %||% "Gráfico de dispersión"
        xlab <- input$sc_xlab %||% xcol
        ylab <- input$sc_ylab %||% ycol
        
        one_col <- get_colors_n(1, mode, pal_name, manual_cols)[1]
        
        p <- ggplot(d, aes(x = .data[[xcol]], y = .data[[ycol]])) +
          geom_point(alpha = 0.85, color = one_col) +
          labs(title = ttl, x = xlab, y = ylab) +
          base_theme
        
        p <- apply_scales(p, x_is_num = TRUE, y_is_num = TRUE)
        return(p)
      }
      
      # Gráfico circular
      if (type == "pie") {
        cat_col <- input$pie_cat
        stat <- input$pie_stat
        val_col <- input$pie_val %||% ""
        
        ttl <- input$pie_title %||% "Gráfico circular"
        
        dd <- d
        dd[[cat_col]] <- as.factor(dd[[cat_col]])
        
        if (stat == "count") {
          agg <- as.data.frame(table(dd[[cat_col]]))
          names(agg) <- c("cat", "value")
        } else {
          validate(need(is_num(dd[[val_col]]), "La columna Valor debe ser numérica."))
          agg <- aggregate(dd[[val_col]], by = list(cat = dd[[cat_col]]), FUN = sum, na.rm = TRUE)
          names(agg) <- c("cat", "value")
        }
        
        n <- nlevels(as.factor(agg$cat))
        cols <- get_colors_n(n, mode, pal_name, manual_cols)
        
        p <- ggplot(agg, aes(x = "", y = value, fill = cat)) +
          geom_col(width = 1) +
          coord_polar(theta = "y") +
          labs(title = ttl, fill = cat_col) +
          base_theme +
          theme(
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.position = if (legend_pos == "none") "none" else legend_pos
          ) +
          scale_fill_manual(values = cols)
        
        return(p)
      }
      
      ggplot() + base_theme + labs(title = "Configuración no válida")
    })
    
    
    output$plot_or_msg_ui <- renderUI({
      req(hasData())
      if (!isTRUE(ready_to_plot())) {
        return(div(style="color:#ff6b6b; font-weight:700;", "Selecciona la información para crear el gráfico."))
      }
      plotOutput(session$ns("plot"), height = "460px")
    })
    
    output$plot <- renderPlot({
      req(hasData())
      req(isTRUE(ready_to_plot()))
      plot_obj()
    })
    
    # Botón para guardar el gráfic (UI)
    output$download_ui <- renderUI({
      req(hasData())
      if (!isTRUE(ready_to_plot())) return(NULL)
      downloadButton(session$ns("download_plot"), "Guardar gráfico (PNG)", class = "btn-primary")
    })
    
    # Botón para descargar el grafico (SERVER)
    output$download_plot <- downloadHandler(
      filename = function() {"grafico.png"},
      content = function(file) {
        ggsave(filename = file, plot = plot_obj(), width = 10, height = 6, dpi = 160)
      }
    )
    
    invisible(NULL)
  })
}
