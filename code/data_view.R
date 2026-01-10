# ------------------------------------------------------------------------------
# TABLA DE DATOS
# Visualización de los datos del CSV en formato de tabla
# Possibilidad de filtrar, ordenar i exportar como CSV
# Si se ordena o se filtra, possibilidad de usar el dataframe para los futuros graficos
# Possibilidad de reiniciar el código para cambiar los datos
# ------------------------------------------------------------------------------

library(shiny)

# UI
dataViewUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # Tabla
    tableOutput(ns("table")),
    
    # Botones de páginas para ir pasando las páginas ("\u25C0" y "\u25B6") son las flechas para los lados
    div(
      class = "pager",
      actionButton(ns("prev_page"), "\u25C0", class = "btn-primary"),
      span(class = "pager-info", textOutput(ns("page_info"), inline = TRUE)),
      actionButton(ns("next_page"), "\u25B6", class = "btn-primary")
    ),
    
    # Barra de acciones
    div(
      class = "action-bar",
      
      # Opción de ordenar los datos
      tags$details(
        class = "dropdown",
        tags$summary("Ordenar"),
        div(class = "dropdown-body", uiOutput(ns("sort_ui")))
      ),
      
      # Opción de filtrar los datos
      tags$details(
        class = "dropdown",
        tags$summary("Filtrar"),
        div(class = "dropdown-body", uiOutput(ns("filters_ui")))
      ),
      
      # Botón para usar los datos
      div(
        style = "padding: 10px 12px;",
        uiOutput(ns("use_data_btn_ui"))
      ),
      
      # Botón para exportar los datos en formato CSV
      div(
        style = "padding: 10px 12px;",
        downloadButton(ns("download_csv"), "Exportar CSV", class = "btn-primary")
      ),
      
      # Botón para cambiar el dataset
      div(
        style = "padding: 10px 12px;",
        actionButton(ns("reset_app"), "Cambiar dataset", class = "btn-warning")
      )
    ),
  )
}

# Server
dataViewServer <- function(id, df, order_col = NULL, page_size = 20, on_reset = NULL) {      # Page size es las rows que queremos mostrar al dataset
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Valores reactivos
    rv <- reactiveValues(
      page = 1,            # Página de datos
      baseline = NULL,     # Baseline de filtros
      applied = FALSE,     # Si los datos se quieren usar (cambios aplicados)
      df_applied = NULL    # Dataframe con cambios confirmados
    )
    
    # Pequeñas funciones para saber si es numerico o string
    is_num  <- function(x) is.numeric(x) || is.integer(x)
    is_cat <- function(x) is.logical(x)
    
    # Baseline cuando cambia el df (para detectar cambios reales)
    observeEvent(df(), {
      d <- df()
      req(d)
      
      # Lista con los cambios que aplicamos
      base <- list()
      for (col in names(d)) {
        x <- d[[col]]
        if (is_num(x)) {
          rng <- suppressWarnings(range(x, na.rm = TRUE))
          if (length(rng) != 2 || !is.finite(rng[1]) || !is.finite(rng[2])) rng <- c(0, 0)
          base[[paste0("fmin_", col)]] <- rng[1]
          base[[paste0("fmax_", col)]] <- rng[2]
        } else {
          base[[paste0("f_", col)]] <- ""
        }
      }
      
      rv$baseline <- base
      
      # Reseteamos el estado al aplicar los cambios
      rv$applied <- FALSE
      rv$df_applied <- NULL
      
      rv$page <- 1    # Volvemos a la primera pagina
    }, ignoreInit = FALSE)
    
    # Reset de página cuando se cambian datos
    observeEvent(list(df(), if (is.function(order_col)) order_col() else NULL), {
      rv$page <- 1
    }, ignoreInit = TRUE)
    
    # UI de Ordenar (lo tenemos que crear aquí dentro)
    output$sort_ui <- renderUI({
      d <- df()
      req(d)
      
      # Inicialmente, si no hay selección de columna, no se ordena
      default_col <- ""
      if (is.function(order_col)) {
        tmp <- order_col()
        if (!is.null(tmp) && nzchar(tmp) && tmp %in% names(d)) default_col <- tmp
      }
      
      # Creamos la selección (ponemos un valor inicial)
      tagList(
        selectInput(
          ns("sort_col"),
          "Ordenar por:",
          choices = c("— Sin ordenar —" = "", names(d)),
          selected = default_col
        ),
        selectInput(
          ns("sort_dir"),
          "Dirección:",
          choices = c("Menor a mayor" = "asc", "Mayor a menor" = "desc"),       # Según si es ascendente o descendiente
          selected = "asc"
        )
      )
    })
    
    # UI de filtros
    output$filters_ui <- renderUI({
      d <- df()
      req(d)
      
      controls <- lapply(names(d), function(col) {
        x <- d[[col]]
        
        # Si es numerico, del menor al mayor
        if (is_num(x)) {
          rng <- suppressWarnings(range(x, na.rm = TRUE))
          if (length(rng) != 2 || !is.finite(rng[1]) || !is.finite(rng[2])) rng <- c(0, 0)
          
          # Devolvemos un input de números
          return(
            div(
              tags$label(col),
              div(
                class = "range-row",
                numericInput(ns(paste0("fmin_", col)), "Mín", value = rng[1]),
                numericInput(ns(paste0("fmax_", col)), "Máx", value = rng[2])
              )
            )
          )
        }
        
        # Si es logico, añadimos un filtro de nombre (contiene)
        if (is_cat(x)) {
          return(
            selectInput(
              ns(paste0("f_", col)),
              col,
              choices = c("Todos" = "", "TRUE" = "TRUE", "FALSE" = "FALSE"),
              selected = ""
            )
          )
        }
        
        # Filtro de texto (contiene)
        textInput(
          ns(paste0("f_", col)),
          col,
          value = "",
          placeholder = "contiene..."
        )
      })
      
      div(class = "filter-grid", tagList(controls))
    })
    
    # Detección de cambios
    changed <- reactive({
      d <- df()
      req(d)
      base <- rv$baseline      # Baseline de filtros
      req(!is.null(base))
      
      # En el caso que hay algun orden, lo aplicamos
      sort_changed <- !is.null(input$sort_col) && nzchar(input$sort_col)
      
      # Booleano para activar el botón
      filter_changed <- FALSE
      
      # Para cada columna vamos a ver si ha cambiado los filtros
      for (col in names(d)) {
        x <- d[[col]]
        
        # Si es numerica, añadimos los valores al filtro
        if (is_num(x)) {
          keymin <- paste0("fmin_", col)
          keymax <- paste0("fmax_", col)
          
          vmin <- input[[keymin]]
          vmax <- input[[keymax]]
          bmin <- base[[keymin]]
          bmax <- base[[keymax]]
          
          # Si cambian los valores, tenemos en cuenta el cambio
          if (!is.null(vmin) && !is.null(vmax) && !is.null(bmin) && !is.null(bmax)) {
            if (!isTRUE(all.equal(as.numeric(vmin), as.numeric(bmin))) ||
                !isTRUE(all.equal(as.numeric(vmax), as.numeric(bmax)))) {
              filter_changed <- TRUE
              break
            }
          }
          next
        }
        
        # Lo mismo con el valor no numerico
        key <- paste0("f_", col)
        val <- input[[key]] %||% ""
        b   <- base[[key]] %||% ""
        if (nzchar(val) && !identical(val, b)) {
          filter_changed <- TRUE
          break
        }
      }
      
      isTRUE(sort_changed || filter_changed)
    })
    
    # Dataframe filtrado
    filtered_df <- reactive({
      d <- df()
      req(d)
      
      # Dataframe a devolver
      out <- d
      
      # Checkeamos cada columna
      for (col in names(out)) {
        x <- out[[col]]
        
        # Si es numerica
        if (is_num(x)) {
          vmin <- input[[paste0("fmin_", col)]]
          vmax <- input[[paste0("fmax_", col)]]
          if (!is.null(vmin) && !is.null(vmax) && is.finite(vmin) && is.finite(vmax)) {
            lo <- min(vmin, vmax)
            hi <- max(vmin, vmax)
            out <- out[!is.na(out[[col]]) & out[[col]] >= lo & out[[col]] <= hi, , drop = FALSE]
          }
          next
        }
        
        # Si no es numerica
        if (is_cat(x)) {
          val <- input[[paste0("f_", col)]]
          if (!is.null(val) && nzchar(val)) {
            target <- (val == "TRUE")
            out <- out[!is.na(out[[col]]) & out[[col]] == target, , drop = FALSE]
          }
          next
        }
        
        val <- input[[paste0("f_", col)]]
        if (!is.null(val) && nzchar(val)) {
          patt <- tolower(val)
          vv <- tolower(as.character(out[[col]]))
          out <- out[!is.na(vv) & grepl(patt, vv, fixed = TRUE), , drop = FALSE]
        }
      }
      
      out
    })
    
    # Ordenación si se ha filtrado
    sorted_df <- reactive({
      d <- filtered_df()
      req(d)
      
      col <- input$sort_col
      dir <- input$sort_dir
      
      if (is.null(col) || !nzchar(col) || !(col %in% names(d))) return(d)
      
      v <- d[[col]]
      ord <- order(v, na.last = TRUE)
      if (!is.null(dir) && dir == "desc") ord <- rev(ord)
      
      d[ord, , drop = FALSE]
    })
    
    # Botón de usar los datos (solo se puede usar si hay cambios en el dataset)
    output$use_data_btn_ui <- renderUI({
      req(df())
      if (isTRUE(changed())) {
        actionButton(ns("use_data"), "Usar datos", class = "btn-primary")
      } else {
        tags$button(
          type = "button",
          class = "btn btn-primary disabled",
          disabled = "disabled",
          "Usar datos"
        )
      }
    })
    
    # Aplicamos los cambios
    observeEvent(input$use_data, {
      req(isTRUE(changed()))
      rv$df_applied <- sorted_df()
      rv$applied <- TRUE
    }, ignoreInit = TRUE)
    
    # Paginas de la visualización
    n_pages <- reactive({
      d <- sorted_df()
      req(d)
      max(1, ceiling(nrow(d) / page_size))
    })
    
    observeEvent(list(sorted_df(), n_pages()), {
      rv$page <- max(1, min(rv$page, n_pages()))
    }, ignoreInit = TRUE)
    
    # Cambio de página
    paged_df <- reactive({
      d <- sorted_df()
      req(d)
      
      total <- nrow(d)
      if (total == 0) return(d)
      
      p <- max(1, min(rv$page, n_pages()))
      start <- (p - 1) * page_size + 1
      end   <- min(p * page_size, total)
      
      d[start:end, , drop = FALSE]
    })
    
    # Página previa
    observeEvent(input$prev_page, {
      rv$page <- max(1, rv$page - 1)
    })
    
    # Siguiente página
    observeEvent(input$next_page, {
      rv$page <- min(n_pages(), rv$page + 1)
    })
    
    # Información que tiene la página (para saber el número)
    output$page_info <- renderText({
      d <- sorted_df()
      req(d)
      sprintf("Página %d / %d  |  Filas: %d", rv$page, n_pages(), nrow(d))
    })
    
    # Tabla filtrada
    output$table <- renderTable({
      d <- paged_df()
      req(d)
      d
    }, striped = TRUE, hover = TRUE, spacing = "s")
    
    # Exportar el CSV
    output$download_csv <- downloadHandler(
      filename = function() {"datos_exportados.csv"},
      content = function(file) {
        d <- sorted_df()
        write.table(d, file = file, sep = ";", row.names = FALSE, col.names = TRUE, fileEncoding = "UTF-8")     # Ponemos el envoding tal y como queremos de input para en un futuro
      }
    )
    
    # Cambio en el dataset
    observeEvent(input$reset_app, {
      showModal(
        modalDialog(      # Mostramos información por si acaso
          title = "Confirmar cambio de dataset",
          p("Vas a cargar un nuevo archivo de datos. Esto va a reiniciar la aplicación y se perderá la configuración actual."),
          footer = tagList(
            modalButton("Cancelar"),
            actionButton(ns("confirm_reset"), "Sí, cambiar dataset", class = "btn-danger")      # Botones para confirmar o cancelar el cambio
          )
        )
      )
    })
    
    # Confirmación del reset
    observeEvent(input$confirm_reset, {
      removeModal()
      ok <- FALSE
      if (is.function(on_reset)) ok <- tryCatch({ on_reset(); TRUE }, error = function(e) FALSE)
      if (!ok) session$reload()
    }, ignoreInit = TRUE)
    
    # Retorno
    reactive({
      list(
        df_view = sorted_df(),             # Dataset filtrado o ordenado
        changed = isTRUE(changed()),       # Cambios en el dataset
        applied = isTRUE(rv$applied),      # Si el usuario pulso el botón de cambios
        df_applied = rv$df_applied         # Dataframe para los cambios
      )
    })
  })
}

