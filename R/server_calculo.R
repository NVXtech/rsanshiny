rm_shiny_classes <- function(lista) {
  for (name in names(lista)) {
    is_shiny_class <- any(grepl("shiny", class(lista[[name]])))
    if (is_shiny_class) {
      lista[[name]] <- NULL
    }
  }
  return(lista)
}

salva_estado <- function(state, input, name) {
  if (is.null(state[["input"]])) {
    rlog::log_info("Sem parâmetros anteriores")
    state$input <- list()
  }
  params <- shiny::isolate(shiny::reactiveValuesToList(input))
  params <- rm_shiny_classes(params)
  for (param in names(params)) {
    if (grepl("-", param)) {
      parts <- strsplit(param, "-")[[1]]
      componente <- parts[1]
      name <- paste(parts[-1], collapse = "-")
      rlog::log_info(paste("Salvando parâmetro", name, "do componente", componente))
      state$input[[componente]][[name]] <- params[[param]]
    } else if (param %in% names(state$input)) {
      rlog::log_info(paste("Salvando parâmetro", param))
      state$input[[param]] <- params[[param]]
    } else {
      rlog::log_info(paste("Pulando parâmetro", param))
    }
  }
  rsan::save_state(state)
  return(state)
}


#' Modulo calculo
#'
#' @param input shiny input
#' @param app_state estado da aplicaçào
#' @param parent shiny parent input
#'
#' @return shiny server
#' @export
modulo_calculo <- function(id, app_state, parent) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$rodar, {
      shiny::withProgress(message = "Recalculando", value = 0, {
        n <- 4
        shiny::incProgress(0, detail = "Iniciando cálculo")
        shiny::incProgress(1 / n, detail = "Salvando novos parâmetros")
        app_state <- rsan::salva_parametros(app_state, input, id)
        shiny::incProgress(1 / n, detail = "Investimento")
        app_state <- rsan::rodar_modelo(app_state)
        shiny::incProgress(1 / n, detail = "Salvando resultados")
        rsan::save_state(app_state)
        shiny::incProgress(1 / n, detail = "Fim")
      })
    })

    shiny::observeEvent(input$salvar, {
      shiny::withProgress(message = "Salvando Parâmetros", value = 0, {
        app_state <- salva_estado(app_state, input, id)
      })
    })

    output$download <- shiny::downloadHandler(
      filename = function() {
        paste0(id, ".xlsx")
      },
      content = function(file) {
        if (id == "agua" | id == "esgoto") {
          writexl::write_xlsx(
            list(
              urbana = app_state[[id]],
              rural = app_state[[paste0(id, "_rural")]]
            ),
            file
          )
        } else {
          writexl::write_xlsx(app_state[[id]], file)
        }
      }
    )
  })
}
