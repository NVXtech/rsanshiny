update_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$atualizar_sinapi, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SINAPI")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        # protect the update call so the server doesn't hang on errors
        result <- tryCatch(
          {
            rsan::update_sinapi(
              as.integer(input$sinapi_ano),
              as.integer(input$sinapi_mes)
            )
          },
          error = function(e) {
            # log and notify the user, return FALSE so downstream logic handles it
            rlog::log_error(sprintf("Error updating SINAPI: %s", e$message))
            shiny::showNotification(sprintf("SINAPI - Erro: %s", e$message), type = "error")
            return(FALSE)
          }
        )
        if (!result) {
          shiny::showNotification("SINAPI - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_censo, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Censo")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        # protect the update call so the server doesn't hang on errors
        result <- tryCatch(
          {
            rsan::update_ibge_censo(input$censo_ano)
          },
          error = function(e) {
            # log and notify the user, return FALSE so downstream logic handles it
            rlog::log_error(sprintf("Error updating CENSO: %s", e$message))
            shiny::showNotification(sprintf("CENSO IBGE - Erro: %s", e$message), type = "error")
            return(FALSE)
          }
        )
        if (!result) {
          shiny::showNotification("CENSO IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_estimativa, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Estimativa")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        # protect the update call so the server doesn't hang on errors
        result <- tryCatch(
          {
            rsan::update_ibge_estimativa(input$estimativa_ano)
          },
          error = function(e) {
            # log and notify the user, return FALSE so downstream logic handles it
            rlog::log_error(sprintf("Error updating Estimativa IBGE: %s", e$message))
            shiny::showNotification(sprintf("Estimativa IBGE - Erro: %s", e$message), type = "error")
            return(FALSE)
          }
        )
        if (!result) {
          shiny::showNotification("Estimativa IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_sinisa, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SINISA")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- tryCatch(
          {
            rsan::download_and_preprocess_sinisa(input$sinisa_ano)
          },
          error = function(e) {
            # log and notify the user, return FALSE so downstream logic handles it
            rlog::log_error(sprintf("Error updating SINISA: %s", e$message))
            shiny::showNotification(sprintf("SINISA - Erro: %s", e$message), type = "error")
            return(FALSE)
          }
        )

        if (is.null(result) || identical(result, FALSE)) {
          shiny::showNotification("SINISA - Atualização indisponível!")
        }
      })
    })

    ## end module server logic
  })
}
