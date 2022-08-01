config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {

    shiny::observeEvent(input$ano, {
      if (!is.null(input$ano)) {
        app_state$input$geral$ano <- input$ano
        rsan::save_state(app_state)
      }
    })

    shiny::observeEvent(input$atualizar_sinapi, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SINAPI")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_sinapi(input$sinapi_ano, input$sinapi_mes)
        if (!result) {
          shiny::showNotification("SINAPI - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_censo, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Censo")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_ibge_censo(input$censo_ano)
        if (!result) {
          shiny::showNotification("CENSO IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_estimativa, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Estimativa")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_ibge_estimativa(input$estimativa_ano)
        if (!result) {
          shiny::showNotification("Estimativa IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_snis_ap, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SNIS-AP")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_snis_ap(input$snis_ap_ano)
        if (!result) {
          shiny::showNotification("SNIS-AP - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_snis_rs, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SNIS-RS")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_snis_rs(input$snis_rs_ano)
        if (!result) {
          shiny::showNotification("SNIS-RS - Atualização indisponível!")
        }
      })
    })
  })
}
