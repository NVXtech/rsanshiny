update_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
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

    ## end module server logic
  })
}
