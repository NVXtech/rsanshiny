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
  })
}
