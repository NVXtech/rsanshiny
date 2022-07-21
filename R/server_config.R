config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$ano, {
      if (!is.null(input$ano)) {
        app_state$input$geral$ano <- input$ano
        rsan::save_state(app_state)
      }
    })
    shiny::observeEvent(input$atualizar, {
      rlog::log_info("Updating data")
    })
  })
}
