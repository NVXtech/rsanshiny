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
        shiny::incProgress(0, detail = "Carregando cálculos anteriores")
        app_state <- rsan::load_app_state()
        shiny::incProgress(1 / n, detail = "Salvando novos parâmetros")
        app_state <- rsan::salva_parametros(app_state, input, id)
        shiny::incProgress(1 / n, detail = "Investimento")
        app_state <- rsan::rodar_modelo(app_state)
        shiny::incProgress(1 / n, detail = "Salvando resultados")
        rsan::save_state(app_state)
        shiny::incProgress(1 / n, detail = "Fim")
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
