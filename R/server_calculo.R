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

    shiny::observeEvent(parent$pages, {
      update_sinapi_ui()
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

    update_sinapi_ui <- function() {
      shiny::updateSelectInput(
        session, "sinapi",
        choices = sort(get_sinapi_list(), decreasing = T),
        selected = input$sinapi
      )
      shiny::updateSelectInput(
        session, "snis_rs",
        choices = rsan::get_snis_rs_list(),
        selected = input$snis_rs
      )
    }
  })
}


#' Agua Server
#'
#' @param input shiny input
#' @param app_state estado da aplicaçào
#' @param parent shiny parent input
#'
#' @return shiny server
#' @export
agua_server <- function(id, app_state, parent) {
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
        # TODO: check why save state does not save ano_inicial
        rsan::save_state(app_state)
        shiny::incProgress(1 / n, detail = "Fim")
      })
    })

    output$fonte_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("fonte_ano"),
        label = shiny::strong(paste0("Selecione o ano da fonte estruturas (", input$fonte_nome, ")")),
        choices = estrutura_anos_disponiveis("agua", input$fonte_nome),
        selected = app_state$input$agua$fonte_ano
      )
    })

    output$atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis(input$atendimento),
        selected = app_state$input$agua$atendimento_ano
      )
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
