projecao_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$fonte2, {
      shiny::updateSliderInput(
        session,
        inputId = "ano",
        min = rsan::nome_para_ano(input$fonte2),
        value = app_state$projecao$ano
      )
    })

    resultado_projecao <- shiny::reactiveVal(app_state$projecao)

    output$grafico <- plotly::renderPlotly({
      dados <- resultado_projecao()
      if (is.null(dados)) {
        rlog::log_warn("resultado da projecao não encontrado")
        return()
      }
      data <- tidyr::drop_na(dados)
      data <- dplyr::group_by(data, tipo_populacao, ano)
      data <- dplyr::summarize(data,
        populacao = sum(populacao) / 1e6,
        .groups = "drop_last"
      )
      data <- tidyr::pivot_wider(
        data,
        names_from = tipo_populacao,
        values_from = populacao, names_sep = "_"
      )
      fig <- plotly::plot_ly(
        data,
        x = ~ano,
        y = ~rural,
        type = "bar",
        name = "Rural"
      )
      fig <- plotly::add_trace(fig, y = ~urbana, name = "Urbana")
      fig <- plotly::add_trace(fig, y = ~total, name = "Total")

      fig <- plotly::layout(
        fig,
        title = "Projeção Populacional",
        yaxis = list(title = "Polução (milhões)"),
        xaxis = list(title = "anos"),
        barmode = "group"
      )
    })

    shiny::observeEvent(input$rodar, {
      shiny::withProgress(message = "Calculando Investimento", value = 0, {
        n <- 3
        shiny::incProgress(1 / n, detail = "Carregando cálculos anteriores")
        app_state <- rsan::load_app_state()
        shiny::incProgress(0, detail = "Salvando parâmetros")
        app_state <- rsan::salva_parametros(app_state, input, "projecao")
        shiny::incProgress(1 / n, detail = "Calculando Investimento")
        app_state <- rsan::rodar_modelo(app_state)
        resultado_projecao(app_state$projecao)
        shiny::incProgress(1 / n, detail = "Salvando Resultados")
        rsan::save_state(app_state)
        shiny::incProgress(1 / n, detail = "Fim")
      })
    })
  })
}
