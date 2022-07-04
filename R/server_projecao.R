projecao_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$fonte2, {
      updateSliderInput(
        session,
        inputId = "ano",
        min = rsan::get_year_from_path(input$fonte2),
        value = app_state$projecao$ano
      )
    })

    resultado_projecao <- reactiveVal(app_state$projecao)

    output$grafico <- renderPlotly({
      dados <- resultado_projecao()
      if (is.null(dados)){
        rlog::log_warn("resultado da projecao não encontrado")
        return()
      }
      df <- drop_na(dados)
      df <- group_by(df, tipo_populacao, ano)
      df <-
        summarize(df,
          populacao = sum(populacao),
          .groups = "drop_last"
        )
      p <-
        ggplot(data = df, aes(x = ano, y = populacao, fill = tipo_populacao))
      p <- p + geom_bar(position = "dodge", stat = "identity")
      p <- p + labs(x = "tempo (anos)", y = "População", fill = "Classe")
      ggplotly(p)
    })

    observeEvent(input$rodar, {
      withProgress(message = "Calculando Investimento", value = 0, {
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
