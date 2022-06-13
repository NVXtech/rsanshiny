rodar_projecao_populacional <- function (input) {
  fonte1 <- as_tibble(load_data(input$fonte1))
  fonte1 <- adicionar_proporcao_urbana_rural(fonte1)
  if (grepl(".*censo.*", input$fonte2)) {
    # TODO: implementar para caso a fonte seja outro censo
    showNotification("Fonte de dados 2 não pode ser censo!", type = "error")
    return()
  }
  fonte2 <- as_tibble(load_data(input$fonte2))
  ano1 <- get_year_from_path(input$fonte1)
  ano2 <- get_year_from_path(input$fonte2)
  consolidado <- junta_fontes_populacao(fonte1, fonte2)
  consolidado <- calcula_taxa_crescimento(consolidado, ano1, ano2)
  consolidado <- calcular_urbana_rural_fonte2(consolidado)
  return(calcula_projecao(consolidado, ano2, input$ano))
}

projecao_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$fonte2, {
      updateSliderInput(
        session,
        inputId = "ano",
        min = rsan::get_year_from_path(input$fonte2),
        value = app_state$projecao$modelar_ate
      )
    })

    resultado_projecao <- reactiveVal(app_state$projecao$resultado)

    output$grafico = renderPlotly({
      df <- drop_na(resultado_projecao())
      df <- group_by(df, tipo_populacao, ano)
      df <-
        summarize(df,
                  populacao = sum(populacao),
                  .groups = "drop_last")
      p <-
        ggplot(data = df, aes(x = ano, y = populacao, fill = tipo_populacao))
      p <- p + geom_bar(position = "dodge", stat = "identity")
      p <- p + labs(x = "tempo (anos)", y = "População", fill = "Classe")
      ggplotly(p)
    })

    observeEvent(input$rodar, {
      rlog::log_info('Running projeção populacional')
      app_state$projecao$resultado <<-
        rodar_projecao_populacional(input)
      resultado_projecao(app_state$projecao$resultado)
      save_projecao_state(input)
    })

  })
}
