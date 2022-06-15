choice_to_varname <- function(choice) {
  if (choice == "País") {
    return("pais")
  }
  if (choice == "Região") {
    return("regiao")
  }
  if (choice == "UF") {
    return("Estado")
  }
}

prepara_dados_investimento <- function(espacial, tabela) {
  tabela <- tidyr::drop_na(tabela)
  espacial_varname <- choice_to_varname(espacial)
  tabela <- dplyr::select(tabela, all_of(c(espacial_varname, "investimento_total")))
  tabela <- dplyr::group_by(tabela, .data[[espacial_varname]])
  tabela <-
    dplyr::summarize(tabela, investimento_total = sum(investimento_total) /
      1e9)
}

prepara_dados_soma <- function(espacial, tabela) {
  tabela <- tidyr::drop_na(tabela)
  espacial_varname <- choice_to_varname(espacial)
  tabela <- dplyr::group_by(tabela, .data[[espacial_varname]])
  tabela <- dplyr::summarize(tabela, across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
}

adiciona_marcador_milhar <- function(x) {
  format(
    x,
    big.mark = ".",
    decimal.mark = ",",
    scientific = FALSE
  )
}

plot_investimento_total <- function(input, dado) {
  renderPlotly({
    espacial <- input$espacial
    data <- prepara_dados_investimento(espacial, dado())
    fig <- plotly::plot_ly(
      data,
      labels = ~ .data[[choice_to_varname(input$espacial)]],
      values = ~investimento_total,
      textinfo = "label",
      hoverinfo = "label+percent+text",
      text = ~ sprintf("R$ %s bilhões", adiciona_marcador_milhar(round(investimento_total, 2))),
      type = "pie"
    )
    fig <- plotly::layout(fig, title = "Investimento total")
  })
}

plot_investimento_por_tipo <- function(input, dado) {
  renderPlotly({
    data <- prepara_dados_soma(input$espacial, dado())
    fig <-
      plot_ly(
        data,
        x = ~ .data[[choice_to_varname(input$espacial)]],
        y = ~investimento_total,
        type = "bar",
        name = "Total"
      )
    fig <- plotly::add_trace(fig, y = ~investimento_expansao, name = "Expansão")
    fig <- plotly::add_trace(fig, y = ~investimento_reposicao, name = "Reposição")
    fig <- plotly::layout(
      fig,
      title = "Investimento por Tipo",
      yaxis = list(title = "Investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$espacial, {
    })

    # AGUA E ESGOTO
    agua_esgoto <- reactiveVal(app_state$modulo_financeiro$resultado)
    residuos <- reactiveVal(app_state$residuos$resultado$resultado)

    output$agua_esgoto_investimento <- plot_investimento_total(input, agua_esgoto)
    output$residuos_investimento <- plot_investimento_total(input, residuos)

    output$agua_esgoto_investimento_por_tipo <- plot_investimento_por_tipo(input, agua_esgoto)
    output$residuos_investimento_por_tipo <- plot_investimento_por_tipo(input, residuos)

    output$agua_esgoto_deficit <- renderPlotly({
      data <- prepara_dados_soma(input$espacial, agua_esgoto())
      fig <-
        plotly::plot_ly(
          data,
          x = ~ .data[[choice_to_varname(input$espacial)]],
          y = ~deficit_agua_total,
          type = "bar",
          name = "Água"
        )
      fig <- plotly::add_trace(fig, y = ~deficit_esgoto_total, name = "Esgoto")
      fig <- plotly::layout(
        fig,
        title = "Déficit de Atendimento Previsto",
        yaxis = list(title = "Déficit (habitantes)"),
        xaxis = list(title = input$espacial),
        barmode = "group"
      )

      # RESIDUOS
    })
  })
}
