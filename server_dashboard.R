choice_to_varname <- function(choice) {
  if (choice == "País") {
    return("pais")
  }
  if (choice == "Região") {
    return("regiao")
  }
  if (choice == "UF") {
    return("estado")
  }
}

prepara_dados_investimento <- function(espacial, tabela) {
  espacial_varname <- choice_to_varname(espacial)
  tabela <- dplyr::select(tabela, espacial_varname, investimento_total)
  tabela <- tidyr::drop_na(tabela)
  tabela <- dplyr::group_by(tabela, .data[[espacial_varname]])
  tabela <-
    dplyr::summarize(tabela, investimento_total = sum(investimento_total) /
      1e9)

  return(tabela)
}

prepara_dados_soma <- function(espacial, tabela, vars) {
  espacial_varname <- choice_to_varname(espacial)
  vars <- c(espacial_varname, vars)
  tabela <- dplyr::select(tabela, dplyr::all_of(vars))
  tabela <- tidyr::drop_na(tabela)
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
    data <- prepara_dados_soma(input$espacial, dado(), vars = c("investimento_total", "investimento_expansao", "investimento_reposicao"))
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

plot_deficit_agua_esgoto <- function(input, dado) {
  renderPlotly({
    data <- prepara_dados_soma(input$espacial, dado(), vars = c("deficit_agua_total", "deficit_esgoto_total"))
    fig <-
      plotly::plot_ly(
        data,
        x = ~ .data[[choice_to_varname(input$espacial)]],
        y = ~deficit_agua_total,
        type = "bar",
        name = "Água"
      )
    fig <- plotly::add_trace(fig, y = ~ .data[["deficit_esgoto_total"]], name = "Esgoto")
    fig <- plotly::layout(
      fig,
      title = "Déficit de Atendimento Previsto",
      yaxis = list(title = "Déficit (habitantes)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

dashboard_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    agua_esgoto <- reactiveVal(app_state$agua_esgoto$resultado)
    residuos <- reactiveVal(app_state$residuos$resultado$resultado)
    drenagem <- reactiveVal(app_state$drenagem$resultado)

    # GERAL

    # AGUA E ESGOTO
    output$agua_esgoto_investimento <- plot_investimento_total(input, agua_esgoto)
    output$agua_esgoto_investimento_por_tipo <- plot_investimento_por_tipo(input, agua_esgoto)
    output$agua_esgoto_deficit <- plot_deficit_agua_esgoto(input, agua_esgoto)

    # RESIDUOS
    output$residuos_investimento <- plot_investimento_total(input, residuos)
    output$residuos_investimento_por_tipo <- plot_investimento_por_tipo(input, residuos)

    # DRENAGEM
    output$drenagem_investimento <- plot_investimento_total(input, drenagem)
    output$drenagem_investimento_por_tipo <- plot_investimento_por_tipo(input, drenagem)
  })
}
