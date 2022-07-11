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
  tabela <- dplyr::summarize(
    tabela,
    investimento_total = sum(investimento_total) * 1e-9
  )
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

plot_investimento_por_tema <- function(input, dado) {
  vars <- c("investimento_total_agua", "investimento_total_esgoto", "investimento_total_drenagem", "investimento_total")
  title <- c("Água", "Esgoto", "Drenagem", "Total")
  renderPlotly({
    data <- prepara_dados_soma(input$espacial, dado(), vars = vars)
    fig <-
      plot_ly(
        data,
        x = ~ .data[[choice_to_varname(input$espacial)]],
        y = ~ .data[[vars[1]]],
        type = "bar",
        name = title[1]
      )
    for (i in seq(from = 2, to = length(vars))) {
      fig <- plotly::add_trace(fig, y = data[[vars[i]]], name = title[i])
    }

    fig <- plotly::layout(
      fig,
      title = "Investimento por Tema",
      yaxis = list(title = "Investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

plot_investimento_por_tipo <- function(input, dado, drenagem = FALSE) {
  renderPlotly({
    vars <- c("investimento_expansao", "investimento_reposicao")
    title <- c("Expansão", "Reposição")
    if (drenagem) {
      vars <- c(vars, "investimento_cadastro")
      title <- c(title, "Cadastro")
    }
    vars <- c(vars, "investimento_total")
    title <- c(title, "Total")
    data <- prepara_dados_soma(input$espacial, dado(), vars = vars)
    fig <- plotly::plot_ly(
      data,
      x = ~ .data[[choice_to_varname(input$espacial)]],
      y = ~ .data[[vars[1]]],
      type = "bar",
      name = title[1]
    )
    for (i in seq(from = 2, to = length(vars))) {
      fig <- plotly::add_trace(fig, y = data[[vars[i]]], name = title[i])
    }
    fig <- plotly::layout(
      fig,
      title = "Investimento por Tipo",
      yaxis = list(title = "Investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

plot_deficit <- function(input, dado) {
  renderPlotly({
    vars <- c("deficit_urbana", "deficit_rural", "deficit_total")
    title <- c("Urbana", "Rural", "Total")
    data <- prepara_dados_soma(input$espacial, dado(), vars = vars)
    fig <- plotly::plot_ly(
      data,
      x = ~ .data[[choice_to_varname(input$espacial)]],
      y = ~ .data[[vars[1]]],
      type = "bar",
      name = title[1]
    )
    for (i in seq(from = 2, to = length(vars))) {
      fig <- plotly::add_trace(fig, y = data[[vars[i]]], name = title[i])
    }
    fig <- plotly::layout(
      fig,
      title = "Déficit de Atendimento Previsto",
      yaxis = list(title = "Déficit (habitantes)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    app_state <- rsan::load_app_state()

    geral <- reactiveVal(app_state$geral)
    agua <- reactiveVal(app_state$agua)
    esgoto <- reactiveVal(app_state$esgoto)
    residuos <- reactiveVal(app_state$residuos)
    drenagem <- reactiveVal(app_state$drenagem)

    # GERAL
    output$geral_investimento <- plot_investimento_total(input, geral)
    output$geral_investimento_por_tipo <- plot_investimento_por_tema(input, geral)

    # AGUA
    output$agua_investimento <- plot_investimento_total(input, agua)
    output$agua_investimento_por_tipo <- plot_investimento_por_tipo(input, agua)
    output$agua_deficit <- plot_deficit(input, agua)

    # ESGOTO
    output$esgoto_investimento <- plot_investimento_total(input, esgoto)
    output$esgoto_investimento_por_tipo <- plot_investimento_por_tipo(input, esgoto)
    output$esgoto_deficit <- plot_deficit(input, esgoto)

    # RESIDUOS
    output$residuos_investimento <- plot_investimento_total(input, residuos)
    output$residuos_investimento_por_tipo <- plot_investimento_por_tipo(input, residuos)

    # DRENAGEM
    output$drenagem_investimento <- plot_investimento_total(input, drenagem)
    output$drenagem_investimento_por_tipo <- plot_investimento_por_tipo(input, drenagem, drenagem = TRUE)

    # Tabset Events
    observeEvent(input$dash_tab, {
      rlog::log_info("Updating dashboard app state")
      app_state <- rsan::load_app_state()
      geral <- reactiveVal(app_state$geral)
      agua(app_state$agua)
      esgoto(app_state$esgoto)
      residuos(app_state$residuos)
      drenagem(app_state$drenagem)
    })
  })
}
