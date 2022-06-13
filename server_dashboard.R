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

dashboard_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$espacial, {

    })

    agua_esgoto <-
      reactiveVal(app_state$modulo_financeiro$resultado)

    output$investimento <- renderPlotly({
      espacial <- input$espacial
      data <- prepara_dados_investimento(espacial, agua_esgoto())
      fig <-
        plot_ly(
          data,
          labels = ~ .data[[choice_to_varname(input$espacial)]],
          values = ~investimento_total,
          textinfo = "label",
          hoverinfo = "label+percent+text",
          text = ~ sprintf("R$ %s bilhões", adiciona_marcador_milhar(round(investimento_total, 2))),
          type = "pie"
        )
    })

    output$investimento_segmentado <- renderPlotly({
      data <- prepara_dados_soma(input$espacial, agua_esgoto())
      fig <-
        plot_ly(
          data,
          x = ~ .data[[choice_to_varname(input$espacial)]],
          y = ~investimento_total,
          type = "bar",
          name = "Total"
        )
      fig <- fig %>%
        add_trace(y = ~investimento_expansao, name = "Expansão")
      fig <- fig %>%
        add_trace(y = ~investimento_reposicao, name = "Reposição")
      fig <-
        fig %>% layout(
          yaxis = list(title = "Investimento (R$ bilhões)"),
          xaxis = list(title = input$espacial),
          barmode = "group"
        )
    })
    output$deficit <- renderPlotly({
      data <- prepara_dados_soma(input$espacial, agua_esgoto())
      fig <-
        plot_ly(
          data,
          x = ~ .data[[choice_to_varname(input$espacial)]],
          y = ~deficit_agua_total,
          type = "bar",
          name = "Água"
        )
      fig <- fig %>%
        add_trace(y = ~deficit_esgoto_total, name = "Esgoto")
      fig <-
        fig %>% layout(
          yaxis = list(title = "Déficit de atendimento (hab.)"),
          xaxis = list(title = input$espacial),
          barmode = "group"
        )
    })
  })
}
