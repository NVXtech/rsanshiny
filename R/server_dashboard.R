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
    investimento_total = round(sum(investimento_total) * 1e-9, 2)
  )
  return(tabela)
}

prepara_dados_soma <- function(espacial, tabela, vars) {
  espacial_varname <- choice_to_varname(espacial)
  vars <- c(espacial_varname, vars)
  tabela <- dplyr::select(tabela, dplyr::all_of(vars))
  tabela <- tidyr::drop_na(tabela)
  tabela <- dplyr::group_by(tabela, .data[[espacial_varname]])
  tabela <- dplyr::summarize(
    tabela,
    dplyr::across(where(is.numeric), ~ round(sum(.x, na.rm = TRUE), 2))
  )
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
  plotly::renderPlotly({
    espacial <- input$espacial
    data <- prepara_dados_investimento(espacial, dado())
    fig <- plotly::plot_ly(
      data,
      labels = ~ .data[[choice_to_varname(input$espacial)]],
      values = ~investimento_total,
      textinfo = "label",
      hoverinfo = "label+percent+text",
      text = ~ sprintf(
        "R$ %s bilhões",
        adiciona_marcador_milhar(investimento_total)
      ),
      type = "pie"
    )
    fig <- plotly::layout(fig, title = "Necessidade de investimento total")
  })
}

tabela_investimento_total <- function(input, dado) {
  tbl <- shiny::reactive(
    prepara_dados_investimento(input$espacial, dado())
  )
  rsanshiny:::create_datatable(tbl)
}

plot_investimento_por_tema <- function(input, dado) {
  vars <- c(
    "investimento_total_agua",
    "investimento_total_esgoto",
    "investimento_total_drenagem",
    "investimento_total"
  )
  title <- c("Água", "Esgoto", "Drenagem", "Total")
  plotly::renderPlotly({
    data <- prepara_dados_soma(input$espacial, dado(), vars = vars)
    fig <-
      plotly::plot_ly(
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
      title = "Necessidade de investimento por componente",
      yaxis = list(title = "Necessidade de investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

tabela_investimento_por_tema <- function(input, dado) {
  vars <- c(
    "investimento_total_agua",
    "investimento_total_esgoto",
    "investimento_total_drenagem",
    "investimento_total"
  )
  data <- shiny::reactive(
    prepara_dados_soma(input$espacial, dado(), vars = vars)
  )
  rsanshiny:::create_datatable(data)
}

plot_investimento_por_tipo <- function(input, dado, drenagem = FALSE) {
  plotly::renderPlotly({
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
      title = "Necessidade de investimento por destino",
      yaxis = list(title = "Necessidade de investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

tabela_investimento_por_tipo <- function(input, dado, drenagem = FALSE) {
  vars <- c("investimento_expansao", "investimento_reposicao")
  if (drenagem) {
    vars <- c(vars, "investimento_cadastro")
  }
  vars <- c(vars, "investimento_total")
  data <- shiny::reactive(
    prepara_dados_soma(input$espacial, dado(), vars = vars)
  )
  rsanshiny:::create_datatable(data)
}

plot_deficit <- function(input, dado) {
  plotly::renderPlotly({
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

plot_sankey <- function(input, dado) {
  plotly::renderPlotly({
    data <- rsan::prepara_sankey(dado(), c("situacao", "destino", "componente", "etapa"))
    fig <- plotly::plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(
        label = data$labels,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      link = list(
        source = data$link$source,
        target = data$link$target,
        value =  data$link$value
      )
    )
  })
}

tabela_deficit <- function(input, dado, vars) {
  vars <- c("deficit_urbana", "deficit_rural", "deficit_total")
  data <- shiny::reactive(
    prepara_dados_soma(input$espacial, dado(), vars = vars)
  )
  rsanshiny:::create_datatable(data)
}

dashboard_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    geral <- shiny::reactiveVal(app_state$geral)
    geral_longa <- shiny::reactiveVal(app_state$geral_longa)
    agua <- shiny::reactiveVal(app_state$agua)
    esgoto <- shiny::reactiveVal(app_state$esgoto)
    residuos <- shiny::reactiveVal(app_state$residuos)
    drenagem <- shiny::reactiveVal(app_state$drenagem)

    # GERAL
    output$geral_investimento <- plot_investimento_total(input, geral)
    output$tbl_geral_investimento <- tabela_investimento_total(input, geral)
    output$geral_sankey <- plot_sankey(input, geral_longa)

    output$geral_investimento_por_tema <-
      plot_investimento_por_tema(input, geral)
    output$tbl_geral_investimento_por_tema <-
      tabela_investimento_por_tema(input, geral)

    # AGUA
    output$agua_investimento <- plot_investimento_total(input, agua)
    output$agua_investimento_por_tipo <- plot_investimento_por_tipo(input, agua)
    output$agua_deficit <- plot_deficit(input, agua)

    output$tbl_agua_investimento <- tabela_investimento_total(input, agua)
    output$tbl_agua_investimento_por_tipo <- tabela_investimento_por_tipo(input, agua)
    output$tbl_agua_deficit <- tabela_deficit(input, agua)

    # ESGOTO
    output$esgoto_investimento <- plot_investimento_total(input, esgoto)
    output$esgoto_investimento_por_tipo <- plot_investimento_por_tipo(input, esgoto)
    output$esgoto_deficit <- plot_deficit(input, esgoto)

    output$tbl_esgoto_investimento <- tabela_investimento_total(input, esgoto)
    output$tbl_esgoto_investimento_por_tipo <- tabela_investimento_por_tipo(input, esgoto)
    output$tbl_esgoto_deficit <- tabela_deficit(input, esgoto)

    # RESIDUOS
    output$residuos_investimento <- plot_investimento_total(input, residuos)
    output$residuos_investimento_por_tipo <- plot_investimento_por_tipo(input, residuos)

    output$tbl_residuos_investimento <- tabela_investimento_total(input, residuos)
    output$tbl_residuos_investimento_por_tipo <- tabela_investimento_por_tipo(input, residuos)
    # DRENAGEM
    output$drenagem_investimento <- plot_investimento_total(input, drenagem)
    output$drenagem_investimento_por_tipo <- plot_investimento_por_tipo(input, drenagem, drenagem = TRUE)

    output$tbl_drenagem_investimento <- tabela_investimento_total(input, drenagem)
    output$tbl_drenagem_investimento_por_tipo <- tabela_investimento_por_tipo(input, drenagem, drenagem = TRUE)

    # Tabset Events
    shiny::observeEvent(input$dash_tab, {
      update_state()
    })

    update_state <- function() {
      rlog::log_info("Updating dashboard app state")
      app_state <- rsan::load_app_state()
      geral(app_state$geral)
      agua(app_state$agua)
      esgoto(app_state$esgoto)
      residuos(app_state$residuos)
      drenagem(app_state$drenagem)
      geral_longa(app_state$geral_longa)
    }

    return(update_state)
  })
}
