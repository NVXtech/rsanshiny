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

tab_to_varname <- function(tab) {
  if (tab == "Geral") {
    return("geral")
  }
  if (tab == "Água") {
    return("agua")
  }
  if (tab == "Esgoto") {
    return("esgoto")
  }
  if (tab == "Resíduos") {
    return("residuos")
  }
  if (tab == "Drenagem") {
    return("drenagem")
  }
}



prepara_dados_por_componente <- function(tabela, grupos, nome_componente) {
  tabela <- dplyr::filter(tabela, componente == nome_componente)
  tabela <- prepara_dados(tabela, grupos)
  return(tabela)
}

prepara_deficit <- function(tabela, grupos) {
  for (grupo in grupos) {
    tabela <- dplyr::group_by(tabela, .data[[grupo]], .add = TRUE)
  }
  tabela <- dplyr::summarize(
    tabela,
    deficit = sum(deficit, na.rm = TRUE)
  )
  return(tabela)
}

prepara_deficit_por_componente <- function(tabela, grupos, nome_componente) {
  tabela <- dplyr::filter(tabela, componente == nome_componente)
  tabela <- rsanshiny:::prepara_deficit(tabela, grupos)
  return(tabela)
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
    espacial <- choice_to_varname(input$espacial)
    grupos <- c(espacial)
    data <- rsanshiny:::prepara_dados(dado(), grupos)
    fig <- plotly::plot_ly(
      data,
      labels = ~ .data[[espacial]],
      values = ~ necessidade_investimento * 1e-9,
      textinfo = "label",
      hoverinfo = "label+percent+text",
      text = ~ sprintf(
        "R$ %s bilhões",
        adiciona_marcador_milhar(necessidade_investimento * 1e-9)
      ),
      type = "pie"
    )
    fig <- plotly::layout(fig, title = "Necessidade de investimento total")
  })
}

plot_investimento_total_por_componente <- function(input, dado) {
  plotly::renderPlotly({
    espacial <- choice_to_varname(input$espacial)
    grupos <- c(espacial, "componente")
    componente <- tab_to_varname(input$dash_tab)
    data <- rsanshiny:::prepara_dados(dado(), grupos)
    fig <- plotly::plot_ly(
      data,
      x = ~ .data[[espacial]],
      y = ~necessidade_investimento,
      color = ~componente,
      type = "bar"
    )
    fig <- plotly::layout(
      fig,
      title = "Necessidade de investimento por componente",
      yaxis = list(title = "Necessidade de investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}


plot_investimento_por_componente <- function(input, dado) {
  plotly::renderPlotly({
    espacial <- choice_to_varname(input$espacial)
    grupos <- c(espacial)
    componente <- tab_to_varname(input$dash_tab)
    data <- rsanshiny:::prepara_dados_por_componente(dado(), grupos, componente)
    fig <- plotly::plot_ly(
      data,
      labels = ~ .data[[espacial]],
      values = ~ necessidade_investimento * 1e-9,
      textinfo = "label",
      hoverinfo = "label+percent+text",
      text = ~ sprintf(
        "R$ %s bilhões",
        adiciona_marcador_milhar(necessidade_investimento * 1e-9)
      ),
      type = "pie"
    )
    fig <- plotly::layout(fig, title = "Necessidade de investimento total")
  })
}

tabela_investimento_por_componente <- function(input, dado) {
  tbl <- shiny::reactive(
    prepara_dados_por_componente(
      dado(),
      grupos = c(choice_to_varname(input$espacial)),
      nome_componente = tab_to_varname(input$dash_tab)
    )
  )
  rsanshiny:::create_datatable(tbl)
}

plot_investimento_por_destino <- function(input, dado) {
  plotly::renderPlotly({
    espacial <- choice_to_varname(input$espacial)
    grupos <- c(espacial, "destino")
    componente <- tab_to_varname(input$dash_tab)
    data <- rsanshiny:::prepara_dados_por_componente(dado(), grupos, componente)
    fig <- plotly::plot_ly(
      data,
      x = ~ .data[[espacial]],
      y = ~necessidade_investimento,
      color = ~destino,
      type = "bar"
    )
    fig <- plotly::layout(
      fig,
      title = "Necessidade de investimento por destino",
      yaxis = list(title = "Necessidade de investimento (R$ bilhões)"),
      xaxis = list(title = input$espacial),
      barmode = "group"
    )
  })
}

tabela_investimento_total <- function(input, dado) {
  data <- shiny::reactive(
    rsanshiny:::prepara_dados(
      dado(),
      grupos = c(choice_to_varname(input$espacial))
    )
  )
  rsanshiny:::create_datatable(data)
}

tabela_investimento_total_por_componente <- function(input, dado) {
  data <- shiny::reactive(
    rsanshiny:::prepara_dados(
      dado(),
      grupos = c(choice_to_varname(input$espacial), "componente")
    )
  )
  rsanshiny:::create_datatable(data)
}

tabela_investimento_por_componente <- function(input, dado) {
  data <- shiny::reactive(
    rsanshiny:::prepara_dados_por_componente(
      dado(),
      grupos = c(choice_to_varname(input$espacial), "componente"),
      nome_componente = tab_to_varname(input$dash_tab)
    )
  )
  rsanshiny:::create_datatable(data)
}

tabela_investimento_por_destino <- function(input, dado, drenagem = FALSE) {
  data <- shiny::reactive(
    prepara_dados_por_componente(
      dado(),
      grupos = c(choice_to_varname(input$espacial), "destino"),
      nome_componente = tab_to_varname(input$dash_tab)
    )
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

plot_deficit_por_componente <- function(input, dado) {
  plotly::renderPlotly({
    espacial <- choice_to_varname(input$espacial)
    grupos <- c(espacial, "situacao")
    componente <- tab_to_varname(input$dash_tab)
    data <- prepara_deficit_por_componente(dado(), grupos, componente)
    fig <- plotly::plot_ly(
      data,
      x = ~ .data[[espacial]],
      y = ~deficit,
      color = ~situacao,
      type = "bar"
    )
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
    data <- rsan::prepara_sankey(dado(), c("situacao", "destino", "componente", "subsistema"))
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

tabela_deficit_por_componente <- function(input, dado, vars) {
  data <- shiny::reactive(
    prepara_deficit_por_componente(
      dado(),
      c(choice_to_varname(input$espacial), "situacao"),
      tab_to_varname(input$dash_tab)
    )
  )
  rsanshiny:::create_datatable(data)
}

dashboard_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    necessidade <- shiny::reactiveVal(app_state$necessidade)
    deficit <- shiny::reactiveVal(app_state$deficit)

    # Sankey plot
    output$geral_sankey <- plot_sankey(input, necessidade)

    componentes <- c("agua", "esgoto", "residuos", "drenagem")
    for (componente in componentes) {
      id_grafico <- paste0(componente, "_investimento")
      output[[id_grafico]] <- plot_investimento_por_componente(
        input,
        necessidade
      )
      id_tabela <- paste0("tbl_", componente, "_investimento")
      output[[id_tabela]] <- tabela_investimento_por_componente(
        input,
        necessidade
      )

      id_grafico <- paste0(componente, "_investimento_por_destino")
      output[[id_grafico]] <- plot_investimento_por_destino(
        input,
        necessidade
      )
      id_tabela <- paste0("tbl_", componente, "_investimento_por_destino")
      output[[id_tabela]] <- tabela_investimento_por_destino(input, necessidade)
    }

    # GERAL
    output$geral_investimento <- plot_investimento_total(input, necessidade)
    output$tbl_geral_investimento <- tabela_investimento_total(input, necessidade)

    output$geral_investimento_por_componente <-
      plot_investimento_total_por_componente(input, necessidade)
    output$tbl_geral_investimento_por_componente <-
      tabela_investimento_total_por_componente(input, necessidade)

    # DEFICIT
    # # AGUA
    componentes <- c("agua", "esgoto", "residuos")
    for (componente in componentes) {
      id_grafico <- paste0(componente, "_deficit")
      output[[id_grafico]] <- plot_deficit_por_componente(
        input,
        deficit
      )

      id_tabela <- paste0("tbl_", componente, "_deficit")
      output[[id_tabela]] <- tabela_deficit_por_componente(
        input,
        deficit
      )
    }
    # output$agua_deficit <- plot_deficit(input, agua)
    # output$tbl_agua_deficit <- tabela_deficit(input, agua)

    # # ESGOTO
    # output$esgoto_deficit <- plot_deficit(input, esgoto)
    # output$tbl_esgoto_deficit <- tabela_deficit(input, esgoto)

    # Tabset Events
    shiny::observeEvent(input$dash_tab, {
      update_state()
    })

    update_state <- function() {
      rlog::log_info("Updating dashboard app state")
      app_state <- rsan::load_app_state()
      necessidade(app_state$necessidade)
      deficit(app_state$deficit)
    }

    return(update_state)
  })
}
