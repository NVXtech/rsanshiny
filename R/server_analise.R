prepara_dados <- function(tabela, grupos) {
  for (grupo in grupos) {
    tabela <- dplyr::group_by(tabela, .data[[grupo]], .add = TRUE)
  }

  tabela <- dplyr::summarize(
    tabela,
    necessidade_investimento = round(
      sum(necessidade_investimento, na.rm=TRUE),
      2)
  )
  return(tabela)
}

plot_analise <- function(input, dado) {
  plotly::renderPlotly({
    eixo <- input$eixo
    cores <- input$cores
    tipo_barra <- input$barra
    grupos <- c(eixo, cores)
    data <- prepara_dados(dado(), grupos)
    print(data)
    fig <- plot_ly(
          data,
          y = ~ necessidade_investimento,
          x = ~ .data[[eixo]],
          color = ~ .data[[cores]],
          type = "bar"
      )
    fig <- plotly::layout(
      fig,
      yaxis = list(title = "Necessidade de investimento (R$)"),
      xaxis = list(title = eixo),
      barmode = tipo_barra,
      showlegend = input$legenda
      )
  })
}


tabela_analise <- function(input, geral_longa) {
  return(
    DT::renderDataTable(
      {
        eixo <- input$eixo
        cores <- input$cores
        grupos <- c(eixo, cores)
        dt <- prepara_dados(geral_longa(), grupos)
        DT::formatCurrency(
          DT::datatable(dt,
                        extensions = c("Buttons", "Scroller"),
                        options = list(
                          scrollX = TRUE,
                          deferRender = TRUE,
                          scrollY = 200,
                          scroller = TRUE,
                          dom = "Bfrtip",
                          buttons = list(
                            "copy",
                            list(
                              extend = "excel",
                              text = "Download",
                              filename = "investimento"
                            )
                          )
                        )),
          currency = "R$ ",
          mark = ".",
          dec.mark = ",",
          columns='necessidade_investimento'
        )
      }
    )
  )
}

analise_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    geral_longa <- shiny::reactiveVal(app_state$geral_longa)

    output$grafico <- plot_analise(input, geral_longa)
    output$tbl <- tabela_analise(input, geral_longa)

    shiny::observeEvent(input$dash_tab, {
      update_state()
    })

    update_state <- function() {
      rlog::log_info("Updating dashboard app state")
      app_state <- rsan::load_app_state()
      geral_longa(app_state$geral_longal)
    }

    return(update_state)
  })
}
