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
    if (input$orientacao== "v"){
      eixo_x <- input$eixo
      eixo_y <- "necessidade_investimento"
      label_x <- input$eixo
      label_y <- "Necessidade de investimento (R$)"
    } else {
      eixo_y <- input$eixo
      eixo_x <- "necessidade_investimento"
      label_y <- input$eixo
      label_x <- "Necessidade de investimento (R$)"
    }
    cores <- input$cores
    tipo_barra <- input$barra
    grupos <- c(input$eixo, cores)
    data <- prepara_dados(dado(), grupos)
    fig <- plot_ly(
          data,
          x = ~ .data[[eixo_x]],
          y = ~ .data[[eixo_y]],
          color = ~ .data[[cores]],
          type = "bar"
      )
    fig <- plotly::layout(
      fig,
      xaxis = list(title = label_x),
      yaxis = list(title = label_y),
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

analise_server <- function(id, app_state, parent) {
  shiny::moduleServer(id, function(input, output, session) {
    geral_longa <- shiny::reactiveVal(app_state$geral_longa)

    output$grafico <- plot_analise(input, geral_longa)
    output$tbl <- tabela_analise(input, geral_longa)

    shiny::observeEvent(parent$pages, {
      print("tab changed")
      update_state()
    })

    update_state <- function() {
      rlog::log_info("Updating analise app state")
      app_state <- rsan::load_app_state()
      geral_longa(app_state$geral_longa)
    }

    return(update_state)
  })
}
