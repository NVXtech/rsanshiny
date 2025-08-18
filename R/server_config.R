config_server <- function(id, app_state, parent) {
  shiny::moduleServer(id, function(input, output, session) {
    output$agua_fonte_ano <- shiny::renderUI({
      if (is.null(input[["agua-fonte_nome"]]) || input[["agua-fonte_nome"]] == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("agua-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("agua", input[["agua-fonte_nome"]]),
        selected = app_state$input$agua$fonte_ano
      )
    })

    output$agua_atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("agua-atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis("agua", input[["agua-atendimento"]]),
        selected = app_state$input$agua$atendimento_ano
      )
    })

    output$esgoto_fonte_ano <- shiny::renderUI({
      if (is.null(input[["esgoto-fonte_nome"]]) || input[["esgoto-fonte_nome"]] == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("esgoto-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("esgoto", input[["esgoto-fonte_nome"]]),
        selected = app_state$input$esgoto$fonte_ano
      )
    })

    output$esgoto_atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("esgoto-atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis("esgoto", input[["esgoto-atendimento"]]),
        selected = app_state$input$esgoto$atendimento_ano
      )
    })

    output$residuos_fonte_ano <- shiny::renderUI({
      if (is.null(input[["residuos-fonte_nome"]]) || input[["residuos-fonte_nome"]] == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("residuos-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("residuos", input[["residuos-fonte_nome"]]),
        selected = app_state$input$residuos$fonte_ano
      )
    })

    output$residuos_atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("residuos-atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis("residuos", input[["residuos-atendimento"]]),
        selected = app_state$input$residuos$atendimento_ano
      )
    })
    output$drenagem_info <- shiny::renderText({
      "Os cálculos de drenagem urbana utiliza a listagem de municípios críticos que podem ser atualizada em `dados\\base_calculo\\drenagem_municipios_criticos.csv`. Somente os municipios críticos que estão nesta lista serão considerados para os cálculos de drenagem urbana."
    })

    shiny::observeEvent(input$rodar, {
      shiny::withProgress(message = "Recalculando", value = 0, {
        n <- 4
        shiny::incProgress(0, detail = "Iniciando cálculo")
        shiny::incProgress(1 / n, detail = "Salvando novos parâmetros")
        app_state <- salva_estado(app_state, input, id)
        shiny::incProgress(1 / n, detail = "Investimento")
        app_state <- rsan::rodar_modelo(app_state)
        shiny::incProgress(1 / n, detail = "Salvando resultados")
        rsan::save_state(app_state)
        shiny::incProgress(1 / n, detail = "Fim")
      })
    })

    shiny::observeEvent(input$salvar, {
      shiny::withProgress(message = "Salvando Parâmetros", value = 0, {
        app_state <- salva_estado(app_state, input, id)
      })
    })
  })
}
