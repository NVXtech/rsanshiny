config_server <- function(id, app_state) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$rodar, {
      shiny::withProgress(message = "Recalculando", value = 0, {
        n <- 4
        shiny::incProgress(0, detail = "Iniciando cálculo")
        shiny::incProgress(1 / n, detail = "Salvando novos parâmetros")
        app_state <- rsan::salva_parametros(app_state, input, id)
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
    shiny::observeEvent(input$ano, {
      if (!is.null(input$ano)) {
        app_state$input$geral$ano <- input$ano
        rsan::save_state(app_state)
      }
    })

    shiny::observeEvent(input$atualizar_sinapi, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SINAPI")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_sinapi(input$sinapi_ano, input$sinapi_mes)
        if (!result) {
          shiny::showNotification("SINAPI - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_censo, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Censo")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_ibge_censo(input$censo_ano)
        if (!result) {
          shiny::showNotification("CENSO IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_estimativa, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating Estimativa")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_ibge_estimativa(input$estimativa_ano)
        if (!result) {
          shiny::showNotification("Estimativa IBGE - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_snis_ap, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SNIS-AP")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_snis_ap(input$snis_ap_ano)
        if (!result) {
          shiny::showNotification("SNIS-AP - Atualização indisponível!")
        }
      })
    })

    shiny::observeEvent(input$atualizar_snis_rs, {
      shiny::withProgress(message = "Baixando", value = 0, {
        rlog::log_info("Updating SNIS-RS")
        shiny::incProgress(0.2, detail = "Aguarde, pode demorar...")
        result <- rsan::update_snis_rs(input$snis_rs_ano)
        if (!result) {
          shiny::showNotification("SNIS-RS - Atualização indisponível!")
        }
      })
    })

    output$agua_fonte_ano <- shiny::renderUI({
      if (is.null(input$agua_fonte_nome) || input$agua_fonte_nome == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("agua-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("agua", input$agua_fonte_nome),
        selected = app_state$input$agua$fonte_ano
      )
    })

    output$agua_atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("agua-atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis(input$agua_atendimento),
        selected = app_state$input$agua$atendimento_ano
      )
    })

    output$esgoto_fonte_ano <- shiny::renderUI({
      if (is.null(input$esgoto_fonte_nome) || input$esgoto_fonte_nome == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("esgoto-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("esgoto", input$esgoto_fonte_nome),
        selected = app_state$input$esgoto$fonte_ano
      )
    })

    output$esgoto_atendimento_ano <- shiny::renderUI({
      shiny::selectInput(
        inputId = session$ns("esgoto-atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = atendimento_anos_disponiveis(input$esgoto_atendimento),
        selected = app_state$input$esgoto$atendimento_ano
      )
    })

    output$residuos_fonte_ano <- shiny::renderUI({
      if (is.null(input$residuos_fonte_nome) || input$residuos_fonte_nome == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("residuos-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("residuos", input$residuos_fonte_nome),
        selected = app_state$input$residuos$fonte_ano
      )
    })

    output$drenagem_fonte_ano <- shiny::renderUI({
      if (is.null(input$drenagem_fonte_nome) || input$drenagem_fonte_nome == "") {
        return(NULL)
      }
      shiny::selectInput(
        inputId = session$ns("drenagem-fonte_ano"),
        label = shiny::strong("Selecione o ano da fonte estruturas"),
        choices = estrutura_anos_disponiveis("drenagem", input$drenagem_fonte_nome),
        selected = app_state$input$drenagem$fonte_ano
      )
    })
  })
}
