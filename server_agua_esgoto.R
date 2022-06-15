source("ui_helpers.R")

snis_fields <-
  c(
    "codigo_municipio",
    "POP_TOT",
    "Estado",
    "AG001",
    "AG005",
    "AG006",
    "AG010",
    "AG026",
    "ES001",
    "ES004",
    "ES006",
    "ES026"
  )

rodar_modulo_demografico <- function(input) {
  ano <- input$ano
  snis_data <- rsan::get_snis_data(input$snis, snis_fields)
  snis_data <- rsan::add_density(snis_data)
  snis_data <- rsan::fill_missing_density(snis_data, c("densidade_distribuicao_agua", "densidade_producao_agua", "densidade_coleta_esgoto"))
  consolidado <-
    rsan::consolida_populacao_snis(app_state$projecao$resultado, ano, snis_data)
  consolidado <-
    calculate_geografico(
      consolidado,
      input$meta_agua,
      input$meta_esgoto,
      input$proporcao
    )
  app_state$modulo_demografico$resultado <<- consolidado
}

demografico_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resultado_modelo_demografico <-
      reactiveVal(app_state$modulo_demografico$resultado)

    observeEvent(input$rodar, {
      rlog::log_info("Running modelo demografico")
      rodar_modulo_demografico(input)

      resultado_modelo_demografico(app_state$modulo_demografico$resultado)
      save_modulo_demografico_state(input)
    })

    output$grafico_total <- renderPlotly({
      df <- drop_na(resultado_modelo_demografico())
      df <- gather(df, "tipo_demanda", "demanda", -codigo_municipio)
      df <- group_by(df, tipo_demanda)
      df <-
        summarize(df, demanda = sum(demanda), .groups = "drop_last")
      p <- ggplot(data = df, aes(x = tipo_demanda, y = demanda))
      p <- p + geom_bar(position = "dodge", stat = "identity")
      # p <- p + labs(x="tempo (anos)", y="População")
      ggplotly(p)
    })

    output$tabela <- create_datatable(resultado_modelo_demografico)
  })
}

rodar_modulo_orcamentario <- function(input) {
  sinapi <- load_data(input$sinapi)

  data("projeto_distribuicao_agua")
  app_state$modulo_orcamentario$resultado$distribuicao <<-
    calcula_precos_distribuicao(
      projeto_distribuicao_agua,
      sinapi,
      input$fator_servicos,
      input$fator_materiais
    )

  data("projeto_coleta_esgoto")
  app_state$modulo_orcamentario$resultado$coleta <<-
    calcula_precos_distribuicao(
      projeto_coleta_esgoto,
      sinapi,
      input$fator_servicos,
      input$fator_materiais
    )

  data("projeto_producao_agua")
  app_state$modulo_orcamentario$resultado$producao <<-
    calcula_preco_unidades_producao(
      projeto_producao_agua_unidades,
      sinapi,
      input$fator_insumo,
      input$fator_composicao
    )

  data("projeto_tratamento_esgoto")
  app_state$modulo_orcamentario$resultado$tratamento <<-
    calcula_preco_unidades_producao(
      projeto_tratamento_esgoto_unidades,
      sinapi,
      input$fator_insumo,
      input$fator_composicao
    )

  app_state$modulo_orcamentario$resultado$custo <<-
    calcula_custo_expansao(
      app_state$modulo_demografico$resultado,
      app_state$modulo_orcamentario$resultado$distribuicao,
      app_state$modulo_orcamentario$resultado$coleta,
      app_state$modulo_orcamentario$resultado$producao,
      app_state$modulo_orcamentario$resultado$tratamento,
      input$perda_agua
    )
}


orcamentario_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    distribuicao <-
      reactiveVal(app_state$modulo_orcamentario$resultado$distribuicao)
    coleta <-
      reactiveVal(app_state$modulo_orcamentario$resultado$coleta)
    producao <-
      reactiveVal(app_state$modulo_orcamentario$resultado$producao)
    tratamento <-
      reactiveVal(app_state$modulo_orcamentario$resultado$tratamento)
    custo <-
      reactiveVal(app_state$modulo_orcamentario$resultado$custo)

    observeEvent(input$rodar, {
      rlog::log_info("Running módulo orçamentário")
      rodar_modulo_orcamentario(input)

      distribuicao(app_state$modulo_orcamentario$resultado$distribuicao)
      coleta(app_state$modulo_orcamentario$resultado$coleta)
      producao(app_state$modulo_orcamentario$resultado$producao)
      tratamento(app_state$modulo_orcamentario$resultado$tratamento)
      custo(app_state$modulo_orcamentario$resultado$custo)

      save_modulo_orcamentario_state(input)
    })

    output$grafico_total <- renderPlotly({

    })

    output$distribuicao <- create_datatable(distribuicao)
    output$coleta <- create_datatable(coleta)
    output$producao <- create_datatable(producao)
    output$tratamento <- create_datatable(tratamento)
    output$custo <- create_datatable(custo)
  })
}

calcula_capacidade_instalada <- function(snis, custo) {
  # Quando não informada assume extensões igual a 0
  snis$AG005[is.na(snis$AG005)] <- 0
  snis$ES004[is.na(snis$ES004)] <- 0
  snis$AG006[is.na(snis$AG006)] <- 0
  snis$ES006[is.na(snis$ES006)] <- 0

  tabela <- dplyr::left_join(snis, custo, by = "codigo_municipio")
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_distribuicao = AG005 * 1e3 * preco_distribuicao_agua,
    capacidade_instalada_coleta = ES004 * 1e3 * preco_coleta_esgoto,
    capacidade_instalada_producao = AG006 * 1e3 * custo_relativo_producao,
    capacidade_instalada_tratamento = ES006 * 1e3 * custo_relativo_tratamento
  )
  return(tabela)
}

campos_reposicao <- function(capacidade, custo, reposicao) {
  return(list(
    capacidade = capacidade,
    custo = custo,
    reposicao = reposicao
  ))
}

rodar_modulo_financeiro <- function(input) {
  snis_data <- get_snis_data(input$snis, snis_fields)
  custo <- app_state$modulo_orcamentario$resultado$custo
  tabela <- calcula_capacidade_instalada(snis_data, custo)
  ano_inicial <- 2021
  ano_final <- 2033
  ano_corrente <- 2022
  vida_util <- 30

  vars <- list(
    campos_reposicao(
      "capacidade_instalada_distribuicao",
      "custo_expansao_distribuicao_agua",
      "reposicao_distribuicao_agua"
    ),
    campos_reposicao(
      "capacidade_instalada_coleta",
      "custo_expansao_coleta_esgoto",
      "reposicao_coleta_esgoto"
    ),
    campos_reposicao(
      "capacidade_instalada_producao",
      "custo_expansao_producao_agua",
      "reposicao_producao_agua"
    ),
    campos_reposicao(
      "capacidade_instalada_tratamento",
      "custo_expansao_tratamento_esgoto",
      "reposicao_tratamento_esgoto"
    )
  )

  for (var in vars) {
    tabela <-
      rsan::calcula_reposicao_parcial(
        tabela,
        var$capacidade,
        var$custo,
        var$reposicao,
        ano_inicial,
        ano_final,
        ano_corrente,
        vida_util
      )
  }

  tabela <- rsan::consolida_investimentos_agua_esgoto(tabela)
  tabela <- rsan::adiciona_pais(tabela)
  tabela <- rsan::adiciona_regiao(tabela)
  app_state$modulo_financeiro$resultado <<- tabela
  return(tabela)
}
financeiro_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resultado <-
      reactiveVal(app_state$modulo_financeiro$resultado)

    observeEvent(input$rodar, {
      rlog::log_info("Running módulo financeiro")
      rodar_modulo_financeiro(input)
      resultado(app_state$modulo_financeiro$resultado)
      save_modulo_financeiro_state(input)
    })

    output$tabela <- create_datatable(resultado)
  })
}
