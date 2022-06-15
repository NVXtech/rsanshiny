snis_fields <- c(
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

rodar_modulo_demografico <- function(input, projecao) {
  ano <- input$ano
  tabela <- rsan::get_snis_data(input$snis, snis_fields)
  tabela <- rsan::add_density(tabela)
  tabela <- rsan::fill_missing_density(tabela, c("densidade_distribuicao_agua", "densidade_producao_agua", "densidade_coleta_esgoto"))
  tabela <- rsan::consolida_populacao_snis(projecao, ano, tabela)
  tabela <- calculate_geografico(tabela, input$meta_agua, input$meta_esgoto, input$proporcao)
  return(tabela)
}

rodar_modulo_orcamentario <- function(input, demografico) {
  sinapi <- load_data(input$sinapi)
  data("projeto_distribuicao_agua")
  distribuicao <- calcula_precos_distribuicao(
    projeto_distribuicao_agua,
    sinapi,
    input$fator_servicos,
    input$fator_materiais
  )

  data("projeto_coleta_esgoto")
  coleta <- calcula_precos_distribuicao(
    projeto_coleta_esgoto,
    sinapi,
    input$fator_servicos,
    input$fator_materiais
  )

  data("projeto_producao_agua")
  producao <- calcula_preco_unidades_producao(
    projeto_producao_agua_unidades,
    sinapi,
    input$fator_insumo,
    input$fator_composicao
  )

  data("projeto_tratamento_esgoto")
  tratamento <- calcula_preco_unidades_producao(
    projeto_tratamento_esgoto_unidades,
    sinapi,
    input$fator_insumo,
    input$fator_composicao
  )

  custo <- calcula_custo_expansao(
    demografico,
    distribuicao,
    coleta,
    producao,
    tratamento,
    input$perda_agua
  )
  resultado <- list(
    distribuicao = distribuicao,
    coleta = coleta,
    producao = producao,
    tratamento = tratamento,
    custo = custo
  )
  return(resultado)
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

rodar_modulo_financeiro <- function(input, orcamentario) {
  snis_data <- get_snis_data(input$snis, snis_fields)
  custo <- orcamentario$custo
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
    tabela <- rsan::calcula_reposicao_parcial(
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
  return(tabela)
}

agua_esgoto_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$rodar, {
      rlog::log_info("Calculando investimento para agua esgoto")
      projecao <- app_state$projecao$resultado
      demografico <- rodar_modulo_demografico(input, projecao)
      orcamentario <- rodar_modulo_orcamentario(input, demografico)
      financeiro <- rodar_modulo_financeiro(input, orcamentario)
      save_agua_esgoto_state(input, financeiro)
    })
  })
}
