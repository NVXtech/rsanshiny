source("ui_helpers.R")

residuos_snis_fields <- c(
  "codigo_municipio", "POP_TOT", "POP_URB", "Estado",
  "CO164", "CO050", "CO119", "CS026",
  "CO054", "CO055", "CO056", "CO057", "CO058", "CO059",
  "CO063", "CO064", "CO065", "CO066", "CO067", "CO068",
  "CS001", "CS009", "CS050"
)

preco_unidade_faixa <- function(tabela, precos) {
  tabela <- dplyr::left_join(
    tabela,
    precos,
    by = "faixa"
  )
}

investimento_residuos_total <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_reposicao =  reposicao_aterro + reposicao_compostagem + reposicao_triagem + reposicao_coleta_comum + reposicao_coleta_seletiva,
    investimento_expansao = investimento_aterro + investimento_compostagem + investimento_triagem + investimento_coleta_comum + investimento_coleta_seletiva,
    investimento_total = investimento_reposicao + investimento_expansao
  )
}

# ATERRO
investimento_aterro <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_aterro =  total_residuos_projecao * preco_unidade_aterro * vida_util
  )
}

capacidade_instalada_aterro <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_aterro =  total_residuos * preco_unidade_aterro * vida_util
  )
}

# TRIAGEM
demanda_triagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_triagem = meta_reaproveitamento / 100.0 * total_residuos_projecao - pmax(CS026, 0.0),
  )
}

investimento_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_triagem = demanda_triagem * preco_unidade_triagem * vida_util
  )
}

capacidade_instalada_triagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_triagem = meta_reaproveitamento / 100.0 * total_residuos * preco_unidade_triagem * vida_util
  )
}

# COMPOSTAGEM
demanda_compostagem <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    demanda_compostagem = meta_compostagem / 100.0 * total_residuos_projecao - pmax(quantidade_compostagem, 0.0),
  )
}

investimento_compostagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_compostagem = demanda_compostagem * preco_unidade_compostagem * vida_util
  )
}

capacidade_instalada_compostagem <- function(tabela, vida_util) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_compostagem = meta_compostagem / 100.0 * total_residuos * preco_unidade_compostagem * vida_util
  )
}

# COLETA SELETIVA

densidade_caminhoes_bau <- function(tabela) {
  # TODO: avaliar como foi preenchido na planilha de investimento
  # solucao usar tabela fixa?
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes_bau = ifelse(
      numero_caminhoes_bau > 0,
      CS050 / numero_caminhoes_bau,
      NA
    )
  )
}

preenche_caminhoes_bau <- function(tabela, tabela_densidade, campo_densidade, campo_faixa = "faixa") {
  tabela_densidade <- dplyr::select(tabela_densidade, campo_densidade, campo_faixa)
  tabela <- dplyr::select(tabela, -campo_densidade)
  tabela <- dplyr::left_join(tabela, tabela_densidade, by = campo_faixa)
}

mascara_coleta_seletiva <- function(tabela) {
  mask <- tabela$CS001 == "Sim"
  mask[is.na(mask)] <- FALSE
  return(mask)
}

atendimento_relativo_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_seletiva_urbano = CS050 / POP_URB,
  )
}

deficit_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_seletiva = pmax(populacao_urbana * (1.0 - atendimento_relativo_seletiva_urbano), 0)
  )
}

investimento_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_seletiva = deficit_coleta_seletiva / densidade_caminhoes_bau * valor
  )
}

capacidade_instalada_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_seletiva = POP_URB * (1.0 - atendimento_relativo_seletiva_urbano) / densidade_caminhoes_bau * valor
  )
}

# COLETA COMUM
numero_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes = CO054 + CO055 + CO056 + CO057 + CO058 + CO059
  )
}

deficit_coleta_comum <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_comum = pmax(meta_coleta / 100.0 * populacao_total - CO164, 0),
  )
}

investimento_coleta_comum <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_comum = deficit_coleta_comum / densidade_caminhoes * valor
  )
}

capacidade_instalada_coleta_comum <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    capacidade_instalada_coleta_comum = CO164 / densidade_caminhoes * valor
  )
}



numero_caminhoes_bau <- function(tabela) {
  tabela$mask <- mascara_coleta_seletiva(tabela)
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes_bau = ifelse(mask, CO063 + CO064 + CO065 + CO066 + CO067 + CO068, NA)
  )
  tabela <- dplyr::select(
    tabela,
    -mask
  )
}

densidade_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    densidade_caminhoes = ifelse(
      numero_caminhoes > 0,
      CO164 / numero_caminhoes,
      NA
    )
  )
}



meta_plansab_residuo <- function(tabela) {
  data("plansab")
  tabela <- dplyr::left_join(
    tabela,
    plansab,
    by = "regiao"
  )
}

atendimento_relativo_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_total = CO164 / POP_TOT,
    atendimento_relativo_urbano = CO050 / POP_URB,
    atendimento_relativo_rural = (CO164 - CO050) / (POP_TOT - POP_URB)
  )
}

geracao_residuos <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    taxa_geracao_residuos = CO119 / CO164,
    total_residuos = taxa_geracao_residuos * POP_TOT,
    total_residuos_projecao = taxa_geracao_residuos * populacao_total,
    percentual_recuperado = CS009 / CO119
  )
}

tabela_preco_unidade_residuos <- function(input, name) {
  faixa <- seq.int(1, 7)
  precos <- c()
  for (i in 1:7) {
    id <- sprintf("%s_faixa%s", name, i)
    precos <- c(precos, input[[id]])
  }
  output <- dplyr::tibble(faixa, precos)
  colnames(output)[colnames(output) == "precos"] <- sprintf("preco_unidade_%s", name)
  return(output)
}

rodar_residuos <- function(input) {
  # parâmetros de entradar (TODO: colocar como parametros da funcao)
  ano <- input$ano
  valor_caminhao <- input$valor_caminhao
  valor_caminhao_bau <- input$valor_caminhao_bau
  preco_unidade_aterro <- tabela_preco_unidade_residuos(input, "aterro")
  vida_util_aterro <- input$vida_util_aterro
  preco_unidade_compostagem <- tabela_preco_unidade_residuos(input, "compostagem")
  vida_util_compostagem <- input$vida_util_compostagem
  preco_unidade_triagem <- tabela_preco_unidade_residuos(input, "triagem")
  vida_util_triagem <- input$vida_util_triagem
  ano_inicial <- 2021
  ano_final <- 2033
  ano_corrente <- 2022

  # Consolida os dados de Unidades de Processamento (SNIS-prestadores)
  data(snis_rs)
  compostagem <- rsan::quantidade_compostagem_municipio(snis_rs$ano2020)

  # Consolidação dos dados para classificação
  tabela <- rsan::get_snis_data(input$snis, residuos_snis_fields)
  tabela <- rsan::consolida_populacao_snis(
    app_state$projecao$resultado, ano, tabela
  )
  tabela <- dplyr::left_join(tabela, compostagem, by = "codigo_municipio")
  tabela <- rsan::adiciona_pais(tabela)
  tabela <- rsan::adiciona_estado(tabela)
  tabela <- rsan::adiciona_regiao(tabela)

  # Dados coleta comum
  tabela <- atendimento_relativo_residuos(tabela)
  tabela <- geracao_residuos(tabela)
  tabela <- numero_caminhoes(tabela)
  tabela <- densidade_caminhoes(tabela)

  # Dados coleta seletiva
  tabela <- numero_caminhoes_bau(tabela)
  tabela <- densidade_caminhoes_bau(tabela)

  limites <- as.integer(c(0, 10e3, 30e3, 100e3, 250e3, 1e6, 4e6))
  tabela <- rsan::classifica_faixa_populacional(tabela, limites)
  antes <- tabela

  # Agrupamento por regiao e faixa populacional
  media_regiao <- rsan::media_por_estado_faixa(tabela, campo_estado = "regiao")
  soma_regiao <- rsan::soma_por_estado_faixa(tabela, campo_estado = "regiao")

  # Agrupamento por faixa
  media_faixa <- rsan::media_por_faixa(tabela)
  soma_faixa <- rsan::soma_por_faixa(tabela)
  soma_faixa <- densidade_caminhoes_bau(soma_faixa)

  # Preenchimentos

  # Arupamento por estado e faixa populacional
  media <- rsan::media_por_estado_faixa(tabela)
  tabela <- rsan::soma_por_estado_faixa(tabela)
  tabela$densidade_caminhoes <- media$densidade_caminhoes
  tabela$densidade_caminhoes_bau <- media$densidade_caminhoes_bau

  # Coleta comum
  tabela <- meta_plansab_residuo(tabela)
  tabela <- deficit_coleta_comum(tabela)
  tabela <- investimento_coleta_comum(tabela, valor_caminhao)
  tabela <- capacidade_instalada_coleta_comum(tabela, valor_caminhao)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_comum",
    "investimento_coleta_comum",
    "reposicao_coleta_comum",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_comum)
  )

  # Coleta seletiva
  tabela <- preenche_caminhoes_bau(tabela, soma_faixa, campo_densidade = "densidade_caminhoes_bau")
  tabela <- atendimento_relativo_coleta_seletiva(tabela)
  tabela <- deficit_coleta_seletiva(tabela)
  tabela <- investimento_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- capacidade_instalada_coleta_seletiva(tabela, valor_caminhao_bau)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_coleta_seletiva",
    "investimento_coleta_seletiva",
    "reposicao_coleta_seletiva",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_coleta_seletiva)
  )

  # Compostagem
  tabela <- demanda_compostagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_compostagem)
  tabela <- investimento_compostagem(tabela, vida_util_compostagem)
  tabela <- capacidade_instalada_compostagem(tabela, vida_util_compostagem)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_compostagem",
    "investimento_compostagem",
    "reposicao_compostagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_compostagem)
  )

  # Aterro
  tabela <- preco_unidade_faixa(tabela, preco_unidade_aterro)
  tabela <- investimento_aterro(tabela, vida_util_aterro)
  tabela <- capacidade_instalada_aterro(tabela, vida_util_aterro)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_aterro",
    "investimento_aterro",
    "reposicao_aterro",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_aterro)
  )

  # Triagem
  tabela <- demanda_triagem(tabela)
  tabela <- preco_unidade_faixa(tabela, preco_unidade_triagem)
  tabela <- investimento_triagem(tabela, vida_util_triagem)
  tabela <- capacidade_instalada_triagem(tabela, vida_util_triagem)
  tabela <- rsan::calcula_reposicao_parcial(
    tabela,
    "capacidade_instalada_triagem",
    "investimento_triagem",
    "reposicao_triagem",
    ano_inicial,
    ano_final,
    ano_corrente,
    rsan::depreciacao_para_vida_util(input$deprec_triagem)
  )

  tabela <- investimento_residuos_total(tabela)
  resultado <- tabela
  return(list("resultado" = resultado, "antes" = antes))
}

residuos_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    resultado <- shiny::reactiveVal(app_state$residuos$resultado$resultado)
    antes <- shiny::reactiveVal(app_state$residuos$resultado$antes)

    observeEvent(input$rodar, {
      rlog::log_info("Running solid waste module")
      app_state$residuos$resultado <<- rodar_residuos(input)
      resultado(app_state$residuos$resultado$resultado)
      antes(app_state$residuos$resultado$antes)
      save_residuos_state(input)
    })

    output$tabela <- create_datatable(resultado)
    output$antes <- create_datatable(antes)
  })
}
