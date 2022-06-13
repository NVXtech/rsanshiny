residuos_snis_fields <- c(
  "codigo_municipio", "POP_TOT", "POP_URB", "Estado",
  "CO164", "CO050", "CO119", "CS026",
  "CO054", "CO055", "CO056", "CO057", "CO058", "CO059",
  "CO063", "CO064", "CO065", "CO066", "CO067", "CO068",
  "CS001", "CS009", "CS050"
)

deficit_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_seletiva = pmax(populacao_urbana * (1.0 - atendimento_relativo_seletiva_urbano), 0)
  )
}

atendimento_relativo_coleta_seletiva <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    atendimento_relativo_seletiva_urbano = CS050 / POP_URB,
  )
}

investimento_coleta_seletiva <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_seletiva = deficit_coleta_seletiva / densidade_caminhoes_bau * valor
  )
}

mascara_coleta_seletiva <- function(tabela) {
  mask <- tabela$CS001 == "Sim"
  mask[is.na(mask)] <- FALSE
  return(mask)
}

investimento_coleta_comum <- function(tabela, valor) {
  tabela <- dplyr::mutate(
    tabela,
    investimento_coleta_comum = deficit_coleta_comum / densidade_caminhoes * valor
  )
}

deficit_coleta_comum <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    deficit_coleta_comum = pmax(meta_coleta / 100.0 * populacao_total - CO164, 0)
  )
}

numero_caminhoes <- function(tabela) {
  tabela <- dplyr::mutate(
    tabela,
    numero_caminhoes = CO054 + CO055 + CO056 + CO057 + CO058 + CO059
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

rodar_residuos <- function(input) {
  # parâmetros de entradar (TODO: colocar como parametros da funcao)
  ano <- input$ano
  valor_caminhao <- input$valor_caminhao
  valor_caminhao_bau <- input$valor_caminhao_bau

  # Consolida os dados de Unidades de Processamento (SNIS-prestadores)
  data(snis_rs)
  compostagem <- quantidade_compostagem_municipio(snis_rs$ano2020)

  # Consolidação dos dados para classificação
  tabela <- rsan::get_snis_data(input$snis, residuos_snis_fields)
  tabela <- rsan::consolida_populacao_snis(
    app_state$projecao$resultado, ano, tabela
  )
  tabela <- dplyr::left_join(tabela, compostagem, by = "codigo_municipio")
  tabela <- rsan::adiciona_pais(tabela)
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

  # Classificação e agrupamento por faixa populacional
  media <- rsan::media_por_estado_faixa(tabela)
  tabela <- rsan::soma_por_estado_faixa(tabela)
  tabela$densidade_caminhoes <- media$densidade_caminhoes
  tabela$densidade_caminhoes_bau <- media$densidade_caminhoes_bau

  # Coleta comum
  tabela <- meta_plansab_residuo(tabela)
  tabela <- deficit_coleta_comum(tabela)
  tabela <- investimento_coleta_comum(tabela, valor_caminhao)

  # Coleta seletiva
  tabela <- atendimento_relativo_coleta_seletiva(tabela)
  tabela <- deficit_coleta_seletiva(tabela)
  tabela <- investimento_coleta_seletiva(tabela, valor_caminhao_bau)
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
      # save_residuos_state(input)
    })

    output$tabela <- create_datatable(resultado)
    output$antes <- create_datatable(antes)
  })
}
