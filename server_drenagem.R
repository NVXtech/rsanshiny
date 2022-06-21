source("ui_helpers.R")

investimento_total_drenagem <- function(tabela) {
    # TODO: fazer capacidade intalada
    tabela <- dplyr::mutate(
        tabela,
        investimento_total = investimento_expansao + investimento_reposicao
    )
    return(tabela)
}

capacidade_instalada_drenagem <- function(tabela) {
    # TODO: fazer capacidade intalada
    tabela <- dplyr::mutate(
        tabela,
        capacidade_instalada = investimento_expansao * 0.0
    )
    return(tabela)
}

aplica_regressao_drenagem <- function(tabela, modelo) {
    tabela <- dplyr::mutate(
        tabela,
        modelo = stats::predict(modelo, tabela),
        investimento_modelo = modelo * GE006,
        investimento_expansao = investimento_modelo # TODO arrumar para trocar pelo investimento do plano
    )
    return(tabela)
}

regressao_drenagem <- function(plano) {
    # TODO: corrigir com investimento anterior
    plano <- dplyr::mutate(
        plano,
        densidade_investimento = investimento_corrigido / GE006
    )
    return(stats::lm(formula = densidade_investimento ~ pd, plano))
}

prepara_regressao <- function(plano, tabela) {
    tabela <- dplyr::select(tabela, "codigo_municipio", "pd", "GE006")
    plano <- dplyr::left_join(plano, tabela, by = "codigo_municipio")
}

corrige_plano_drenagem <- function(data) {
    data(plano_drenagem)
    igp <- rsan::get_igp()
    plano_drenagem <- dplyr::mutate(
        plano_drenagem,
        data_inicial = as.Date(paste0(as.character(ano_plano), "-06-30")),
        data_final = as.Date(data),
        taxa_igp = rsan::get_taxa_igp(igp, data_inicial, data_final), # fix get_taxa_igp
        investimento_corrigido = investimento * taxa_igp
    )
    return(plano_drenagem)
}

coeficiente_pd <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        pd = precipitacao_moda * densidade_urbana / 1e3
    )
    return(tabela)
}

precipitacao <- function(tabela) {
    data(pluviometria)
    pluviometria <- dplyr::select(
        pluviometria,
        codigo_municipio,
        precipitacao_moda
    )
    tabela <- dplyr::left_join(
        tabela,
        pluviometria,
        by = "codigo_municipio"
    )
    return(tabela)
}

densidade_urbana <- function(tabela) {
    tabela <- dplyr::mutate(
        tabela,
        densidade_urbana = GE006 / GE002
    )
    return(tabela)
}

rodar_drenagem <- function(input) {
    data("snis_ap")
    ano <- "2020"
    ano_inicial <- 2021
    ano_final <- 2033
    ano_corrente <- 2022
    depreciacao <- rsan::depreciacao_para_vida_util(input$deprec_drenagem)

    tabela <- snis_ap[[paste0("ano", ano)]]
    tabela <- densidade_urbana(tabela)
    tabela <- precipitacao(tabela)
    tabela <- coeficiente_pd(tabela)

    plano <- corrige_plano_drenagem("2021-12-01")
    plano <- prepara_regressao(plano, tabela)
    modelo <- regressao_drenagem(plano)
    tabela <- aplica_regressao_drenagem(tabela, modelo)
    tabela <- capacidade_instalada_drenagem(tabela)
    tabela <- rsan::calcula_reposicao_parcial(
        tabela,
        "capacidade_instalada",
        "investimento_expansao",
        "investimento_reposicao",
        ano_inicial,
        ano_final,
        ano_corrente,
        depreciacao
    )
    tabela <- investimento_total_drenagem(tabela)
    tabela <- rsan::adiciona_pais(tabela)
    print(modelo)
    print(plano)
    return(tabela)
}

drenagem_server <- function(id) {
    moduleServer(id, function(input, output, session) {
        resultado <- shiny::reactiveVal(app_state$drenagem$resultado)

        observeEvent(input$rodar, {
            rlog::log_info("Running solid waste module")
            app_state$drenagem$resultado <<- rodar_drenagem(input)
            resultado(app_state$drenagem$resultado)
            save_drenagem_state(input, app_state$drenagem$resultado)
        })

        output$tabela <- create_datatable(resultado)
    })
}
