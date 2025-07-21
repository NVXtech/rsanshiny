#' Retorna lista de opções de dados do SNIS
#'
#' @return um vetor com os nomes
#' @export
get_snis_list <- function() {
  utils::data("snis", package = "rsan")
  snis <- get("snis")
  snis_choices <- as.list(snis$caminho)
  names(snis_choices) <- snis$nome
  return(snis_choices)
}


estrutura_anos_disponiveis <- function(componente, source) {
  base_dir <- file.path("dados", "base_calculo")
  pattern <- paste0(componente, "_", source, "_\\d{4}\\.csv")
  files <- list.files(base_dir, pattern = pattern, full.names = TRUE)
  # extract years from filename (e.g., agua_snis_2022.csv)
  years <- as.numeric(gsub(paste0(".*_", source, "_(\\d{4})\\.csv"), "\\1", files))
  return(years)
}


atendimento_anos_disponiveis <- function(fonte_nome) {
  if (fonte_nome == "censo") {
    return(c(2022, 2021))
  } else if (fonte_nome == "sinisa") {
    return(c(2023))
  } else if (fonte_nome == "pnadc") {
    return(c(2022, 2021))
  }
}

#' Retorna lista de opções de dados do SINAPI
#'
#' @return um vetor com os nomes
#' @export
get_sinapi_list <- function() {
  sinapi_choices <- rsan::get_sinapi_labels()
  output <- c()
  labels <- c()
  for (item in sinapi_choices) {
    split <- strsplit(item, "_")[[1]]
    year <- split[length(split)]
    label <- paste0("SINAPI ", year)
    labels <- c(labels, label)
    output <- c(output, item)
  }
  names(output) <- labels
  return(output)
}

anos_estrutura <- function(fonte_nome) {
  if (fonte_nome == "snis") {
    return(c(2022, 2021))
  } else if (fonte_nome == "sinisa") {
    return(c(2023))
  }
}
#' Gera interface para os parâmetros de cálculo
#'
#' @param ns é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
parametros_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Abastecimento de Água"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      ),
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("vida_util"),
          label = shiny::strong("Vida útil média dos ativos (anos)"),
          value = app_state$input$agua$vida_util,
          min = 1e-10,
          max = 1e10
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("meta_agua"),
          label = shiny::strong("Meta de atendimento para abastecimento de água (%)"),
          value = app_state$input$agua$meta_agua,
          min = 0,
          max = 100
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h3("Produção"),
        shiny::numericInput(
          inputId = ns("fator_composicao"),
          label = shiny::strong("Parcela de BDI para Composição (%)"),
          value = app_state$input$agua$fator_composicao,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("fator_insumo"),
          label = shiny::strong("Parcela de BDI para Insumos (%)"),
          value = app_state$input$agua$fator_insumo,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("perda_agua"),
          label = shiny::strong("Estimativa de Perda de água (%)"),
          value = app_state$input$agua$perda_agua,
          min = 0,
          max = 100
        )
      ),
      shiny::column(
        6,
        shiny::h3("Distribuição"),
        shiny::numericInput(
          inputId = ns("fator_servicos"),
          label = shiny::strong("Parcela de BDI de Serviços (%)"),
          value = app_state$input$agua$fator_servicos,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("fator_materiais"),
          label = shiny::strong("Parcela de BDI para Materiais (%)"),
          value = app_state$input$agua$fator_materiais,
          min = 0,
          max = 100
        )
      )
    ),
    shiny::h3("Rural"),
    shiny::fluidRow(
      shiny::column(
        6,
        shinyWidgets::autonumericInput(
          inputId = ns("custo_rural_individual"),
          label = shiny::strong("Custo rural individual (R$/dom)"),
          value = app_state$input$agua$custo_rural_individual,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        )
      ),
      shiny::column(
        6,
        shinyWidgets::autonumericInput(
          inputId = ns("custo_rural_individual_sem"),
          label = shiny::strong("Custo rural individual sem disponibilidade (R$/dom)"),
          value = app_state$input$agua$custo_rural_individual_sem,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Esgotamento Sanitário"), style = "display: inline-block;margin:0;"),
        shiny::titlePanel("Parâmetros"),
        shiny::numericInput(
          inputId = ns("meta_esgoto"),
          label = shiny::strong("Meta de atendimento para esgoto (%)"),
          value = app_state$input$esgoto$meta_esgoto,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("proporcao"),
          label = shiny::strong("Proporção entre a densidade esgoto e abastecimento (%)"),
          value = app_state$input$esgoto$proporcao,
          min = 0,
          max = 100
        ),
        shiny::column(
          6,
          shiny::h3("Coleta"),
          shiny::numericInput(
            inputId = ns("fator_servicos"),
            label = shiny::strong("Parcela de BDI para Serviços (%)"),
            value = app_state$input$esgoto$fator_servicos,
            min = 0,
            max = 100
          ),
          shiny::numericInput(
            inputId = ns("fator_materiais"),
            label = shiny::strong("Parcela de BDI para Materiais (%)"),
            value = app_state$input$esgoto$fator_materiais,
            min = 0,
            max = 100
          )
        ),
        shiny::column(
          6,
          shiny::h3("Tratamento"),
          shiny::numericInput(
            inputId = ns("fator_composicao"),
            label = shiny::strong("Parcela de BDI para Composição (%)"),
            value = app_state$input$esgoto$fator_composicao,
            min = 0,
            max = 100
          ),
          shiny::numericInput(
            inputId = ns("fator_insumo"),
            label = shiny::strong("Parcela de BDI para Insumos (%)"),
            value = app_state$input$esgoto$fator_insumo,
            min = 0,
            max = 100
          )
        )
      ),
      shiny::numericInput(
        inputId = ns("vida_util"),
        label = shiny::strong("Vida útil média dos ativos (anos)"),
        value = app_state$input$esgoto$vida_util,
        min = 1e-10,
        max = 1e10
      ),
      shiny::fluidRow(
        shiny::h4("Custo individual (R$/domicílio):"),
        esgoto_individual_input(ns, app_state$input$esgoto)
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Resíduos Sólidos"), style = "display: inline-block;margin:0;"),
        shiny::h3("Coleta Indiferenciada"),
        shinyWidgets::autonumericInput(
          inputId = ns("valor_caminhao"),
          label = shiny::strong("Valor do caminhão compactador em R$"),
          value = app_state$input$residuos$valor_caminhao,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::numericInput(
          inputId = ns("deprec_coleta_indiferenciada"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_coleta_indiferenciada,
          min = 0,
          max = 100
        ),
        shiny::h3("Coleta Seletiva"),
        shinyWidgets::autonumericInput(
          inputId = ns("valor_caminhao_bau"),
          label = shiny::strong("Valor caminhão bau em R$"),
          value = app_state$input$residuos$valor_caminhao_bau,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::numericInput(
          inputId = ns("deprec_coleta_seletiva"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_coleta_seletiva,
          min = 0,
          max = 100
        ),
        shiny::h3("Triagem"),
        shiny::column(
          6,
          shiny::numericInput(
            inputId = ns("vida_util_triagem"),
            label = shiny::strong("Vida útil unidades de triagem"),
            value = app_state$input$residuos$vida_util_triagem,
            min = 1e-3,
            max = 1e20
          ),
          shiny::numericInput(
            inputId = ns("deprec_triagem"),
            label = shiny::strong("Depreciação em %"),
            value = app_state$input$residuos$deprec_triagem,
            min = 0,
            max = 100
          )
        ),
        shiny::column(
          6,
          shiny::h4("Custo unidade de triagem por faixa populacional (R$/t):"),
          residuos_unidade_input(ns, "triagem", app_state$input$residuos)
        ),
        shiny::h3("Compostagem"),
        shiny::column(
          6,
          shiny::numericInput(
            inputId = ns("vida_util_compostagem"),
            label = shiny::strong("Vida útil unidades de compostagem"),
            value = app_state$input$residuos$vida_util_compostagem,
            min = 1e-3,
            max = 1e20
          ),
          shiny::numericInput(
            inputId = ns("deprec_compostagem"),
            label = shiny::strong("Depreciação em %"),
            value = app_state$input$residuos$deprec_compostagem,
            min = 0,
            max = 100
          )
        ),
        shiny::column(
          6,
          shiny::h4("Custo unidade de compostagem por faixa populacional (R$/t):"),
          residuos_unidade_input(ns, "compostagem", app_state$input$residuos)
        ),
        shiny::h3("Aterro"),
        shiny::column(
          6,
          shiny::numericInput(
            inputId = ns("vida_util_aterro"),
            label = shiny::strong("Vida útil unidades de aterro"),
            value = app_state$input$residuos$vida_util_aterro,
            min = 1e-3,
            max = 1e20
          ),
          shiny::numericInput(
            inputId = ns("deprec_aterro"),
            label = shiny::strong("Depreciação em %"),
            value = app_state$input$residuos$deprec_aterro,
            min = 0,
            max = 100
          ),
          shiny::numericInput(
            inputId = ns("tempo_finalizacao_attero"),
            label = shiny::strong("Tempo para finalizar a expansão dos aterros (ano)"),
            value = 3,
            min = 0,
            max = 100
          ),
          shiny::numericInput(
            inputId = ns("taxa_reducao_capacidade"),
            label = shiny::strong("Reducão dos custos de reposição (%/ano)"),
            value = 2.1,
            min = 0,
            max = 100
          )
        ),
        shiny::column(
          6,
          shiny::h4("Custo unidade de aterro por faixa populacional (R$/t):"),
          residuos_unidade_input(ns, "aterro", app_state$input$residuos)
        ),
        shiny::h3("Regionalização"),
        shiny::column(
          6,
          shinyWidgets::autonumericInput(
            inputId = ns("custo_transbordo"),
            label = shiny::strong("Custo tranbordo (R$)"),
            value = 857816.82,
            align = "left",
            decimalCharacter = ",",
            digitGroupSeparator = ".",
            decimalPlaces = 2
          ),
          shiny::selectInput(
            inputId = ns("cenario_regionalizacao"),
            label = shiny::strong("Cenário regionalizacao:"),
            choices = c("A - 0%" = "A", "B - Intermediário" = "B", "C - 100%" = "C"),
            selected = "A"
          ),
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::h1("Drenagem Urbana"),
        shiny::hr(),
        shiny::h3("Investimento por habitante"),
        shiny::selectInput(ns("modo"),
          label = shiny::strong("Modo de cálculo"),
          choices = list("Investimento per capita constante" = 1, "Regressão PMSB" = 2),
          selected = 2
        ),
        shiny::conditionalPanel(
          condition = "input.modo == 1",
          ns = ns,
          shinyWidgets::autonumericInput(
            inputId = ns("investimento_per_capita"),
            label = shiny::strong("Investimento em drenagem por habitante (R$/hab)"),
            value = app_state$input$drenagem$investimento_per_capita,
            align = "left",
            decimalCharacter = ",",
            digitGroupSeparator = ".",
            decimalPlaces = 2
          )
        ),
        shinyWidgets::autonumericInput(
          inputId = ns("custo_cadastro"),
          label = shiny::strong("Custo do cadastro técnico (R$/km²)"),
          value = app_state$input$drenagem$custo_cadastro,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::h3("Pesos dos Indicadores"),
        shinyWidgets::autonumericInput(
          inputId = ns("peso_pluviometria"),
          label = shiny::strong("Pluviometria [R$/(mm/ano)]"),
          value = app_state$input$drenagem$peso_pluviometria,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shinyWidgets::autonumericInput(
          inputId = ns("peso_densidade"),
          label = shiny::strong("Densidade urbana [R$/(hab/km²)]"),
          value = app_state$input$drenagem$peso_densidade,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shinyWidgets::autonumericInput(
          inputId = ns("peso_fisicas"),
          label = shiny::strong("Características físicas (R$/-)"),
          value = app_state$input$drenagem$peso_fisicas,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shinyWidgets::autonumericInput(
          inputId = ns("peso_infraestrutura"),
          label = shiny::strong("Infraestrutura (R$/-)"),
          value = app_state$input$drenagem$peso_infraestrutura,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shinyWidgets::autonumericInput(
          inputId = ns("peso_constante"),
          label = shiny::strong("Constante"),
          value = app_state$input$drenagem$peso_constante,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::h3("Reposição"),
        shinyWidgets::autonumericInput(
          inputId = ns("deprec_drenagem"),
          label = shiny::strong("Depreciação dos ativos em %"),
          value = app_state$input$drenagem$deprec_drenagem,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        )
      )
    )
  ) # end fluidPage
}
