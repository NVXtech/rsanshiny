esgoto_individual_input <- function(ns, input) {
  labels <- c(
    "com disponibilidade hídrica e média hab/domicílio entre 0 e 2",
    "com disponibilidade hídrica e média hab/domicílio entre 2 e 3",
    "com disponibilidade hídrica e média hab/domicílio entre 3 e 4",
    "com disponibilidade hídrica e média hab/domicílio entre 4 e 5",
    "com disponibilidade hídrica e média hab/domicílio entre 5 e 6",
    "com disponibilidade hídrica e média hab/domicílio entre 6 e 7",
    "com disponibilidade hídrica e média hab/domicílio entre 7 e 8",
    "com disponibilidade hídrica e média hab/domicílio maior que 8",
    "sem disponibilidade hídrica e média hab/domicílio entre 0 e 5",
    "sem disponibilidade hídrica e média hab/domicílio maior que 5"
  )
  output <- list()
  for (i in seq.int(1, length(labels))) {
    value_id <- sprintf("custo_individual_esgoto_faixa%s", i)
    id <- paste0("esgoto-", value_id)
    output[[i]] <- list(shiny::column(6, shinyWidgets::autonumericInput(
      inputId = ns(id),
      label = shiny::strong(labels[i]),
      value = input[[value_id]],
      align = "left",
      decimalCharacter = ",",
      digitGroupSeparator = ".",
      decimalPlaces = 2
    )))
  }
  return(output)
}

faixas <- c(
  "até 10 mil habitantes",
  "de 10.001 a 30 mil habitantes",
  "fe 30.001 a 100 mil habitantes",
  "de 100.001 a 250 mil habitantes",
  "de 250.001 a 1 milhão habitantes",
  "de 1.000.001 a 4 milhões habitantes",
  "acima de 4 milhões"
)

residuos_unidade_input <- function(ns, name, input) {
  output <- list()
  for (i in seq.int(1, length(faixas))) {
    value_id <- sprintf("%s_faixa%s", name, i)
    id <- paste0("residuos-", value_id)
    output[[i]] <- list(shinyWidgets::autonumericInput(
      inputId = ns(id),
      label = shiny::strong(faixas[i]),
      value = input[[value_id]],
      align = "left",
      decimalCharacter = ",",
      digitGroupSeparator = ".",
      decimalPlaces = 2
    ))
  }
  return(output)
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
    # Abastecimento de Água
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Abastecimento de Água"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("salvar"), icon = shiny::icon("save"), label = "Salvar Parâmetros", style = "display: inline-block;margin-bottom:10px;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
      ),
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("agua-vida_util"),
          label = shiny::strong("Vida útil média dos ativos (anos)"),
          value = app_state$input$agua$vida_util,
          min = 1e-10,
          max = 1e10
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("agua-meta_agua"),
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
          inputId = ns("agua-fator_composicao"),
          label = shiny::strong("Parcela de BDI para Composição (%)"),
          value = app_state$input$agua$fator_composicao,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("agua-fator_insumo"),
          label = shiny::strong("Parcela de BDI para Insumos (%)"),
          value = app_state$input$agua$fator_insumo,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("agua-perda_agua"),
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
          inputId = ns("agua-fator_servicos"),
          label = shiny::strong("Parcela de BDI de Serviços (%)"),
          value = app_state$input$agua$fator_servicos,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("agua-fator_materiais"),
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
          inputId = ns("agua-custo_rural_individual"),
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
          inputId = ns("agua-custo_rural_individual_sem"),
          label = shiny::strong("Custo rural individual sem disponibilidade (R$/dom)"),
          value = app_state$input$agua$custo_rural_individual_sem,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        )
      ),
    ),
    # Esgotamento Sanitário
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Esgotamento Sanitário"), style = "display: inline-block;margin:0;"),
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("esgoto-meta_esgoto"),
          label = shiny::strong("Meta de atendimento para esgoto (%)"),
          value = app_state$input$esgoto$meta_esgoto,
          min = 0,
          max = 100
        )
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("esgoto-proporcao"),
          label = shiny::strong("Proporção entre a densidade esgoto e abastecimento (%)"),
          value = app_state$input$esgoto$proporcao,
          min = 0,
          max = 100
        )
      ),
      shiny::column(
        12,
        shiny::numericInput(
          inputId = ns("esgoto-vida_util"),
          label = shiny::strong("Vida útil média dos ativos (anos)"),
          value = app_state$input$esgoto$vida_util,
          min = 1e-10,
          max = 1e10
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h3("Coleta"),
        shiny::numericInput(
          inputId = ns("esgoto-fator_servicos"),
          label = shiny::strong("Parcela de BDI para Serviços (%)"),
          value = app_state$input$esgoto$fator_servicos,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("esgoto-fator_materiais"),
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
          inputId = ns("esgoto-fator_composicao"),
          label = shiny::strong("Parcela de BDI para Composição (%)"),
          value = app_state$input$esgoto$fator_composicao,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("esgoto-fator_insumo"),
          label = shiny::strong("Parcela de BDI para Insumos (%)"),
          value = app_state$input$esgoto$fator_insumo,
          min = 0,
          max = 100
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("Rural"),
        shiny::h4("Custo individual (R$/domicílio):"),
        shiny::fluidRow(
          esgoto_individual_input(ns, app_state$input$esgoto)
        )
      )
    ),
    # Resíduos Sólidos
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Resíduos Sólidos"), style = "display: inline-block;margin:0;"),
      )
    ),
    shiny::hr(),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::h3("Coleta Indiferenciada"),
        shinyWidgets::autonumericInput(
          inputId = ns("residuos-valor_caminhao"),
          label = shiny::strong("Valor do caminhão compactador em R$"),
          value = app_state$input$residuos$valor_caminhao,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::numericInput(
          inputId = ns("residuos-deprec_coleta_indiferenciada"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_coleta_indiferenciada,
          min = 0,
          max = 100
        ),
      ),
      shiny::column(
        6,
        shiny::h3("Coleta Seletiva"),
        shinyWidgets::autonumericInput(
          inputId = ns("residuos-valor_caminhao_bau"),
          label = shiny::strong("Valor caminhão bau em R$"),
          value = app_state$input$residuos$valor_caminhao_bau,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::numericInput(
          inputId = ns("residuos-deprec_coleta_seletiva"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_coleta_seletiva,
          min = 0,
          max = 100
        ),
      ),
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::h3("Triagem")),
      shiny::column(
        6,
        shiny::h4("Custo unidade de triagem por faixa populacional (R$/t):"),
        residuos_unidade_input(ns, "triagem", app_state$input$residuos)
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("residuos-vida_util_triagem"),
          label = shiny::strong("Vida útil unidades de triagem"),
          value = app_state$input$residuos$vida_util_triagem,
          min = 1e-3,
          max = 1e20
        ),
        shiny::numericInput(
          inputId = ns("residuos-deprec_triagem"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_triagem,
          min = 0,
          max = 100
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::h3("Compostagem")),
      shiny::column(
        6,
        shiny::h4("Custo unidade de compostagem por faixa populacional (R$/t):"),
        residuos_unidade_input(ns, "compostagem", app_state$input$residuos)
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("residuos-vida_util_compostagem"),
          label = shiny::strong("Vida útil unidades de compostagem"),
          value = app_state$input$residuos$vida_util_compostagem,
          min = 1e-3,
          max = 1e20
        ),
        shiny::numericInput(
          inputId = ns("residuos-deprec_compostagem"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_compostagem,
          min = 0,
          max = 100
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::h3("Aterro")),
      shiny::column(
        6,
        shiny::h4("Custo unidade de aterro por faixa populacional (R$/t):"),
        residuos_unidade_input(ns, "aterro", app_state$input$residuos)
      ),
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("residuos-vida_util_aterro"),
          label = shiny::strong("Vida útil unidades de aterro"),
          value = app_state$input$residuos$vida_util_aterro,
          min = 1e-3,
          max = 1e20
        ),
        shiny::numericInput(
          inputId = ns("residuos-deprec_aterro"),
          label = shiny::strong("Depreciação em %"),
          value = app_state$input$residuos$deprec_aterro,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("residuos-tempo_finalizacao_attero"),
          label = shiny::strong("Tempo para finalizar a expansão dos aterros (ano)"),
          value = 3,
          min = 0,
          max = 100
        ),
        shiny::numericInput(
          inputId = ns("residuos-taxa_reducao_capacidade"),
          label = shiny::strong("Reducão dos custos de reposição (%/ano)"),
          value = 2.1,
          min = 0,
          max = 100
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(12, shiny::h3("Regionalização")),
      shiny::column(
        6,
        shinyWidgets::autonumericInput(
          inputId = ns("residuos-custo_transbordo"),
          label = shiny::strong("Custo tranbordo (R$)"),
          value = 857816.82,
          align = "left",
          decimalCharacter = ",",
          digitGroupSeparator = ".",
          decimalPlaces = 2
        ),
        shiny::selectInput(
          inputId = ns("residuos-cenario_regionalizacao"),
          label = shiny::strong("Cenário regionalizacao:"),
          choices = c("A - 0%" = "A", "B - Intermediário" = "B", "C - 100%" = "C"),
          selected = "A"
        ),
      ),
    ),
    # Drenagem Urbana
    shiny::fluidRow(
      shiny::column(
        9,
        shiny::h1("Drenagem Urbana"),
        shiny::hr(),
        shiny::column(
          6,
          shiny::h3("Reposição"),
          shinyWidgets::autonumericInput(
            inputId = ns("drenagem-deprec_drenagem"),
            label = shiny::strong("Depreciação dos ativos em %"),
            value = app_state$input$drenagem$deprec_drenagem,
            align = "left",
            decimalCharacter = ",",
            digitGroupSeparator = ".",
            decimalPlaces = 2
          ),
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
              inputId = ns("drenagem-investimento_per_capita"),
              label = shiny::strong("Investimento em drenagem por habitante (R$/hab)"),
              value = app_state$input$drenagem$investimento_per_capita,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            )
          ),
          shinyWidgets::autonumericInput(
            inputId = ns("drenagem-custo_cadastro"),
            label = shiny::strong("Custo do cadastro técnico (R$/km²)"),
            value = app_state$input$drenagem$custo_cadastro,
            align = "left",
            decimalCharacter = ",",
            digitGroupSeparator = ".",
            decimalPlaces = 2
          ),
        ),
        shiny::conditionalPanel(
          condition = "input.modo == 2",
          ns = ns,
          shiny::column(
            6,
            shiny::h3("Pesos dos Indicadores"),
            shinyWidgets::autonumericInput(
              inputId = ns("drenagem-peso_pluviometria"),
              label = shiny::strong("Pluviometria [R$/(mm/ano)]"),
              value = app_state$input$drenagem$peso_pluviometria,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("drenagem-peso_densidade"),
              label = shiny::strong("Densidade urbana [R$/(hab/km²)]"),
              value = app_state$input$drenagem$peso_densidade,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("drenagem-peso_fisicas"),
              label = shiny::strong("Características físicas (R$/-)"),
              value = app_state$input$drenagem$peso_fisicas,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("drenagem-peso_infraestrutura"),
              label = shiny::strong("Infraestrutura (R$/-)"),
              value = app_state$input$drenagem$peso_infraestrutura,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("drenagem-peso_constante"),
              label = shiny::strong("Constante"),
              value = app_state$input$drenagem$peso_constante,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            )
          )
        )
      )
    )
  ) # end fluidPage
}
