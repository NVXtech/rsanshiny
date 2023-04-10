faixas <- c(
  "até 10 mil habitantes",
  "de 10.001 a 30 mil habitantes",
  "fe 30.001 a 100 mil habitantes",
  "de 100.001 a 250 mil habitantes",
  "de 250.001 a 1 milhão habitantes",
  "de 1.000.001 a 4 milhões habitantes",
  "acima de 4 milhões"
)

valores_aterro <- c(
  43.24,
  19.45,
  23.78,
  23.78,
  19.45,
  8.65,
  8.65
)

valores_compostagem <- c(
  18.37,
  5.4,
  6.92,
  6.92,
  11.89,
  8,
  8
)

valores_triagem <- c(
  70.25,
  34.58,
  37.82,
  37.82,
  23.78,
  12.97,
  12.97
)

residuos_unidade_input <- function(ns, name, input) {
  output <- list()
  for (i in seq.int(1, length(faixas))) {
    id <- sprintf("%s_faixa%s", name, i)
    output[[i]] <- list(shinyWidgets::autonumericInput(
      inputId = ns(id),
      label = shiny::strong(faixas[i]),
      value = input[[id]],
      align = "left",
      decimalCharacter = ",",
      digitGroupSeparator = ".",
      decimalPlaces = 2
    ))
  }
  return(output)
}

#' Title
#'
#' @param ns é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
residuos_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  params <- app_state$input$residuos
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Resíduos Sólidos"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::titlePanel("Fonte de Dados"),
        shiny::column(
          12,
          shiny::selectInput(
            inputId = ns("snis"),
            label = shiny::strong("Selecione o ano do SNIS"),
            choices = get_snis_list(),
            selected = app_state$input$residuos$snis
          )
        ),
        shiny::column(
          12,
          shiny::selectInput(
            inputId = ns("snis_rs"),
            label = shiny::strong("Selecione o ano do SNIS-RS Unidades Producao"),
            choices = rsan::get_snis_rs_list(),
            selected = app_state$input$residuos$snis_rs
          )
        )
      ),
      shiny::column(
        9,
        shiny::titlePanel("Parâmetros"),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Coleta Indiferenciada",
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
            )
          ),
          shiny::tabPanel(
            "Coleta Seletiva",
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
            )
          ),
          shiny::tabPanel(
            "Triagem",
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
              residuos_unidade_input(ns, "triagem", params)
            )
          ),
          shiny::tabPanel(
            "Compostagem",
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
              residuos_unidade_input(ns, "compostagem", params)
            )
          ),
          shiny::tabPanel(
            "Aterro",
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
              residuos_unidade_input(ns, "aterro", params)
            )
          ),
          shiny::tabPanel(
            "Regionalização",
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
          ),
        )
      )
    ),
    shiny::fluidRow(
      DT::dataTableOutput(ns("tabela")),
    )
  )
}
