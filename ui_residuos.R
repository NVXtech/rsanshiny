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

residuos_unidade_input <- function(ns, name, valores) {
  output <- list()
  for (i in seq.int(1, length(faixas))) {
    id <- sprintf("%s_faixa%s", name, i)
    output[[i]] <- list(numericInput(
      inputId = ns(id),
      label = strong(faixas[i]),
      value = valores[i],
      min = 0,
      max = 1e20
    ))
  }
  return(output)
}

residuos_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        h1(strong("Resíduos Sólidos"), style = "display: inline-block;margin:0;"),
        actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        downloadButton(ns("download"), "Exportar xlsx",style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    fluidRow(
      column(
        3,
        titlePanel("Fonte de Dados"),
        column(
          12,
          selectInput(
            inputId = ns("snis"),
            label = strong("Selecione o ano do SNIS"),
            choices = get_snis_list(),
            selected = app_state$demografico$snis
          ),
          numericInput(
            inputId = ns("ano"),
            label = strong("Calcular investimento para o ano de"),
            value = 2033,
            min = 2022,
            max = 2040
          )
        )
      ),
      column(
        9,
        titlePanel("Parâmetros"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Coleta Comum",
            numericInput(
              inputId = ns("valor_caminhao"),
              label = strong("Valor do caminhão compactador em R$"),
              value = 484709.23,
              min = 1000,
              max = 1e20
            ),
            numericInput(
              inputId = ns("deprec_coleta_comum"),
              label = strong("Depreciação em %"),
              value = 10,
              min = 0,
              max = 100
            )
          ),
          tabPanel(
            "Coleta Seletiva",
            numericInput(
              inputId = ns("valor_caminhao_bau"),
              label = strong("Valor caminhão bau em R$"),
              value = 336490.00,
              min = 1000,
              max = 1e20
            ),
            numericInput(
              inputId = ns("deprec_coleta_seletiva"),
              label = strong("Depreciação em %"),
              value = 10,
              min = 0,
              max = 100
            )
          ),
          tabPanel(
            "Triagem",
            column(
              6,
              numericInput(
                inputId = ns("vida_util_triagem"),
                label = strong("Vida útil unidades de triagem"),
                value = 20,
                min = 1e-3,
                max = 1e20
              ),
              numericInput(
                inputId = ns("deprec_triagem"),
                label = strong("Depreciação em %"),
                value = 5,
                min = 0,
                max = 100
              )
            ),
            column(
              6,
              h4("Custo unidade de triagem por faixa populacional (R$/t):"),
              residuos_unidade_input(ns, "triagem", valores_triagem)
            )
          ),
          tabPanel(
            "Compostagem",
            column(
              6,
              numericInput(
                inputId = ns("vida_util_compostagem"),
                label = strong("Vida útil unidades de compostagem"),
                value = 20,
                min = 1e-3,
                max = 1e20
              ),
              numericInput(
                inputId = ns("deprec_compostagem"),
                label = strong("Depreciação em %"),
                value = 5,
                min = 0,
                max = 100
              )
            ),
            column(
              6,
              h4("Custo unidade de compostagem por faixa populacional (R$/t):"),
              residuos_unidade_input(ns, "compostagem", valores_compostagem)
            )
          ),
          tabPanel(
            "Aterro",
            column(
              6,
              numericInput(
                inputId = ns("vida_util_aterro"),
                label = strong("Vida útil unidades de aterro"),
                value = 20,
                min = 1e-3,
                max = 1e20
              ),
              numericInput(
                inputId = ns("deprec_aterro"),
                label = strong("Depreciação em %"),
                value = 5,
                min = 0,
                max = 100
              ),
              numericInput(
                inputId = ns("tempo_finalizacao_attero"),
                label = strong("Tempo para finalizar a expansão dos aterros (ano)"),
                value = 3,
                min = 0,
                max = 100
              ),
              numericInput(
                inputId = ns("taxa_reducao_capacidade"),
                label = strong("Reducão dos custos de reposição (%/ano)"),
                value = 2.1,
                min = 0,
                max = 100
              )
            ),
            column(
              6,
              h4("Custo unidade de aterro por faixa populacional (R$/t):"),
              residuos_unidade_input(ns, "aterro", valores_aterro)
            )
          ),
        )
      )
    ),
    fluidRow(
      DT::dataTableOutput(ns("tabela")),
    ),
    fluidRow(
      hr(),
      h1("Antes"),
      DT::dataTableOutput(ns("antes")),
    )
  )
}
