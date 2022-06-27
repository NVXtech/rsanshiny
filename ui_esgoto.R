get_snis_list <- function() {
  data("snis")
  snis_choices <- as.list(snis$caminho)
  names(snis_choices) <- snis$nome
  return(snis_choices)
}

get_sinapi_list <- function() {
  data("sinapi")
  sinapi_choices <- as.list(sinapi$caminho)
  names(sinapi_choices) <- sinapi$nome
  return(sinapi_choices)
}

esgoto_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        h1(strong("Esgoto"), style = "display: inline-block;margin:0;"),
        actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
      )
    ),
    fluidRow(
      column(
        3,
        titlePanel("Fonte de Dados"),
        selectInput(
          inputId = ns("snis"),
          label = strong("Selecione o ano do SNIS"),
          choices = get_snis_list(),
          selected = app_state$demografico$snis
        ),
        selectInput(
          inputId = ns("sinapi"),
          label = strong("Selecione o ano e mês do SINAPI"),
          choices = get_sinapi_list(),
          selected = app_state$orcamentario$sinapi
        ),
        sliderInput(
          inputId = ns("ano"),
          strong("Realizar cálculo para o ano de:"),
          step = 1,
          min = 2021,
          max = 2040,
          value = 2033
        ),
      ),
      column(
        9,
        titlePanel("Parâmetros"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Módulo Demográfico",
            numericInput(
              inputId = ns("meta_esgoto"),
              label = strong("Meta de atendimento para esgoto (%)"),
              value = 90,
              min = 0,
              max = 100
            ),
            numericInput(
              inputId = ns("proporcao"),
              label = strong("Proporção entre a densidade esgoto e abastecimento (%)"),
              value = 80,
              min = 0,
              max = 100
            )
          ),
          tabPanel(
            "Módulo Orçamentário",
            fluidRow(
              column(
                6,
                h3("Coleta"),
                numericInput(
                  inputId = ns("fator_servicos"),
                  label = strong("Fator correção dos preços de Serviços (%)"),
                  value = 26,
                  min = 0,
                  max = 100
                ),
                numericInput(
                  inputId = ns("fator_materiais"),
                  label = strong("Fator correção dos preços de Materiais (%)"),
                  value = 18,
                  min = 0,
                  max = 100
                )
              ),
              column(
                6,
                h3("Tratamento"),
                numericInput(
                  inputId = ns("fator_composicao"),
                  label = strong("Fator correção dos preços de Composição (%)"),
                  value = 26,
                  min = 0,
                  max = 100
                ),
                numericInput(
                  inputId = ns("fator_insumo"),
                  label = strong("Fator correção dos preços de Insumos (%)"),
                  value = 18,
                  min = 0,
                  max = 100
                )
              )
            )
          ),
          tabPanel(
            "Módulo Financeiro",
            numericInput(
              inputId = ns("vida_util"),
              label = strong("Vida útil média dos ativos (anos)"),
              value = 30,
              min = 1e-10,
              max = 1e10
            ),
          ),
        )
      )
    )
  )
}
