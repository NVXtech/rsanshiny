residuos_ui <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      titlePanel(strong("Fonte de Dados")),
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
    hr(),
    fluidRow(
      titlePanel(strong("Parâmetros")),
      column(
        4, h3("Coleta Comum"),
        numericInput(
          inputId = ns("valor_caminhao"),
          label = strong("Valor do caminhão compactador em R$"),
          value = 484709.23,
          min = 1000,
          max = 1e20
        )
      ),
      column(
        4, h3("Coleta Seletiva"),
        numericInput(
          inputId = ns("valor_caminhao_bau"),
          label = strong("Valor caminhão bau em R$"),
          value = 336490.00,
          min = 1000,
          max = 1e20
        )
      ),
      column(4, h3("Triagem")),
      column(4, h3("Compostagem")),
      column(4, h3("Aterros")),
    ),
    hr(),
    fluidRow(
      column(
        12,
        actionButton(ns("rodar"), label = "Recalcular")
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
