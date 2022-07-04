config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        h1(strong("Configurações"), style = "display: inline-block;margin:0;"),
      )
    ),
    fluidRow(
      column(
        12,
        sliderInput(
          inputId = ns("ano"),
          label = strong("Calcular necessidades para o ano de:"),
          min = 2020,
          max = 2050,
          value = app_state$input$geral$ano)
      ),
      column(
        12,
        titlePanel("Fonte de Dados"),
        actionButton(ns("atualizar"), icon = icon("sync"), label = "Atualizar")
      )
    )
  )
}
