config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Configurações"), style = "display: inline-block;margin:0;"),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::sliderInput(
          inputId = ns("ano"),
          label = shiny::strong("Calcular necessidades para o ano de:"),
          min = 2020,
          max = 2050,
          value = app_state$input$geral$ano
        )
      ),
      shiny::column(
        12,
        shiny::titlePanel("Fonte de Dados"),
        shiny::actionButton(ns("atualizar"), icon = shiny::icon("sync"), label = "Atualizar")
      )
    )
  )
}
