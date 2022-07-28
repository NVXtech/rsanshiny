config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  current <- rsan::get_last_month_and_year()
  mes_atual <- current$month
  ano_atual <- current$year
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
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::titlePanel("Fonte de Dados"),
      ),
      shiny::column(
        12,
        shiny::h3("SINAPI", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    shiny::column(
      4,
      shiny::selectInput(
        inputId = ns("sinapi_ano"),
        label = shiny::strong("Selecione o ano:"),
        choices = ano:2020,
        selected = ano_atual
      )
    ),
    shiny::column(
      4,
      shiny::selectInput(
        inputId = ns("sinapi_mes"),
        label = shiny::strong("Selecione o mês:"),
        choices = 1:12,
        selected = mes_atual
      )
    )
  )
}
