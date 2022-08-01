config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  current <- rsan::get_last_month_and_year()
  mes_atual <- sprintf("%d", as.integer(current$month))
  ano_atual <- current$year
  anos_censo <- sort(rsan::get_censo_years(), decreasing=TRUE)
  anos_estimativa <- sort(rsan::load_estimativa_years(), decreasing=TRUE)

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
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("IBGE Censo", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar_censo"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("censo_ano"),
          label = shiny::strong("Selecione o ano:"),
          choices = anos_censo,
          selected = anos_censo[1]
        )
      )
    ),
    hr(),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("IBGE Estimativa", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar_estimativa"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("estimativa_ano"),
          label = shiny::strong("Selecione o ano:"),
          choices = anos_estimativa,
          selected = anos_estimativa[1]
        )
      )
    ),
    hr(),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("SINAPI", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar_sinapi"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("sinapi_ano"),
          label = shiny::strong("Selecione o ano:"),
          choices = ano_atual:2020,
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
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("SNIS-RS", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar_snis_rs"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("snis_rs_ano"),
          label = shiny::strong("Selecione o ano:"),
          choices = 2020:ano_atual,
          selected = ano_atual
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h3("SNIS-AP", style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("atualizar_snis_ap"), icon = shiny::icon("sync"), label = "Atualizar", style = "display: inline-block;margin-bottom:10px;")
      ),
      shiny::column(
        4,
        shiny::selectInput(
          inputId = ns("snis_ap_ano"),
          label = shiny::strong("Selecione o ano:"),
          choices = 2017:ano_atual,
          selected = ano_atual
        )
      )
    ),
  )
}
