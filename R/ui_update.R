update_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  current <- rsan::get_last_month_and_year()
  mes_atual <- sprintf("%d", as.integer(current$month))
  ano_atual <- current$year

  anos_censo <- tryCatch(
    rsan::get_censo_years(),
    error = function(e) {
      c("2010")
    }
  )
  anos_censo <- sort(anos_censo, decreasing = TRUE)

  anos_estimativa <- tryCatch(
    rsan::load_estimativa_years(),
    error = function(e) {
      c("2021")
    }
  )
  anos_estimativa <- sort(anos_estimativa, decreasing = TRUE)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Atualização de Dados")),
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
  )
}
