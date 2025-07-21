config_ui <- function(id, app_state) {
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
        shiny::h1(shiny::strong("Fonte de Dados")),
      ),
    ),
    shiny::column(
      3,
      shiny::titlePanel("Abastecimento de Água"),
      shiny::selectInput(
        inputId = ns("fonte_nome"),
        label = shiny::strong("Selecione Fonte para Estruturas"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$agua$fonte_nome
      ),
      shiny::uiOutput(ns("fonte_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$agua$atendimento
      ),
      shiny::uiOutput(ns("atendimento_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$orcamentario$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Esgoto Sanitário"),
      shiny::selectInput(
        inputId = ns("snis"),
        label = shiny::strong("Selecione o ano do SNIS"),
        choices = get_snis_list(),
        selected = app_state$input$esgoto$snis
      ),
      shiny::selectInput(
        inputId = ns("fonte_nome"),
        label = shiny::strong("Selecione a Fonte"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$esgoto$fonte_nome
      ),
      shiny::selectInput(
        inputId = ns("fonte_ano"),
        label = shiny::strong("Selecione o ano da Fonte"),
        choices = c(2022, 2023),
        selected = app_state$input$esgoto$fonte_nome
      ),
      shiny::selectInput(
        inputId = ns("atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$agua$atendimento
      ),
      shiny::selectInput(
        inputId = ns("atendimento_ano"),
        label = shiny::strong("Selecione o ano para o atendimento"),
        choices = c(2022),
        selected = app_state$input$agua$atendimento_ano
      ),
      shiny::selectInput(
        inputId = ns("sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$input$esgoto$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Residuos Sólidos Urbanos"),
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
      3,
      shiny::titlePanel("Drenagem Urbana"),
      shiny::column(
        12,
        shiny::selectInput(
          inputId = ns("snis_ap"),
          label = shiny::strong("Selecione o ano do SNIS - Águas Pluviais"),
          choices = rsan::get_snis_ap_list(),
          selected = app_state$input$drenagem$snis_ap
        )
      )
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
    hr(),
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
    hr(),
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
    hr(),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Configurações Gerais"), style = "display: inline-block;margin:0;"),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::numericInput(
          inputId = ns("ano"),
          label = shiny::strong("Calcular necessidades para o ano de:"),
          min = 2020,
          max = 2050,
          step = 1,
          value = app_state$input$geral$ano
        )
      ),
    )
  )
}
