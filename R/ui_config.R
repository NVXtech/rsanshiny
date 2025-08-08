config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    #' Configurações Gerais
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Configurações Gerais"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("salvar"), icon = shiny::icon("save"), label = "Salvar Parâmetros", style = "display: inline-block;margin-bottom:10px;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("geral-ano_corrente"),
          label = shiny::strong("Calcular necessidades a partir do ano:"),
          min = 2010,
          max = 2050,
          step = 1,
          value = app_state$input$geral$ano_corrente
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("geral-ano"),
          label = shiny::strong("Calcular necessidades até o ano:"),
          min = 2020,
          max = 2050,
          step = 1,
          value = app_state$input$geral$ano
        )
      ),
    ),
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
        inputId = ns("agua-fonte_nome"),
        label = shiny::strong("Selecione Fonte para Estruturas"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$agua$fonte_nome
      ),
      shiny::uiOutput(ns("agua_fonte_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("agua-atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$agua$atendimento
      ),
      shiny::uiOutput(ns("agua_atendimento_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("agua-sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$orcamentario$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Esgoto Sanitário"),
      shiny::selectInput(
        inputId = ns("esgoto-fonte_nome"),
        label = shiny::strong("Selecione a Fonte"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$esgoto$fonte_nome
      ),
      shiny::uiOutput(ns("esgoto_fonte_ano")),
      shiny::selectInput(
        inputId = ns("esgoto-atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$esgoto$atendimento
      ),
      shiny::uiOutput(ns("esgoto_atendimento_ano")),
      shiny::selectInput(
        inputId = ns("esgoto-sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$input$esgoto$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Residuos Sólidos Urbanos"),
      shiny::selectInput(
        inputId = ns("residuos-fonte_nome"),
        label = shiny::strong("Selecione a Fonte"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$residuos$fonte_nome
      ),
      shiny::uiOutput(ns("residuos_fonte_ano")),
    ),
    shiny::column(
      3,
      shiny::titlePanel("Drenagem Urbana"),
      shiny::column(
        12,
        shiny::selectInput(
          inputId = ns("drenagem-fonte_nome"),
          label = shiny::strong("Selecione Fonte para Estruturas"),
          choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
          selected = app_state$input$drenagem$fonte_nome
        ),
        shiny::uiOutput(ns("drenagem_fonte_ano"), inline = TRUE),
      )
    ),
  )
}
