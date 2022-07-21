get_fonte1_list <- function() {
  populacoes <- rsan::load_data("populacao")
  pop <- dplyr::filter(populacoes, tipo == "CENSO")
  fonte1_choices <- as.list(pop$caminho)
  names(fonte1_choices) <- pop$nome
  return(fonte1_choices)
}

get_fonte2_list <- function() {
  populacoes <- rsan::load_data("populacao")
  fonte1_choices <- as.list(populacoes$caminho)
  names(fonte1_choices) <- populacoes$nome
  return(fonte1_choices)
}

projecao_populacional_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Projeção Populacional"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3(shiny::strong("Fonte dos dados")),
        shiny::p("Escolha as fontes de dados que serão utilizadas para calcular a taxa de crescimento"),
        shiny::selectInput(
          inputId = ns("fonte1"),
          label = shiny::strong("Fonte de dados Inicial"),
          choices = get_fonte1_list(),
          selected = app_state$input$projecao$fonte1
        ),
        shiny::selectInput(
          inputId = ns("fonte2"),
          label = shiny::strong("Fonte de dados Final"),
          choices = get_fonte2_list(),
          selected = app_state$input$projecao$fonte2
        ),
      ),
      shiny::mainPanel(
        plotly::plotlyOutput(ns("grafico"))
      ),
    )
  )
}
