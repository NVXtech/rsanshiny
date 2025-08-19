#' Retorna lista de opções de dados ibge censo
#'
#' @return um vetor com os nomes
#' @export
get_fonte1_list <- function() {
  fonte_choices <- as.list(rsan::get_censo_labels())
  nomes <- c()
  for (item in fonte_choices) {
    nomes <- c(nomes, rsan::ibge_id_to_name(item))
  }
  names(fonte_choices) <- nomes
  return(fonte_choices)
}

#' Retorna lista de opções de dados ibge censo e estimativa
#'
#' @return um vetor com os nomes
#' @export
get_fonte2_list <- function() {
  fonte_choices <- as.list(rsan::get_populacao_labels())
  nomes <- c()
  for (item in fonte_choices) {
    nomes <- c(nomes, rsan::ibge_id_to_name(item))
  }
  names(fonte_choices) <- nomes
  return(fonte_choices)
}

#' Title
#'
#' @param ns é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
projecao_populacional_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Projeção Populacional"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
      )
    ),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3(shiny::strong("Fonte de dados")),
        shiny::p("Escolha o intervalo (fontes de dados) que serão utilizadas para calcular a taxa de crescimento"),
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
