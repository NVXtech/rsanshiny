get_fonte1_list <- function() {
  populacoes <- load_data("populacao")
  pop <- filter(populacoes, tipo == "CENSO")
  fonte1_choices <- as.list(pop$caminho)
  names(fonte1_choices) <- pop$nome
  return(fonte1_choices)
}

get_fonte2_list <- function() {
  populacoes <- load_data("populacao")
  fonte1_choices <- as.list(populacoes$caminho)
  names(fonte1_choices) <- populacoes$nome
  return(fonte1_choices)
}

projecao_populacional_ui <- function(id, app_state) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      h3(strong("Fonte dos dados")),
      p("Escolha as fontes de dados que serão utilizadas para calcular a taxa de crescimento"),
      selectInput(
        inputId = ns("fonte1"),
        label = strong("Fonte de dados Inicial"),
        choices = get_fonte1_list(),
        selected = app_state$input$projecao$fonte1
      ),
      selectInput(
        inputId = ns("fonte2"),
        label = strong("Fonte de dados Final"),
        choices = get_fonte2_list(),
        selected = app_state$input$projecao$fonte2
      ),
      actionButton(ns("rodar"), label = "Calcular projeção"),
      downloadButton(ns("download"), "Exportar xlsx",style = "display: inline-block;margin-bottom:10px;")
    ),
    mainPanel(
      plotlyOutput(ns("grafico"))
    ),
  )
}
