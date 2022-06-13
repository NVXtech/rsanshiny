dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(fluidRow(column(
    12,
    h2("Filtros"),
    selectInput(
      inputId = ns("espacial"),
      label = strong("Filtrar espacialmente:"),
      choices = c("País", "Região", "UF"),
      selected = "Brasil"
    ),
  )),
  fluidRow(h2(strong("Água e Esgoto"))),
  fluidRow(
    column(
      12,
      h3("Investimento"),
      plotlyOutput(ns("investimento"), height = 'auto', width = 'auto')
    ),
    column(12,
           plotlyOutput(
             ns("investimento_segmentado"),
             height = 'auto',
             width = 'auto'
           )),
    column(
      12,
      h3("Déficit de atendimento"),
      plotlyOutput(ns("deficit"),  height = 'auto', width = 'auto')
    )
  ),
  )
}
