dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(6, titlePanel(strong("Painel"))),
      column(
        6,
        selectInput(
          inputId = ns("espacial"),
          label = strong("Agrupar por:"),
          choices = c("País", "Região", "UF"),
          selected = "Brasil"
        ),
      )
    ),
    fluidRow(
      column(
        12,
        tabsetPanel(
          type = "tabs",
          tabPanel("Geral"),
          tabPanel("Água e Esgoto", dash_agua_esgoto(ns)),
          tabPanel("Resíduos Sólidos", dash_residuos(ns)),
          tabPanel("Drenagem", dash_drenagem(ns))
        )
      ),
    ),
  )
}

dash_agua_esgoto <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      6,
      plotlyOutput(ns("agua_esgoto_investimento"), height = "auto", width = "100%")
    ),
    column(
      6,
      plotlyOutput(
        ns("agua_esgoto_investimento_por_tipo"),
        height = "auto",
        width = "100%"
      )
    ),
    column(
      6,
      plotlyOutput(ns("agua_esgoto_deficit"), height = "auto", width = "100%")
    )
  )
}

dash_residuos <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      6,
      plotlyOutput(ns("residuos_investimento"), height = "auto", width = "100%")
    ),
    column(
      6,
      plotlyOutput(
        ns("residuos_investimento_por_tipo"),
        height = "auto",
        width = "100%"
      )
    ),
    column(
      6,
      plotlyOutput(ns("residuos_deficit"), height = "auto", width = "100%")
    )
  )
}

dash_drenagem <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      6,
      plotlyOutput(ns("drenagem_investimento"), height = "auto", width = "100%")
    ),
    column(
      6,
      plotlyOutput(
        ns("drenagem_investimento_por_tipo"),
        height = "auto",
        width = "100%"
      )
    )
  )
}


dash_drenagem <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      6,
      plotlyOutput(ns("drenagem_investimento"), height = "auto", width = "100%")
    ),
    column(
      6,
      plotlyOutput(
        ns("drenagem_investimento_por_tipo"),
        height = "auto",
        width = "100%"
      )
    )
  )
}
