dash_geral <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("geral_investimento"), height = "auto", width = "100%")
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(
          ns("geral_investimento_por_tipo"),
          height = "auto",
          width = "100%"
        )
      )
    )
  )
}

dash_agua <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("agua_investimento"), height = "auto", width = "100%")
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(
          ns("agua_investimento_por_tipo"),
          height = "auto",
          width = "100%"
        )
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("agua_deficit"), height = "auto", width = "100%")
      )
    )
  )
}

dash_esgoto <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("esgoto_investimento"), height = "auto", width = "100%")
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(
          ns("esgoto_investimento_por_tipo"),
          height = "auto",
          width = "100%"
        )
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("esgoto_deficit"), height = "auto", width = "100%")
      )
    )
  )
}

dash_residuos <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("residuos_investimento"), height = "auto", width = "100%")
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(
          ns("residuos_investimento_por_tipo"),
          height = "auto",
          width = "100%"
        )
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("residuos_deficit"), height = "auto", width = "100%")
      )
    )
  )
}

dash_drenagem <- function(ns) {
  fluidRow(
    style = "padding:10px",
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(ns("drenagem_investimento"), height = "auto", width = "100%")
      )
    ),
    column(
      12,
      shinycssloaders::withSpinner(
        plotlyOutput(
          ns("drenagem_investimento_por_tipo"),
          height = "auto",
          width = "100%"
        )
      )
    )
  )
}


dashboard_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(12, titlePanel(strong("Painel"))),
      column(
        12,
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
          id = ns("dash_tab"),
          type = "tabs",
          tabPanel("Geral", dash_geral(ns)),
          tabPanel("Água", dash_agua(ns)),
          tabPanel("Esgoto", dash_esgoto(ns)),
          tabPanel("Resíduos", dash_residuos(ns)),
          tabPanel("Drenagem", dash_drenagem(ns))
        )
      ),
    ),
  )
}
