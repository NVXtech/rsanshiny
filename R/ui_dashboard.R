dash_geral <- function(ns) {
  shiny::fluidRow(
    style = "padding:10px",
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento total"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("geral_investimento"),
            height = "auto", width = "100%"
          )
        )
      ),
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_geral_investimento"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento por componente"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("geral_investimento_por_tema"),
            height = "auto",
            width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_geral_investimento_por_tema"))
        )
      ),
    ),
  )
}

dash_agua <- function(ns) {
  shiny::fluidRow(
    style = "padding:10px",
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento total"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("agua_investimento"),
            height = "auto", width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_agua_investimento"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento por destino"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("agua_investimento_por_tipo"),
            height = "auto",
            width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_agua_investimento_por_tipo"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Déficit"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("agua_deficit"), height = "auto", width = "100%")
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_agua_deficit"))
        )
      ),
    )
  )
}

dash_esgoto <- function(ns) {
  shiny::fluidRow(
    style = "padding:10px",
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento total"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("esgoto_investimento"),
            height = "auto", width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_esgoto_investimento"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento por destino"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("esgoto_investimento_por_tipo"),
            height = "auto",
            width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_esgoto_investimento_por_tipo"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Déficit"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("esgoto_deficit"), height = "auto", width = "100%")
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_esgoto_deficit"))
        )
      ),
    ),
  )
}

dash_residuos <- function(ns) {
  shiny::fluidRow(
    style = "padding:10px",
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento total"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("residuos_investimento"),
            height = "auto", width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_residuos_investimento"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento por destino"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("residuos_investimento_por_tipo"),
            height = "auto",
            width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_residuos_investimento_por_destino"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Déficit"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(ns("residuos_deficit"), height = "auto", width = "100%")
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_residuos_deficit"))
        )
      ),
    ),
  )
}

dash_drenagem <- function(ns) {
  shiny::fluidRow(
    style = "padding:10px",
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento total"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("drenagem_investimento"),
            height = "auto", width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_drenagem_investimento"))
        )
      ),
    ),
    shiny::column(
      12,
      shiny::h4(shiny::strong("Necessidade de investimento por destino"))
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('grafico') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          plotly::plotlyOutput(
            ns("drenagem_investimento_por_tipo"),
            height = "auto",
            width = "100%"
          )
        )
      )
    ),
    shiny::conditionalPanel(
      ns = ns,
      condition = "input.vis.indexOf('tabela') > -1",
      shiny::column(
        12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput(ns("tbl_drenagem_investimento_por_tipo"))
        )
      ),
    ),
  )
}


dashboard_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12, shiny::titlePanel(shiny::strong("Painel"))),
      shiny::column(
        6,
        shiny::selectInput(
          inputId = ns("espacial"),
          label = shiny::strong("Agrupar por:"),
          choices = c("País", "Região", "UF"),
          selected = "Brasil"
        ),
      ),
      shiny::column(
        6,
        shiny::checkboxGroupInput(
          ns("vis"),
          label = shiny::strong("Visualizar como"),
          choices = list("Gráfico" = "grafico", "Tabela" = "tabela"),
          selected = "grafico"
        ),
      )
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::tabsetPanel(
          id = ns("dash_tab"),
          type = "tabs",
          shiny::tabPanel("Geral", dash_geral(ns)),
          shiny::tabPanel("Água", dash_agua(ns)),
          shiny::tabPanel("Esgoto", dash_esgoto(ns)),
          shiny::tabPanel("Resíduos", dash_residuos(ns)),
          shiny::tabPanel("Drenagem", dash_drenagem(ns))
        )
      ),
    ),
  )
}
