colunas <- list(
  "Estado" = "estado",
  "Região" = "regiao",
  "Componente" = "componente",
  "Situação" = "situacao",
  "Destino"= "destino",
  "Etapa" = "etapa"
  )

analise_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(12, shiny::titlePanel(shiny::strong("Painel"))),
      shiny::column(
        6,
        shiny::selectInput(
          inputId = ns("eixo"),
          label = shiny::strong("Eixo vertical:"),
          choices = colunas,
          selected = "componente",
        ),
      ),
      shiny::column(
        6,
        shiny::selectInput(
          inputId = ns("cores"),
          label = shiny::strong("Cores:"),
          choices = colunas,
          selected = "destino"
        ),
      ),
      shiny::column(
        6,
        shiny::selectInput(
          inputId = ns("barra"),
          label = shiny::strong("Tipo de barra:"),
          choices = c("stack", "group"),
          selected = "stack"
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
      ),
      shiny::column(
        6,
        shiny::checkboxInput(
          ns("legenda"),
          label = "Legenda",
          value = TRUE
        ),
      ),
    ),
    shiny::fluidRow(
      style = "padding:10px",
      shiny::column(
        12,
        shiny::h4(shiny::strong("Análise"))
      ),
      shiny::conditionalPanel(
        ns = ns,
        condition = "input.vis.indexOf('grafico') > -1",
        shiny::column(
          12,
          shinycssloaders::withSpinner(
            plotly::plotlyOutput(
              ns("grafico"),
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
            DT::dataTableOutput(ns("tbl"))
          )
        ),
      ),
    )
  )
}
