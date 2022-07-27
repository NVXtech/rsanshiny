get_snis_ap_list <- function() {
  utils::data("snis_ap", package = "rsan")
  snis_ap <- get("snis_ap")
  snis_choices <- as.list(names(snis_ap))
  names(snis_choices) <- names(snis_ap)
  return(snis_choices[order(unlist(snis_choices), decreasing = TRUE)])
}

drenagem_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  params <- app_state$input$drenagem
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Drenagem"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::titlePanel("Fonte de Dados"),
        shiny::column(
          12,
          shiny::selectInput(
            inputId = ns("snis_ap"),
            label = shiny::strong("Selecione o ano do SNIS - Águas Pluviais"),
            choices = get_snis_ap_list(),
            selected = app_state$input$snis_ap
          )
        )
      ),
      shiny::column(
        9,
        shiny::titlePanel("Parâmetros"),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Investimento por habitante",
            shiny::selectInput(ns("modo"),
              label = shiny::strong("Modo de cálculo"),
              choices = list("Investimento per capita constante" = 1, "Regressão PMSB" = 2),
              selected = 2
            ),
            shiny::conditionalPanel(
              condition = "input.modo == 1",
              ns = ns,
              shinyWidgets::autonumericInput(
                inputId = ns("investimento_per_capita"),
                label = shiny::strong("Investimento em drenagem por habitante (R$/hab)"),
                value = params$investimento_per_capita,
                align = "left",
                decimalCharacter = ",",
                digitGroupSeparator = ".",
                decimalPlaces = 2
              )
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("custo_cadastro"),
              label = shiny::strong("Custo do cadastro técnico (R$/km²)"),
              value = params$custo_cadastro,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
          ),
          shiny::tabPanel(
            "Pesos dos Indicadores",
            shinyWidgets::autonumericInput(
              inputId = ns("peso_pluviometria"),
              label = shiny::strong("Pluviometria [R$/(mm/ano)]"),
              value = params$peso_pluviometria,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("peso_densidade"),
              label = shiny::strong("Densidade urbana [R$/(hab/km²)]"),
              value = params$peso_densidade,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("peso_fisicas"),
              label = shiny::strong("Características físicas (R$/-)"),
              value = params$peso_fisicas,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("peso_infraestrutura"),
              label = shiny::strong("Infraestrutura (R$/-)"),
              value = params$peso_infraestrutura,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
            shinyWidgets::autonumericInput(
              inputId = ns("peso_constante"),
              label = shiny::strong("Constante"),
              value = params$peso_constante,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            ),
          ),
          shiny::tabPanel(
            "Reposição",
            shinyWidgets::autonumericInput(
              inputId = ns("deprec_drenagem"),
              label = shiny::strong("Depreciação dos ativos em %"),
              value = params$deprec_drenagem,
              align = "left",
              decimalCharacter = ",",
              digitGroupSeparator = ".",
              decimalPlaces = 2
            )
          )
        )
      )
    ),
    shiny::fluidRow(
      DT::dataTableOutput(ns("tabela")),
    )
  )
}
