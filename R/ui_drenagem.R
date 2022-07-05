get_snis_ap_list <- function() {
    data("snis_ap")
    snis_choices <- as.list(names(snis_ap))
    names(snis_choices) <- names(snis_ap)
    return(snis_choices)
}

drenagem_ui <- function(id, app_state) {
    ns <- shiny::NS(id)
    fluidPage(
        fluidRow(
            column(
                12,
                h1(strong("Drenagem"), style = "display: inline-block;margin:0;"),
                actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
                downloadButton(ns("download"), "Exportar xlsx",style = "display: inline-block;margin-bottom:10px;")
            )
        ),
        fluidRow(
            column(
                3,
                titlePanel("Fonte de Dados"),
                column(
                    12,
                    selectInput(
                        inputId = ns("snis_ap"),
                        label = strong("Selecione o ano do SNIS - Águas Pluviais"),
                        choices = get_snis_ap_list(),
                        selected = app_state$input$snis_ap
                    )
                )
            ),
            column(
                9,
                titlePanel("Parâmetros"),
                tabsetPanel(
                  type = "tabs",
                  tabPanel(
                    "Investimento por habitante",
                    selectInput(ns("modo"), label = strong("Modo de cálculo"),
                                choices = list("Investimento per capita constante" = 1, "Regressão PMSB" = 2),
                                selected = 2),
                    conditionalPanel(
                      condition = "input.modo == 1",
                      ns = ns,
                      numericInput(
                        inputId = ns("investimento_per_capita"),
                        label = strong("Investimento em drenagem por habitante (R$/hab)"),
                        value = 10000,
                        min = 0,
                        max = 1e20
                      )
                    ),
                    numericInput(
                      inputId = ns("custo_cadastro"),
                      label = strong("Custo do cadastro técnico (R$/km²)"),
                      value = 7738.89,
                      min = -1e9,
                      max = 1e9
                    ),
                  ),
                  tabPanel(
                    "Pesos dos Indicadores",
                    numericInput(
                      inputId = ns("peso_pluviometria"),
                      label = strong("Pluviometria [R$/(mm/ano)]"),
                      value = 0.063933104088543,
                      min = -1e9,
                      max = 1e9
                    ),
                    numericInput(
                      inputId = ns("peso_densidade"),
                      label = strong("Densidade urbana [R$/(hab/km²)]"),
                      value = -0.189155004725778,
                      min = -1e9,
                      max = 1e9
                    ),
                    numericInput(
                      inputId = ns("peso_fisicas"),
                      label = strong("Características físicas (R$/-)"),
                      value = 3477.79720206452,
                      min = -1e9,
                      max = 1e9
                    ),
                    numericInput(
                      inputId = ns("peso_infraestrutura"),
                      label = strong("Infraestrutura (R$/-)"),
                      value = 519.474326911018,
                      min = -1e9,
                      max = 1e9
                    ),
                    numericInput(
                      inputId = ns("peso_constante"),
                      label = strong("Constante"),
                      value = 791.359914329392,
                      min = -1e9,
                      max = 1e9
                    ),
                  ),
                  tabPanel(
                    "Reposição",
                    numericInput(
                        inputId = ns("deprec_drenagem"),
                        label = strong("Depreciação dos ativos em %"),
                        value = 2,
                        min = 0,
                        max = 100
                    )
                  )
                )
            )
        ),
        fluidRow(
            DT::dataTableOutput(ns("tabela")),
        )
    )
}
