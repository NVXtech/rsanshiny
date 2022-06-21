get_snis_ap_list <- function() {
    # TODO: refactor to get snis ap list
    data("snis")
    snis_choices <- as.list(snis$caminho)
    names(snis_choices) <- snis$nome
    return(snis_choices)
}

drenagem_ui <- function(id) {
    ns <- shiny::NS(id)
    fluidPage(
        fluidRow(
            column(
                12,
                h1(strong("Drenagem"), style = "display: inline-block;margin:0;"),
                actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
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
                column(
                    12,
                    numericInput(
                        inputId = ns("deprec_drenagem"),
                        label = strong("Depreciação em %"),
                        value = 2,
                        min = 0,
                        max = 100
                    )
                )
            )
        ),
        fluidRow(
            DT::dataTableOutput(ns("tabela")),
        )
    )
}
