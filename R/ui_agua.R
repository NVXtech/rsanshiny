get_snis_list <- function() {
  data("snis")
  snis_choices <- as.list(snis$caminho)
  names(snis_choices) <- snis$nome
  return(snis_choices)
}

get_sinapi_list <- function() {
  data("sinapi")
  sinapi_choices <- as.list(sinapi$caminho)
  names(sinapi_choices) <- sinapi$nome
  return(sinapi_choices)
}

agua_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        h1(strong("Água"), style = "display: inline-block;margin:0;"),
        actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    fluidRow(
      column(
        3,
        titlePanel("Fonte de Dados"),
        selectInput(
          inputId = ns("snis"),
          label = strong("Selecione o ano do SNIS"),
          choices = get_snis_list(),
          selected = app_state$demografico$snis
        ),
        selectInput(
          inputId = ns("sinapi"),
          label = strong("Selecione o ano e mês do SINAPI"),
          choices = get_sinapi_list(),
          selected = app_state$orcamentario$sinapi
        )
      ),
      column(
        9,
        titlePanel("Parâmetros"),
        tabsetPanel(
          type = "tabs",
          tabPanel(
            "Módulo Demográfico",
            numericInput(
              inputId = ns("meta_agua"),
              label = strong("Meta de atendimento para abastecimento de água (%)"),
              value = app_state$input$agua$meta_agua,
              min = 0,
              max = 100
            ),
          ),
          tabPanel(
            "Módulo Orçamentário",
            fluidRow(
              column(
                6,
                shiny::h3(" Distribuição"),
                numericInput(
                  inputId = ns("fator_servicos"),
                  label = strong("Fator correção dos preços de Serviços (%)"),
                  value = app_state$input$agua$fator_servicos,
                  min = 0,
                  max = 100
                ),
                numericInput(
                  inputId = ns("fator_materiais"),
                  label = strong("Fator correção dos preços de Materiais (%)"),
                  value = app_state$input$agua$fator_materiais,
                  min = 0,
                  max = 100
                )
              ),
              column(
                6,
                shiny::h3("Produção"),
                numericInput(
                  inputId = ns("fator_composicao"),
                  label = strong("Fator correção dos preços de Composição (%)"),
                  value = app_state$input$agua$fator_composicao,
                  min = 0,
                  max = 100
                ),
                numericInput(
                  inputId = ns("fator_insumo"),
                  label = strong("Fator correção dos preços de Insumos (%)"),
                  value = app_state$input$agua$fator_insumo,
                  min = 0,
                  max = 100
                ),
                numericInput(
                  inputId = ns("perda_agua"),
                  label = strong("Estimativa de Perda de água (%)"),
                  value = app_state$input$agua$perda_agua,
                  min = 0,
                  max = 100
                )
              )
            )
          ),
          tabPanel(
            "Módulo Financeiro",
            numericInput(
              inputId = ns("vida_util"),
              label = strong("Vida útil média dos ativos (anos)"),
              value = app_state$input$agua$vida_util,
              min = 1e-10,
              max = 1e10
            ),
          ),
        )
      )
    )
  )
}
