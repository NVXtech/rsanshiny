get_snis_list <- function() {
  utils::data("snis", package = "rsan")
  snis <- get("snis")
  snis_choices <- as.list(snis$caminho)
  names(snis_choices) <- snis$nome
  return(snis_choices)
}

get_sinapi_list <- function() {
  utils::data("sinapi", package = "rsan")
  sinapi <- get("sinapi")
  sinapi_choices <- as.list(sinapi$caminho)
  names(sinapi_choices) <- sinapi$nome
  return(sinapi_choices)
}

#' Title
#'
#' @param ns é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
agua_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Água"), style = "display: inline-block;margin:0;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::downloadButton(ns("download"), "Exportar xlsx", style = "display: inline-block;margin-bottom:10px;")
      )
    ),
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::titlePanel("Fonte de Dados"),
        shiny::selectInput(
          inputId = ns("snis"),
          label = shiny::strong("Selecione o ano do SNIS"),
          choices = get_snis_list(),
          selected = app_state$demografico$snis
        ),
        shiny::selectInput(
          inputId = ns("sinapi"),
          label = shiny::strong("Selecione o ano e mês do SINAPI"),
          choices = get_sinapi_list(),
          selected = app_state$orcamentario$sinapi
        )
      ),
      shiny::column(
        9,
        shiny::titlePanel("Parâmetros"),
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Módulo Demográfico",
            shiny::numericInput(
              inputId = ns("meta_agua"),
              label = shiny::strong("Meta de atendimento para abastecimento de água (%)"),
              value = app_state$input$agua$meta_agua,
              min = 0,
              max = 100
            ),
          ),
          shiny::tabPanel(
            "Módulo Orçamentário",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::h3(" Distribuição"),
                shiny::numericInput(
                  inputId = ns("fator_servicos"),
                  label = shiny::strong("Fator correção dos preços de Serviços (%)"),
                  value = app_state$input$agua$fator_servicos,
                  min = 0,
                  max = 100
                ),
                shiny::numericInput(
                  inputId = ns("fator_materiais"),
                  label = shiny::strong("Fator correção dos preços de Materiais (%)"),
                  value = app_state$input$agua$fator_materiais,
                  min = 0,
                  max = 100
                )
              ),
              shiny::column(
                6,
                shiny::h3("Produção"),
                shiny::numericInput(
                  inputId = ns("fator_composicao"),
                  label = shiny::strong("Fator correção dos preços de Composição (%)"),
                  value = app_state$input$agua$fator_composicao,
                  min = 0,
                  max = 100
                ),
                shiny::numericInput(
                  inputId = ns("fator_insumo"),
                  label = shiny::strong("Fator correção dos preços de Insumos (%)"),
                  value = app_state$input$agua$fator_insumo,
                  min = 0,
                  max = 100
                ),
                shiny::numericInput(
                  inputId = ns("perda_agua"),
                  label = shiny::strong("Estimativa de Perda de água (%)"),
                  value = app_state$input$agua$perda_agua,
                  min = 0,
                  max = 100
                )
              )
            )
          ),
          shiny::tabPanel(
            "Módulo Financeiro",
            shiny::numericInput(
              inputId = ns("vida_util"),
              label = shiny::strong("Vida útil média dos ativos (anos)"),
              value = app_state$input$agua$vida_util,
              min = 1e-10,
              max = 1e10
            ),
          ),
          shiny::tabPanel(
            "Módulo Rural",
            shiny::numericInput(
              inputId = ns("custo_rural_individual"),
              label = shiny::strong("Custo rural individual (R$/dom)"),
              value = app_state$input$agua$custo_rural_individual,
              min = 1e-10,
              max = 1e10
            ),
            shiny::numericInput(
              inputId = ns("custo_rural_individual_sem"),
              label = shiny::strong("Custo rural individual sem disponibilidade(R$/dom)"),
              value = app_state$input$agua$custo_rural_individual_sem,
              min = 1e-10,
              max = 1e10
            ),
          ),
        )
      )
    )
  )
}
