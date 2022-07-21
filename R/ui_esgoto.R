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

esgoto_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Esgoto"), style = "display: inline-block;margin:0;"),
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
          selected = app_state$input$esgoto$snis
        ),
        shiny::selectInput(
          inputId = ns("sinapi"),
          label = shiny::strong("Selecione o ano e mês do SINAPI"),
          choices = get_sinapi_list(),
          selected = app_state$input$esgoto$sinapi
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
              inputId = ns("meta_esgoto"),
              label = shiny::strong("Meta de atendimento para esgoto (%)"),
              value = app_state$input$esgoto$meta_esgoto,
              min = 0,
              max = 100
            ),
            shiny::numericInput(
              inputId = ns("proporcao"),
              label = shiny::strong("Proporção entre a densidade esgoto e abastecimento (%)"),
              value = app_state$input$esgoto$proporcao,
              min = 0,
              max = 100
            )
          ),
          shiny::tabPanel(
            "Módulo Orçamentário",
            shiny::fluidRow(
              shiny::column(
                6,
                shiny::h3("Coleta"),
                shiny::numericInput(
                  inputId = ns("fator_servicos"),
                  label = shiny::strong("Fator correção dos preços de Serviços (%)"),
                  value = app_state$input$esgoto$fator_servicos,
                  min = 0,
                  max = 100
                ),
                shiny::numericInput(
                  inputId = ns("fator_materiais"),
                  label = shiny::strong("Fator correção dos preços de Materiais (%)"),
                  value = app_state$input$esgoto$fator_materiais,
                  min = 0,
                  max = 100
                )
              ),
              shiny::column(
                6,
                shiny::h3("Tratamento"),
                shiny::numericInput(
                  inputId = ns("fator_composicao"),
                  label = shiny::strong("Fator correção dos preços de Composição (%)"),
                  value = app_state$input$esgoto$fator_composicao,
                  min = 0,
                  max = 100
                ),
                shiny::numericInput(
                  inputId = ns("fator_insumo"),
                  label = shiny::strong("Fator correção dos preços de Insumos (%)"),
                  value = app_state$input$esgoto$fator_insumo,
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
              value = app_state$input$esgoto$vida_util,
              min = 1e-10,
              max = 1e10
            ),
          ),
        )
      )
    )
  )
}
