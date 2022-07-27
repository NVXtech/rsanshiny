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
#' @param id é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
esgoto_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  params <- app_state$input$esgoto
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
          shiny::tabPanel(
            "Módulo Rural",
            shiny::column(
              6,
              shiny::h4("Custo individual (R$/domicílio):"),
              esgoto_individual_input(ns, params)
            )
          )
        )
      )
    )
  )
}

esgoto_individual_input <- function(ns, input) {
  labels <- c(
    "com disponibilidade hídrica e média hab/domicílio entre 0 e 2",
    "com disponibilidade hídrica e média hab/domicílio entre 2 e 3",
    "com disponibilidade hídrica e média hab/domicílio entre 3 e 4",
    "com disponibilidade hídrica e média hab/domicílio entre 4 e 5",
    "com disponibilidade hídrica e média hab/domicílio entre 5 e 6",
    "com disponibilidade hídrica e média hab/domicílio entre 6 e 7",
    "com disponibilidade hídrica e média hab/domicílio entre 7 e 8",
    "com disponibilidade hídrica e média hab/domicílio maior que 8",
    "sem disponibilidade hídrica e média hab/domicílio entre 0 e 5",
    "sem disponibilidade hídrica e média hab/domicílio maior que 5"
  )
  output <- list()
  for (i in seq.int(1, length(labels))) {
    id <- sprintf("custo_individual_esgoto_faixa%s", i)
    output[[i]] <- list(shinyWidgets::autonumericInput(
      inputId = ns(id),
      label = shiny::strong(labels[i]),
      value = input[[id]],
      align = "left",
      decimalCharacter = ",",
      digitGroupSeparator = ".",
      decimalPlaces = 2
    ))
  }
  return(output)
}
