#' Retorna lista de opções de dados do SINAPI
#'
#' @return um vetor com os nomes
#' @export
get_sinapi_list <- function() {
  sinapi_choices <- rsan::get_sinapi_labels()
  output <- c()
  labels <- c()
  for (item in sinapi_choices) {
    split <- strsplit(item, "_")[[1]]
    year <- split[length(split)]
    label <- paste0("SINAPI ", year)
    labels <- c(labels, label)
    output <- c(output, item)
  }
  names(output) <- labels
  return(output)
}


#' Retorna os anos disponíveis para atendimento de acordo com a fonte
#'
#' @param fonte_nome Nome da fonte de dados (ex: "censo", "sinisa", "pnadc")
#' @return Vetor de anos disponíveis para a fonte informada
#' @export
atendimento_anos_disponiveis <- function(componente, fonte_nome) {
  if (fonte_nome == "censo") {
    anos <- rsan::anos_censo_setor_base_calculo()
  } else if (fonte_nome == "pnadc") {
    anos <- rsan::anos_disponiveis_pnadc(componente)
  } else {
    anos <- anos_disponiveis_base_calculo(componente, fonte_nome)
  }
  anos <- sort(anos, decreasing = TRUE)
  if (length(anos) == 0) {
    anos <- c("")
  }
  return(anos)
}


#' Retorna os anos disponíveis arquivos da base de cáculo no formate `componente_source_YYYY.csv`
#'
#' @param componente Nome do componente (ex: "agua", "esgoto")
#' @param source Nome da fonte de dados (ex: "snis", "sin
#'
#' @return Vetor de anos disponíveis encontrados nos arquivos CSV
#' @export
anos_disponiveis_base_calculo <- function(componente, source) {
  base_dir <- file.path("dados", "base_calculo")
  pattern <- paste0(componente, "_", source, "_\\d{4}\\.csv")
  files <- list.files(base_dir, pattern = pattern, full.names = TRUE)
  years <- as.numeric(
    gsub(paste0(".*_", source, "_(\\d{4})\\.csv"), "\\1", files)
  )
  return(years)
}


#' Retorna os anos disponíveis para um componente e fonte de dados estruturais
#'
#' @param componente Nome do componente (ex: "agua", "esgoto")
#' @param source Nome da fonte de dados (ex: "snis", "sinisa")
#' @return Vetor de anos disponíveis encontrados nos arquivos CSV
#' @export
estrutura_anos_disponiveis <- function(componente, source) {
  return(anos_disponiveis_base_calculo(componente, source))
}

#' Gera interface para a configuração das fontes de dados
#'
#' @param ns é o namespace do módulo de interface gráfica
#'
#' @return o html da interface gráfica
#' @export
config_ui <- function(id, app_state) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::actionButton(ns("salvar"), icon = shiny::icon("save"), label = "Salvar Parâmetros", style = "display: inline-block;margin-bottom:10px;"),
        shiny::actionButton(ns("rodar"), icon = shiny::icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
        shiny::actionButton(ns("padrao"), icon = shiny::icon("backward-step"), label = "Voltar padrão", style = "display: inline-block;margin-bottom:10px;"),
        shiny::actionButton(ns("ultimo"), icon = shiny::icon("backward"), label = "Voltar último cálculo", style = "display: inline-block;margin-bottom:10px;"),
      )
    ),
    # Configurações Gerais
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Horizonte"), style = "display: inline-block;margin:0;"),
      )
    ),
    shiny::fluidRow(
      shiny::textOutput(ns("parameters_status")),
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("geral-ano_corrente"),
          label = shiny::strong("Calcular necessidades a partir do ano:"),
          min = 2010,
          max = 2050,
          step = 1,
          value = app_state$input$geral$ano_corrente
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::numericInput(
          inputId = ns("geral-ano"),
          label = shiny::strong("Calcular necessidades até o ano:"),
          min = 2020,
          max = 2050,
          step = 1,
          value = app_state$input$geral$ano
        )
      ),
    ),
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::h1(shiny::strong("Fonte de Dados")),
      ),
    ),
    shiny::column(
      3,
      shiny::titlePanel("Abastecimento de Água"),
      shiny::selectInput(
        inputId = ns("agua-fonte_nome"),
        label = shiny::strong("Selecione fonte de dados estruturais"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$agua$fonte_nome
      ),
      shiny::uiOutput(ns("agua_fonte_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("agua-atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$agua$atendimento
      ),
      shiny::uiOutput(ns("agua_atendimento_ano"), inline = TRUE),
      shiny::selectInput(
        inputId = ns("agua-sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$input$agua$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Esgoto Sanitário"),
      shiny::selectInput(
        inputId = ns("esgoto-fonte_nome"),
        label = shiny::strong("Selecione a fonte de dados estruturais"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$esgoto$fonte_nome
      ),
      shiny::uiOutput(ns("esgoto_fonte_ano")),
      shiny::selectInput(
        inputId = ns("esgoto-atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$esgoto$atendimento
      ),
      shiny::uiOutput(ns("esgoto_atendimento_ano")),
      shiny::selectInput(
        inputId = ns("esgoto-sinapi"),
        label = shiny::strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$input$esgoto$sinapi
      )
    ),
    shiny::column(
      3,
      shiny::titlePanel("Residuos Sólidos Urbanos"),
      shiny::selectInput(
        inputId = ns("residuos-fonte_nome"),
        label = shiny::strong("Selecione a fonte de dados estruturais"),
        choices = c("SNIS" = "snis", "SINISA" = "sinisa"),
        selected = app_state$input$residuos$fonte_nome
      ),
      shiny::uiOutput(ns("residuos_fonte_ano")),
      shiny::selectInput(
        inputId = ns("residuos-atendimento"),
        label = shiny::strong("Selecione a fonte para o atendimento"),
        choices = c("CENSO" = "censo", "SINISA" = "sinisa", "PNADc" = "pnadc"),
        selected = app_state$input$residuos$atendimento
      ),
      shiny::uiOutput(ns("residuos_atendimento_ano")),
    ),
    shiny::column(
      3,
      shiny::titlePanel("Drenagem Urbana"),
      shiny::textOutput(ns("drenagem_info")),
    ),
  )
}
