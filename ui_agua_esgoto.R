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

agua_esgoto_ui <- function(id) {
  ns <- shiny::NS(id)
  fluidPage(
    fluidRow(
      column(
        12,
        h1(strong("Água e esgoto"), style = "display: inline-block;margin:0;"),
        actionButton(ns("rodar"), icon = icon("calculator"), label = "Recalcular", style = "display: inline-block;margin-bottom:10px;"),
      )
    ),
    fluidRow(
      column(
        4, h3(strong("Módulo Demográfico")), h3("Dados de Entrada"),
        selectInput(
          inputId = ns("snis"),
          label = strong("Selecione o ano do SNIS"),
          choices = get_snis_list(),
          selected = app_state$demografico$snis
        ),
        sliderInput(
          inputId = ns("ano"),
          strong("Realizar cálculo para o ano de:"),
          step = 1,
          min = 2021,
          max = 2040,
          value = 2033
        ),
        numericInput(
          inputId = ns("meta_agua"),
          label = strong("Meta de atendimento para abastecimento de água (%)"),
          value = 99,
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = ns("meta_esgoto"),
          label = strong("Meta de atendimento para esgoto (%)"),
          value = 90,
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = ns("proporcao"),
          label = strong("Proporção entre a densidade esgoto e abastecimento (%)"),
          value = 80,
          min = 0,
          max = 100
        )
      ),
      column(
        4, h3(strong("Módulo Orçamentário")), h3("Dados de Entrada"),
        selectInput(
          inputId = ns("sinapi"),
          label = strong("Selecione o ano e mês do SINAPI"),
          choices = get_sinapi_list(),
          selected = app_state$orcamentario$sinapi
        ),
        h3("Parâmetros Distribuição e Coleta"),
        numericInput(
          inputId = ns("fator_servicos"),
          label = strong("Fator correção dos preços de Serviços (%)"),
          value = 26,
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = ns("fator_materiais"),
          label = strong("Fator correção dos preços de Materiais (%)"),
          value = 18,
          min = 0,
          max = 100
        ),
        h3("Parâmetros Produção e Tratamento"),
        numericInput(
          inputId = ns("fator_composicao"),
          label = strong("Fator correção dos preços de Composição (%)"),
          value = 26,
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = ns("fator_insumo"),
          label = strong("Fator correção dos preços de Insumos (%)"),
          value = 18,
          min = 0,
          max = 100
        ),
        numericInput(
          inputId = ns("perda_agua"),
          label = strong("Estimativa de Perda de água (%)"),
          value = 25,
          min = 0,
          max = 100
        )
      ),
      column(
        4, h3(strong("Módulo Financeiro")),
        h3("Dados de Entrada"),
        selectInput(
          inputId = ns("snis"),
          label = strong("Selecione o ano do SNIS"),
          choices = get_snis_list(),
          selected = app_state$financeiro$snis
        ),
        numericInput(
          inputId = ns("vida_util"),
          label = strong("Vida útil média dos ativos (anos)"),
          value = 30,
          min = 1e-10,
          max = 1e10
        ),
      )
    )
  )
}

modulo_demografico_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("snis"),
        label = strong("Selecione o ano do SNIS"),
        choices = get_snis_list(),
        selected = app_state$demografico$snis
      ),
      sliderInput(
        inputId = ns("ano"),
        strong("Realizar cálculo para o ano de:"),
        step = 1,
        min = 2021,
        max = 2040,
        value = 2033
      ),
      numericInput(
        inputId = ns("meta_agua"),
        label = strong("Meta de atendimento para abastecimento de água (%)"),
        value = 99,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("meta_esgoto"),
        label = strong("Meta de atendimento para esgoto (%)"),
        value = 90,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("proporcao"),
        label = strong("Proporção entre a densidade esgoto e abastecimento (%)"),
        value = 80,
        min = 0,
        max = 100
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      plotlyOutput(ns("grafico_total")),
      DT::dataTableOutput(ns("tabela")),
    ),
  )
}

modulo_orcamentario_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("sinapi"),
        label = strong("Selecione o ano e mês do SINAPI"),
        choices = get_sinapi_list(),
        selected = app_state$orcamentario$sinapi
      ),
      hr(),
      h3("Parâmetros Distribuição e Coleta"),
      numericInput(
        inputId = ns("fator_servicos"),
        label = strong("Fator correção dos preços de Serviços (%)"),
        value = 26,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("fator_materiais"),
        label = strong("Fator correção dos preços de Materiais (%)"),
        value = 18,
        min = 0,
        max = 100
      ),
      hr(),
      h3("Parâmetros Produção e Tratamento"),
      numericInput(
        inputId = ns("fator_composicao"),
        label = strong("Fator correção dos preços de Composição (%)"),
        value = 26,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("fator_insumo"),
        label = strong("Fator correção dos preços de Insumos (%)"),
        value = 18,
        min = 0,
        max = 100
      ),
      numericInput(
        inputId = ns("perda_agua"),
        label = strong("Estimativa de Perda de água (%)"),
        value = 25,
        min = 0,
        max = 100
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      h3(strong("Custo")),
      DT::dataTableOutput(ns("custo")),
      h3(strong("Preços para Distribuição de Água por Estado")),
      DT::dataTableOutput(ns("distribuicao")),
      h3(strong("Preços para Coleta de Esgoto por Estado")),
      DT::dataTableOutput(ns("coleta")),
      h3(strong("Preços de Unidades de Produção de Água por Estado")),
      DT::dataTableOutput(ns("producao")),
      h3(strong("Preços de Unidades de Tratamento de Esgoto por Estado")),
      DT::dataTableOutput(ns("tratamento")),
      plotlyOutput(ns("grafico_total"))
    ),
  )
}

modulo_financeiro_ui <- function(id) {
  ns <- shiny::NS(id)
  sidebarLayout(
    sidebarPanel(
      titlePanel("Dados de Entrada"),
      selectInput(
        inputId = ns("snis"),
        label = strong("Selecione o ano do SNIS"),
        choices = get_snis_list(),
        selected = app_state$financeiro$snis
      ),
      numericInput(
        inputId = ns("vida_util"),
        label = strong("Vida útil média dos ativos (anos)"),
        value = 30,
        min = 1e-10,
        max = 1e10
      ),
      fluidRow(actionButton(ns("rodar"), label = "Calcular"))
    ),
    mainPanel(
      DT::dataTableOutput(ns("tabela"))
    ),
  )
}
