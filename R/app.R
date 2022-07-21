# Aplicação Gráfica para
# Cálculo de Necessidade de Investimentos em Saneamento

library(shiny)
library(shinythemes)
library(shinycssloaders)
library(DT)

library(rlog)

library(dplyr, warn.conflicts = FALSE)
library(tidyr)

library(ggplot2)
library(plotly)
library(rsan)
library(writexl)
library(readxl)

#' main app ui
#'
#' @return shiny ui
#' @export
#'
app_ui <- function() {
  app_state <- rsan::check_and_create_state()
  rlog::log_info("App State loaded @ui")

  dashboard <- tabPanel("Painel",
    icon = icon("chart-line"),
    dashboard_ui("dashboard", app_state)
  )

  projecao <- tabPanel(
    "Projeção Populacional",
    icon = icon("users"),
    projecao_populacional_ui("projecao", app_state)
  )

  agua <- tabPanel(
    "Água",
    icon = icon("faucet"),
    agua_ui("agua", app_state)
  )

  esgoto <- tabPanel(
    "Esgoto",
    icon = icon("toilet"),
    esgoto_ui("esgoto", app_state)
  )

  residuos_solidos <-
    tabPanel(
      "Resíduos",
      icon = icon("recycle"),
      fluid = TRUE,
      residuos_ui("residuos", app_state)
    )


  drenagem_urbana <-
    tabPanel(
      "Drenagem",
      icon = icon("water"),
      fluid = TRUE,
      drenagem_ui("drenagem", app_state)
    )

  configuracoes <- tabPanel(
    "Configurações",
    icon = icon("cog"),
    fluid = TRUE,
    config_ui("config", app_state)
  )

  ui <- fluidPage(
    navbarPage(
      "NecessidadeInvest",
      id = "pages",
      theme = shinytheme("simplex"),
      dashboard,
      projecao,
      agua,
      esgoto,
      residuos_solidos,
      drenagem_urbana,
      configuracoes
    ),
    hr(),
    div(
      style = "padding:10px;display:block;margin-left:auto;margin-right:auto;text-align:center;background-color:#fff",
      img(
        src = "ABC.jpg",
        height = 72,
        width = "auto"
      ),
      img(
        src = "MDRlongo.png",
        height = 72,
        width = "auto"
      ),
      img(
        src = "IICA.png",
        height = 72,
        width = "auto"
      ),
      img(
        src = "Envex.png",
        height = 72,
        width = "auto"
      )
    )
  )
  return(ui)
}


modulo_calculo <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$rodar, {
      withProgress(message = "Recalculando", value = 0, {
        n <- 4
        shiny::incProgress(0, detail = "Carregando cálculos anteriores")
        app_state <- rsan::load_app_state()
        shiny::incProgress(1 / n, detail = "Salvando novos parâmetros")
        app_state <- rsan::salva_parametros(app_state, input, id)
        shiny::incProgress(1 / n, detail = "Investimento")
        app_state <- rsan::rodar_modelo(app_state)
        shiny::incProgress(1 / n, detail = "Salvando resultados")
        rsan::save_state(app_state)
        app_state$drenagem
        shiny::incProgress(1 / n, detail = "Fim")
      })
    })

    output$download <- downloadHandler(
      filename = function() {
        paste0(id, ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(app_state[[id]], file)
      }
    )
  })
}

#' main server of app
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#'
#' @return shiny server
#' @export
app_server <- function(input, output, session) {
  rlog::log_info("Started new session")

  app_state <- rsan::check_and_create_state()
  rlog::log_info("App State loaded @server")

  update_dashstate <- rsanshiny:::dashboard_server("dashboard", app_state)
  rsanshiny:::projecao_server("projecao", app_state)

  modulos <- c("agua", "esgoto", "drenagem", "residuos")
  for (modulo in modulos) {
    rsanshiny:::modulo_calculo(modulo, app_state)
  }

  rsanshiny:::config_server("config", app_state)
  rsanshiny:::config_server("drenagem", app_state)

  shiny::observeEvent(input$pages, {
    update_dashstate()
  })
}

#' Run the Shiny Application
#'
#' @param options optional, described in ?shiny::shinyApp
#'
#' @export
run_app <- function(options = list()) {
  rlog::log_info("Starting...")

  app_state <- rsan::check_and_create_state()
  rlog::log_info("App State loaded @runapp")

  rlog::log_info("Preparing local storage")
  rsan::check_and_create_datasets()

  #shiny::runApp()
  shiny::runApp(appDir = system.file("", package = "rsanshiny"))
}
