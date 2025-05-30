# Aplicação Gráfica para
# Cálculo de Necessidade de Investimentos em Saneamento

library(shiny)
library(shinythemes)
library(shinyWidgets)
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

  dashboard <- shiny::tabPanel("Painel",
    icon = shiny::icon("chart-line"),
    dashboard_ui("dashboard", app_state)
  )

  analise <- shiny::tabPanel("Análise",
    icon = shiny::icon("chart-line"),
    analise_ui("analise", app_state)
  )

  projecao <- shiny::tabPanel(
    "Projeção Populacional",
    icon = shiny::icon("users"),
    projecao_populacional_ui("projecao", app_state)
  )

  agua <- shiny::tabPanel(
    "Água",
    icon = shiny::icon("faucet"),
    agua_ui("agua", app_state)
  )

  esgoto <- shiny::tabPanel(
    "Esgoto",
    icon = shiny::icon("toilet"),
    esgoto_ui("esgoto", app_state)
  )

  residuos_solidos <-
    shiny::tabPanel(
      "Resíduos",
      icon = shiny::icon("recycle"),
      fluid = TRUE,
      residuos_ui("residuos", app_state)
    )


  drenagem_urbana <-
    shiny::tabPanel(
      "Drenagem",
      icon = shiny::icon("water"),
      fluid = TRUE,
      drenagem_ui("drenagem", app_state)
    )

  configuracoes <- shiny::tabPanel(
    "Atualização de Dados",
    icon = shiny::icon("cog"),
    fluid = TRUE,
    config_ui("config", app_state)
  )

  ui <- shiny::fluidPage(
    shiny::navbarPage(
      "UniverSan",
      id = "pages",
      theme = shinythemes::shinytheme("simplex"),
      dashboard,
      analise,
      projecao,
      agua,
      esgoto,
      residuos_solidos,
      drenagem_urbana,
      configuracoes
    ),
    shiny::hr(),
    shiny::div(
      style = "padding:10px;display:block;margin-left:auto;margin-right:auto;text-align:center;background-color:#fff",
      shiny::img(
        src = "images/ABC.jpg",
        height = 72,
        width = "auto"
      ),
      shiny::img(
        src = "images/MDRlongo.png",
        height = 72,
        width = "auto"
      ),
      shiny::img(
        src = "images/IICA.png",
        height = 72,
        width = "auto"
      ),
      shiny::img(
        src = "images/Envex.png",
        height = 72,
        width = "auto"
      )
    )
  )
  return(ui)
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

  update_dashstate <- dashboard_server("dashboard", app_state)
  projecao_server("projecao", app_state, parent = input)

  modulos <- c("agua", "esgoto", "drenagem", "residuos")
  for (modulo in modulos) {
    modulo_calculo(modulo, app_state, parent = input)
  }

  config_server("config", app_state)
  config_server("drenagem", app_state)

  analise_server("analise", app_state, parent = input)

  shiny::observeEvent(input$pages, {
    update_dashstate()
  })
}


#' Run the Shiny Application
#'
#' @param options optional, described in ?shiny::shinyApp
#'
#' @export
run_app <- function(options = list(port = 8888)) {
  rlog::log_info("Starting...")
  shiny::addResourcePath("images", system.file("www", package = "rsanshiny"))
  app_state <- rsan::check_and_create_state()
  rlog::log_info("App State loaded @runapp")

  rlog::log_info("Preparing local storage")
  rsan::check_and_create_datasets()

  if (is.null(app_state$necessidade)) {
    rlog::log_info("First Calculation")
    app_state <- rsan::rodar_modelo(app_state)
    rsan::save_state(app_state)
  }

  # shiny::runApp()
  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = options
  )
}
