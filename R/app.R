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

  parametros <- shiny::tabPanel(
    "Parâmetros",
    icon = shiny::icon("sliders"),
    fluid = TRUE,
    parametros_ui("parametros", app_state)
  )

  configuracoes <- shiny::tabPanel(
    "Fonte de Dados",
    icon = shiny::icon("cog"),
    fluid = TRUE,
    config_ui("config", app_state)
  )

  ui <- shiny::fluidPage(
    tags$style(type = "text/css", "body {padding-top: 70px;}"),
    shiny::navbarPage(
      "UniverSan",
      id = "pages",
      theme = shinythemes::shinytheme("simplex"),
      position = "fixed-top",
      dashboard,
      analise,
      projecao,
      parametros,
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

  modulo_calculo("parametros", app_state, parent = input)

  config_server("config", app_state)

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

  shiny::shinyApp(
    ui = app_ui,
    server = app_server,
    options = options
  )
}
