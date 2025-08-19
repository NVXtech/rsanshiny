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
  dashboard <- shiny::tabPanel("Painel",
    icon = shiny::icon("chart-line"),
  )

  analise <- shiny::tabPanel("Análise",
    icon = shiny::icon("chart-line"),
  )

  projecao <- shiny::tabPanel(
    "Projeção Populacional",
    icon = shiny::icon("users"),
  )

  parametros <- shiny::tabPanel(
    "Parâmetros",
    icon = shiny::icon("sliders"),
  )

  configuracoes <- shiny::tabPanel(
    "Fonte de Dados",
    icon = shiny::icon("cog"),
  )

  atualizacao <- shiny::tabPanel(
    "Atualização de Fontes",
    icon = shiny::icon("rotate"),
  )

  ui <- shiny::fluidPage(
    shiny::navbarPage(
      "UniverSan",
      id = "pages",
      theme = shinythemes::shinytheme("simplex"),
      dashboard,
      analise,
      projecao,
      parametros,
      configuracoes,
      atualizacao
    ),
    shiny::uiOutput("content"),
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

  dashboard_update <- dashboard_server("dashboard")

  analise_update <- analise_server("analise")

  projecao_update <- projecao_server("projecao")

  parameters_server("parametros", parent_session = session)

  config_update <- config_server("config", parent_session = session)

  update_server("update")

  shiny::observeEvent(input$pages, {
    rlog::log_info(paste("Page changed to:", input$pages))
    if (input$pages == "Painel") {
      output$content <- shiny::renderUI({
        dashboard_update()
        dashboard_ui("dashboard", rsan::load_app_state())
      })
    } else if (input$pages == "Análise") {
      output$content <- shiny::renderUI({
        analise_update()
        analise_ui("analise", rsan::load_app_state())
      })
    } else if (input$pages == "Projeção Populacional") {
      output$content <- shiny::renderUI({
        projecao_update()
        projecao_populacional_ui("projecao", rsan::load_app_state())
      })
    } else if (input$pages == "Parâmetros") {
      output$content <- shiny::renderUI({
        parametros_ui("parametros", rsan::load_app_state())
      })
    } else if (input$pages == "Fonte de Dados") {
      output$content <- shiny::renderUI({
        config_update()
        config_ui("config", rsan::load_app_state())
      })
    } else if (input$pages == "Atualização de Fontes") {
      output$content <- shiny::renderUI({
        update_ui("update", rsan::load_app_state())
      })
    }
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
  rlog::log_info("App State loaded @server")

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
