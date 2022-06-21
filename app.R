# Aplicação Gráfica para
# Cálculo de Necessidade de Investimentos em Saneamento
# Se estiver no RStudio clique em Run App para rodar a aplicação
rm(list = ls())

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

source("data_helpers.R")

source("app_state.R")
source("data_loader.R")

source("ui_dashboard.R")
source("server_dashboard.R")

source("ui_projecao.R")
source("server_projecao.R")

source("ui_agua_esgoto.R")
source("server_agua_esgoto.R")

source("ui_residuos.R")
source("server_residuos.R")

source("ui_drenagem.R")
source("server_drenagem.R")

rlog::log_info("Starting")

# checking data folder
check_and_create_datasets()

dashboard <- tabPanel(
  "Painel",
  icon = icon("chart-line"),
  dashboard_ui("dashboard")
)

projecao <- tabPanel(
  "Projeção Populacional",
  icon = icon("users"),
  projecao_populacional_ui("projecao")
)


agua_esgoto <- tabPanel(
  "Água e Esgoto",
  icon = icon("faucet"),
  agua_esgoto_ui("agua_esgoto")
)


residuos_solidos <-
  tabPanel("Resíduos Sólidos",
    icon = icon("recycle"),
    fluid = TRUE,
    residuos_ui("residuos")
  )


drenagem_urbana <-
  tabPanel("Drenagem Urbana",
    icon = icon("water"),
    fluid = TRUE,
    drenagem_ui("drenagem")
  )


configuracoes <- navbarMenu("Configurações",
  icon = icon("cog"),
  tabPanel("Atualização de dados"),
)


ui <- fluidPage(
  navbarPage(
    "InvestSan",
    theme = shinytheme("simplex"),
    dashboard,
    projecao,
    agua_esgoto,
    residuos_solidos,
    drenagem_urbana,
    configuracoes,
  ),
  hr(),
  div(
    style = "padding:10px;display:block;margin-left:auto;margin-right:auto;text-align:center;background-color:#fff",
    img(src = "ABC.jpg", height = 72, width = "auto"),
    img(src = "MDRlongo.png", height = 72, width = "auto"),
    img(src = "IICA.png", height = 72, width = "auto"),
    img(src = "Envex.png", height = 72, width = "auto")
  )
)


server <- function(input, output, session) {
  state <- new("employee", name = "Jonas")
  rlog::log_info("Started new session")
  dashboard_server("dashboard")
  projecao_server("projecao")

  agua_esgoto_server("agua_esgoto")

  residuos_server("residuos")

  drenagem_server("drenagem")
}

options <- list(port = 8080)
shinyApp(ui = ui, server = server, options = options)
