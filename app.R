# Aplicação Gráfica para
# Cálculo de Necessidade de Investimentos em Saneamento
# Se estiver no RStudio clique em Run App para rodar a aplicação
rm(list=ls())

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

source("app_state.R")
source("data_loader.R")

source("ui_dashboard.R")

source("ui_projecao.R")
source("server_projecao.R")

source("ui_agua_esgoto.R")
source("server_agua_esgoto.R")


rlog::log_info("Starting")

# checking data folder
check_and_create_datasets()

painelDashboard <- tabPanel(
  "Dashboard",
  icon = icon("chart-line"),
  dashboard_ui("projecao")
)

painelProjecao <- navbarMenu(
  "Projeção Populacional",
  icon = icon("users"),
  tabPanel("Fazer Projeção", projecao_populacional_ui("projecao"))
)


painelAguaEsgoto <- navbarMenu(
  "Água e Esgoto",
  icon = icon("faucet"),
  tabPanel("Módulo Demográfico", modulo_demografico_ui("demografico")),
  tabPanel("Módulo Orçamentário", modulo_orcamentario_ui("orcamentario")),
  tabPanel("Módulo Financeiro", modulo_financeiro_ui("financeiro")),
  tabPanel("Resumo")
)


painelResiduosSolidos <-
  tabPanel("Resíduos Sólidos",
           icon = icon("recycle"),
           fluid = TRUE)


painelDrenagemUrbana <-
  tabPanel("Drenagem Urbana",
           icon = icon("water"),
           fluid = TRUE)


painelConfig <- navbarMenu("Configurações",
                           icon = icon("cog"),
                           tabPanel("Atualização de dados"),
)


ui <- fluidPage(
  navbarPage(
    "InvestSan",
    theme = shinytheme("simplex"),
    painelDashboard,
    painelProjecao,
    painelAguaEsgoto,
    painelResiduosSolidos,
    painelDrenagemUrbana,
    painelConfig,
  )
)


server <- function(input, output, session) {
  rlog::log_info("Started new session")
  projecao_server("projecao")
  demografico_server("demografico")
  orcamentario_server("orcamentario")
  financeiro_server("financeiro")
}

shinyApp(ui = ui, server = server)
