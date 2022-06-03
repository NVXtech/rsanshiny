dashboard_ui <- function(id) {
  ns <- NS(id)
  fluidPage(tabsetPanel(
    tabPanel("Nacional", "Dashboard Nacional"),
    tabPanel("Regional", "Regional"),
    tabPanel("Estado", "UF")
  ))
}
