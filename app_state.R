source("data_helpers.R")

#' Carrega o estado do aplicativo
#' caso nunca tenha sido aberto
#' cria um estado padrão
#'
#' @return estado do aplicativo
#' @exporte
#'
#' @examples
#' app_state <- load_app_state()
load_app_state <- function() {
  load(file = app_state_filename())
  return(app_state)
}

get_default_app_state <- function() {
  default_projecao_state <-
    list(
      fonte1 = "populacao_censo_2010",
      fonte2 = "populacao_estimada_2021",
      modelar_ate = 2033,
      resultado = data.frame()
    )
  default_modulo_demografico <-
    list(
      snis = "snis_2020",
      ano = 2033,
      meta_agua = 99,
      meta_esgoto = 90,
      proporcao = 80,
      resultado = data.frame()
    )
  default_modulo_orcamentario <-
    list(
      sinapi = "sinapi_2021_12",
      perda_agua = 25,
      resultado = list(
        distribuicao = data.frame(),
        coleta = data.frame(),
        producao = data.frame(),
        tratamento = data.frame(),
        custo = data.frame(),
      )
    )
  default_modulo_financeiro <-
    list(
      snis = "snis_2020",
      vida_util = 30,
      resultado = data.frame()
    )
  default_state <-
    list(
      projecao = default_projecao_state,
      modulo_demografico = default_modulo_demografico,
      modulo_orcamentario = default_modulo_orcamentario,
      modulo_financeiro = default_modulo_financeiro
    )
  return(default_state)
}

app_state_exists <- function() {
  file.exists(app_state_filename())
}

app_state_filename <- function() {
  file.path(data_folder, state_file_name)
}

check_and_create_data_folder <- function() {
  if (!file.exists(data_folder)) {
    dir.create(data_folder)
  }
}

load_app_state_or_get_defaults <- function() {
  if (app_state_exists()) {
    return(load_app_state())
  }
  check_and_create_data_folder()
  return(get_default_app_state())
}

save_projecao_state <- function(input) {
  rlog::log_info("saving projecao state")
  projecao_state <-
    list(
      fonte1 = input$fonte1,
      fonte2 = input$fonte2,
      modelar_ate = input$ano,
      resultado = app_state$projecao$resultado
    )
  app_state$projecao <- projecao_state
  save(app_state, file = app_state_filename())
  return(app_state)
}

save_modulo_demografico_state <- function(input) {
  rlog::log_info("saving modulo demografico state")
  modulo_demografico <-
    list(
      snis = input$snis,
      ano = input$ano,
      meta_agua = input$meta_agua,
      meta_esgoto = input$meta_esgoto,
      proporcao = input$proporcao,
      resultado = app_state$modulo_demografico$resultado
    )
  app_state$modulo_demografico <- modulo_demografico
  save(app_state, file = app_state_filename())
  return(app_state)
}

save_modulo_orcamentario_state <- function(input) {
  rlog::log_info("saving modulo orcamentario state")
  modulo_orcamentario <-
    list(
      sinadi = input$sinapi,
      fator_servicos = input$fator_servicos,
      fator_materiais = input$fator_materiais,
      fator_composicao = input$fator_composicao,
      fator_insumo = input$fator_insumo,
      perda_agua = input$perda_agua,
      resultado = app_state$modulo_orcamentario$resultado
    )
  app_state$modulo_orcamentario <- modulo_orcamentario
  save(app_state, file = app_state_filename())
  return(app_state)
}


save_modulo_financeiro_state <- function(input) {
  rlog::log_info("saving modulo financeiro state")
  modulo_financeiro <-
    list(
      snis = input$snis,
      vida_util = input$vida_util,
      resultado = app_state$modulo_financeiro$resultado
    )
  app_state$modulo_financeiro <- modulo_financeiro
  save(app_state, file = app_state_filename())
  return(app_state)
}

rlog::log_info("Loading App State")
app_state <- load_app_state()
