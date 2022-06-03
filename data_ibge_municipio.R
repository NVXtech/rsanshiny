# Codigo Estados e Municípios
library(jsonlite)

municipios_url <-
  "https://servicodados.ibge.gov.br/api/v1/localidades/municipios?view=nivelado"

#' Cria dataset com informações gerais
#' sobre os munícipios do Brasil
#' regiões, estado e códigos do IBGE
#'
#' @return
#' @export
#'
#' @examples
#' create_municipio()
create_municipio <- function() {
  res <- GET(municipios_url)
  municipio <- jsonlite::fromJSON(rawToChar(res$content))
  labels <- c("codigo_municipio", "municipio", "codigo_microregiao", "microregiao", "codigo_mesoregiao", "mesoregiao", "codgio_regiao_imediata", "regiao_imediata", "codigo_regiaointermediaria",  "regiaintermediaria", "codigo_UF", "UF", "Estado", "codigo_regiao", "regiao_sigla", "regiao")
  names(municipio) <- labels
  save(municipio, file=get_data_path("municipio"))
}

integrity_municipio <- function(){
  # TODO: create an better integrity check
  exists <- check_data_exists("municipio")
  if (exists)
    rlog::log_info("municipio dataset is OK")
  else
    rlog::log_warn("integrity check failed to municipio dataset")
  return(exists)
}
