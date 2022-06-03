library(lubridate)

states <- rsan::states_acronym()

get_last_month_and_year <- function(){
  last_date <- floor_date(as.Date(Sys.Date()), "month") - months(1)
  return(list(
    year=substr(last_date,1,4),
    month=substr(last_date,6,7)
    ))
}

download_sinapi <- function(year, month){
  type <- "NaoDesonerado"
  inputs <- tibble()
  compositions <- tibble()
  for (state in states) {
    url <- paste0("https://www.caixa.gov.br/Downloads/sinapi-a-partir-jul-2009-", tolower(state),
                  "/SINAPI_ref_Insumos_Composicoes_", state, "_", month, year, "_", type, ".zip")
    tmp_file <- tempfile()
    tmp_dir <- tempDir()
    curl::curl_download(url, tmp_file)
    unzip(tmp_file, exdir = tmp_dir)

    unlink(tmp_file)
    unlink(tmp_dir)
  }
}


create_sinapi <- function() {
  data("sinapi202112")
  df_snis <- sinapi202112
  save(df_snis, file=get_data_path("sinapi_202112"))

  nome <- c("SINAPI 2021-12")
  ano <- c(2020)
  caminho <- c("sinapi_202112")
  sinapi <- data.frame(nome, ano, caminho)

  save(sinapi, file=get_data_path("sinapi"))
}

integrity_sinapi <- function() {
  pop <- load_data("sinapi")
  for (caminho in pop$caminho) {
    if (!file.exists(get_data_path(caminho))) {
      rlog::log_info(sprintf("%s dataset not found", caminho))
      return(FALSE)
    }
    else{
      rlog::log_info(sprintf("%s dataset is OK", caminho))
    }
  }
  return(TRUE)
}

