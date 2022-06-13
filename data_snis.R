
create_snis <- function() {
  data("snis2020")
  df_snis <- snis2020
  save(df_snis, file = get_data_path("snis_2020"))

  nome <- c("SNIS 2020")
  ano <- c(2020)
  caminho <- c("snis_2020")
  snis <- data.frame(nome, ano, caminho)

  save(snis, file = get_data_path("snis"))
}


integrity_snis <- function() {
  pop <- load_data("snis")
  for (caminho in pop$caminho) {
    if (!file.exists(get_data_path(caminho))) {
      rlog::log_info(sprintf("%s dataset not found", caminho))
      return(FALSE)
    } else {
      rlog::log_info(sprintf("%s dataset is OK", caminho))
    }
  }
  return(TRUE)
}
