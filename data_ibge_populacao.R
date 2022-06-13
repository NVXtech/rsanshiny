#
# Funções auxiliarias para criar os datasets do IBGE
#


source("data_helpers.R")

# CONSTANTS --------------------------------------------------------------------

# FTP dados de população

# Censo
censo_url <- "ftp.ibge.gov.br/Censos/"
censo_suffix <- "Censo_Demografico_%s/resultados/"
censo_file <- "^total_populacao_.*.zip"

# Estimativas de população
estimativa_pop_url <- "ftp.ibge.gov.br/Estimativas_de_Populacao/"
estimativa_pop_suffix <- "/Estimativas_%s/"
estimativa_pop_file <- "estimativa_dou_%s.xls"

# FUNÇÕES ----------------------------------------------------------------------

#' List of files/folders in a FTP folder
#'
#' @param URL
#'
#' @return list of files and folders
#' @export
#'
#' @examples
#' listFilesFromFTP("ftp.ibge.gov.br")
#' listFilesFromFTP("ftp.ibge.gov.br/Censos/")
listFilesFromFTP <- function(URL) {
  listFiles <- curl::new_handle()
  curl::handle_setopt(listFiles, ftp_use_epsv = TRUE, dirlistonly = TRUE)
  con <-
    curl::curl(url = paste0("ftp://", URL), "r", handle = listFiles)
  files <- readLines(con)
  close(con)
  files
}


#' Lista de anos de população estimada pelo Censo
#' que estão disponíveis para download no ftp do IBGE
#'
#' @param URL
#'
#' @return lista de anos do censo disponíveis
#' @export
#'
#' @examples
#' getCensoYears()
getCensoYears <- function() {
  files <- listFilesFromFTP(censo_url)
  output <- c()
  for (file in files) {
    if (grepl("^Censo_Demografico.*", file)) {
      only_number <- gsub("Censo_Demografico_", "", file)
      year <- strtoi(only_number)
      output <- c(output, year)
    }
  }
  return(output)
}


#' Cria conjunto de dados do censo IBGE
#'
#' @return Dataframe com informações dos conjuntos de dados baixados
#' @export
#'
#' @examples
#' downloadCensoRawData()
downloadCensoRawData <- function() {
  years <- getCensoYears()
  # Anos anteriores a 2010 não são suportados
  years <- years[years >= 2010]
  col_names <-
    c(
      "codigo_municipio",
      "municipio",
      "populacao_total_anterior",
      "populacao_masculina",
      "populacao_feminina",
      "populacao_urbana",
      "populacao_rural",
      "populacao_total"
    )
  col_types <-
    c(
      "numeric",
      "text",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric",
      "numeric"
    )
  nome <- c()
  ano <- c()
  caminho <- c()
  tipo <- c()
  for (year in years) {
    URL <- paste0(censo_url, sprintf(censo_suffix, year))
    files <- listFilesFromFTP(URL)
    files <- files[grepl(censo_file, files)]
    df_censo <- data_frame()

    for (file in files) {
      url_to_download <- paste0(URL, file)
      destfile <- tempfile()
      tmp_dir <- tempdir()
      curl::curl_download(url_to_download, destfile)
      unzip(destfile, exdir = tmp_dir)
      base::unlink(destfile)
      xls_filename <-
        file.path(tmp_dir, gsub("\\.zip$", ".xls", file))
      df <-
        read_xls(
          xls_filename,
          skip = 1,
          col_names = col_names,
          col_types = col_types
        )
      df_censo <- bind_rows(df_censo, df)
      base::unlink(xls_filename)
    }
    # Remove NA (rodape do xls)
    df_censo <- df_censo[complete.cases(df_censo), ]
    # transforma codigo_municipio to char
    df_censo <-
      transform(df_censo, codigo_municipio = as.character(codigo_municipio))
    filename_out <-
      file.path(data_folder, sprintf("populacao_censo_%s.rda", year))
    save(df_censo, file = filename_out)
    ano <- c(ano, year)
    caminho <- c(caminho, sprintf("populacao_censo_%s", year))
    tipo <- c(tipo, "CENSO")
    nome <- c(nome, sprintf("%s - Censo", year))
  }
  return(data.frame(nome, tipo, ano, caminho))
}


#' Lista de anos disponíveis de estimativas de população por amostra do IBGE
#'
#' @return Lista de anos disponíveis
#' @export
#'
#' @examples
#' load_estimativa_years()
load_estimativa_years <- function() {
  files <- listFilesFromFTP(estimativa_pop_url)
  output <- c()
  for (file in files) {
    if (grepl("^Estimativas_.*", file)) {
      only_number <- gsub("Estimativas_", "", file)
      year <- strtoi(only_number)
      if (year >= 2019) {
        output <- c(output, year)
      }
    }
  }
  return(output)
}

#' Cria conjunto de dados de estimativas populacionais por município do IBGE
#'
#' @return
#' @export
#'
#' @examples
#' download_estimativa_populacao()
download_estimativa_populacao <- function() {
  col_names <-
    c("UF", "codigo_UF", "codigo", "municipio", "populacao_str")
  col_types <- c("text", "text", "text", "text", "text")
  nome <- c()
  ano <- c()
  caminho <- c()
  tipo <- c()
  for (year in c(2021)) {
    # for (year in load_estimativa_years()) {
    URL <- paste0(
      estimativa_pop_url,
      estimativa_pop_suffix,
      estimativa_pop_file
    )
    URL <- sprintf(URL, year, year)
    destfile <- tempfile()
    curl::curl_download(URL, destfile)
    populacao_estimada <-
      read_xls(
        destfile,
        skip = 2,
        sheet = 2,
        col_names = col_names,
        col_types = col_types
      )
    base::unlink(destfile)
    # Arruma populacoes com notas de rodapé e remove separador de milhar
    populacao_estimada$populacao_total <-
      as.numeric(gsub("\\.", "", gsub("\\([^)]*\\)", "", populacao_estimada$populacao_str)))
    # Remove NA (rodape do xls)
    populacao_estimada <-
      populacao_estimada[complete.cases(populacao_estimada), ]
    # Arruma Codigo Municipio
    populacao_estimada$codigo_municipio <-
      paste0(
        populacao_estimada$codigo_UF,
        populacao_estimada$codigo
      )
    populacao_estimada <-
      subset(populacao_estimada, select = -c(codigo, populacao_str))
    filename_out <- file.path(data_folder, sprintf("populacao_estimada_%s.rda", year))
    save(populacao_estimada, file = filename_out)
    ano <- c(ano, year)
    caminho <- c(caminho, sprintf("populacao_estimada_%s", year))
    tipo <- c(tipo, "ESTIMATIVA")
    nome <- c(nome, sprintf("%s - Estimativa", year))
  }
  return(data.frame(nome, tipo, ano, caminho))
}

create_populacao <- function() {
  df_estimativa <- download_estimativa_populacao()
  df_censo <- downloadCensoRawData()
  populacao <- rbind(df_censo, df_estimativa)
  save(populacao, file = file.path(data_folder, "populacao.rda"))
}

clean_populacao <- function() {
  files <- list.files(data_folder)
  for (file in files) {
    if (grepl("^populacao.*", file)) {
      base::unlink(file.path(data_folder, file))
    }
  }
}

integrity_populacao <- function() {
  pop <- load_data("populacao")
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
