source("data_helpers.R")
source("data_ibge_populacao.R")
source("data_ibge_municipio.R")
source("data_snis.R")
source("data_sinapi.R")
source("data_projeto.R")


datasets = c("populacao", "municipio", "snis", "sinapi", "projeto")

create_functions = list("populacao" = create_populacao,
                        "municipio" = create_municipio,
                        "snis" = create_snis,
                        "sinapi" = create_sinapi,
                        "projeto" = create_projeto
                        )

integrity_functions = list("populacao" = integrity_populacao,
                           "municipio"= integrity_municipio,
                           "snis"= integrity_snis,
                           "sinapi"= integrity_sinapi,
                           "projeto" = integrity_projeto
                           )

dataset_exists <- function(name) {
  file.exists(get_data_path(name))
}

create_dataset <- function(name) {
  rlog::log_info(sprintf("Creating %s dataset..", name))
  create_functions[[name]]()
}

clean_dataset <- function(name) {
  rlog::log_info(sprintf("Cleaning %s dataset..", name))
  files <- list.files(get_data_dir())
  for (file in files) {
    if (grepl(sprintf("^%s.*", name), file))
      base::unlink(file.path(get_data_dir(), file))
  }
}

check_dataset_integrity <- function(name) {
  rlog::log_info(sprintf("Checking %s dataset..", name))
  return(integrity_functions[[name]]())
}

check_and_create_datasets <- function() {
  for (dataset in datasets) {
    if (!dataset_exists(dataset)) {
      create_dataset(dataset)
    }
    else{
      if (!check_dataset_integrity(dataset)) {
        logging::logwarn(sprintf("Data integrity checked failed for %s", dataset))
        #clean_dataset(dataset)
        #create_dataset(dataset)
      }
    }

  }
}
