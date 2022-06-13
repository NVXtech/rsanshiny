data_folder <- "data"
state_file_name <- "app_state.rda"

#' get dataset path
#'
#' @param name The variable name
#'
#' @return path to dataset .rda file
#' @export
#'
#' @examples
#' path <- get_data_path("populacao")
get_data_path <- function(name) {
  file.path(data_folder, sprintf("%s.rda", name))
}

#' get dataset dir
#'
#' @return The directory where dataset is stored
#' @export
#'
#' @examples
#' path <- get_data_path("populacao")
get_data_dir <- function() {
  return(data_folder)
}

#' load app dataset
#'
#' @param name The variable name
#'
#' @return the content of the dataset
#' @export
#'
#' @examples
#' populacao <- load_data("populacao")
load_data <- function(name) {
  tmp <- new.env()
  load(file = get_data_path(name), envir = tmp)
  tmp[[ls(tmp)[1]]]
}

#' check dataset file exists
#'
#' @param name The variable name
#'
#' @return If file exists (boolean)
#' @export
#'
#' @examples
#' check_data_exists("populacao")
check_data_exists <- function(name) {
  return(file.exists(file.path(data_folder, sprintf("%s.rda", name))))
}
