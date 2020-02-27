#' @title Log into Synapse
#'
#' @description Logs user into Synapse.
#'
#' @export
#' @param ... Additional parameters sent on to [synapser::synLogin()]
#' @return NULL
log_into_synapse <- function(...) {
  synapser::synLogin(...)
}

#' @title Get data from file
#'
#' @description Get data from file. File types handled are:
#'   - csv
#'   - tsv
#'   - txt (assumed to be tab delimited)
#'   - xlsx (assumed to be single sheet)
#'   - xls (assumed to be single sheet)
#'
#' @export
#' @param path File path
#' @return A data frame with the file data
get_file_data <- function(path) {
  accepted_ext <- c("csv", "tsv", "txt", "xlsx", "xls")
  ext <- tools::file_ext(path)
  if (is.null(path) || is.null(ext) || !(ext %in% accepted_ext)) {
    return(NULL)
  }
  switch(
    tools::file_ext(path),
    csv = readr::read_csv(path, col_types = readr::cols(.default = "c")),
    tsv = readr::read_tsv(path, col_types = readr::cols(.default = "c")),
    txt = readr::read_tsv(path, col_types = readr::cols(.default = "c")),
    xlsx = readxl::read_xlsx(path, col_types = "text"),
    xls = readxl::read_xls(path, col_types = "text")
  )
}

#' @title Convert Synapse annotation dictionary to a list
#'
#' @description Convert Synapse annotation dictionary to a list.
#'
#' @export
#' @param annotations Annotations dictionary from Synapse.
#' @return List of annotations.
dict_to_list <- function(annotations) {
  values <- purrr::map(names(annotations), function(y) annotations$get(y))
  stats::setNames(values, names(annotations))
}
