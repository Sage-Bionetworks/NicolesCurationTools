#' @title Log into Synapse
#'
#' @description Logs user into Synapse.
#'
#' @params Additional parameters sent on to [synapser::synLogin()]
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
#' @param path File path
#' @return A data frame with the file data
get_file_data <- function(path) {
  accepted_ext <- c("csv", "tsv", "txt", "xlsx", "xls")
  ext <- tools::file_ext(path)
  # browser()
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
