#' @title Append study column with study name
#'
#' @description Append column named 'study' to data, with all values as the
#'   study name. Does not change the data if a 'study' column already exists.
#'
#' @param data Data frame or tibble.
#' @param name Name of the study.
#' @return Tibble with the data and appended study column.
append_column_study <- function(data, name) {
  if (!("study" %in% names(data))) {
    data <- tibble::add_column(data, study = name)
  } else {
    stop("study column already exists")
  }
  data
}
