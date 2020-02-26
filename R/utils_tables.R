#' @title Remove NA rows
#'
#' @description Removes NA rows. If columns is not `NULL`, then will only remove
#' rows in which all columns given have `NA`. If columns is `NULL`, then will
#' remove rows that are `NA` across all columns.
#'
#' @param data Data frame with metadata
#' @param columns The columns to compare for `NA` values.
#' @return The data without rows that are `NA` across the given columns.
#' @examples
#' dat <- data.frame(
#'   foo = c("bar0", NA, NA, "bar3"),
#'   bar = c(NA, "foo1", NA, "foo3")
#' )
#' dat1 <- remove_na_rows(dat)
#' dat2 <- remove_na_rows(dat, "foo")
#' dat3 <- remove_na_rows(dat, c("foo", "bar"))
remove_na_rows <- function(data, columns = NULL) {
  if (is.null(columns)) {
    na_rows <- apply(data, 1, function(x) {
      all(is.na(x))
    })
    data[!na_rows, ]
  } else {
    na_in_cols <- purrr::map(columns, function (x) {
      which(is.na(data[x]))
    })
    na_rows <- Reduce(intersect, na_in_cols)
    data[-na_rows, ]
  }
}

#' @title Save table to Synapse
#'
#' @description Save table to Synapse.
#'
#' @param data Data frame with metadata
#' @param table_id synID for the table. If `NULL`, assumes the table needs to be
#'   created first.
#' @param project_id synID for the project the table should be uploaded to, if
#'   table needs to be created.
#' @param name Desired table name, if table needs to be created.
#' @param ... Additional parameters sent to [synapser::synStore()]
#' @return Synapse table entity
save_table_to_synapse <- function(data, table_id = NULL, project_id = NULL,
                                  name = NULL, ...) {
  if (is.null(table_id)) {
    table <- synapser::synBuildTable(name, project_id, data)
    synapser::synStore(table)
  } else {
    table <- synTableQuery(sprintf("select * from %s", table_id))
    table_df <- as.data.frame(table)
    # Won't work correctly once data is up
    table_df <- dplyr::left_join(all_studies_metadata_joined, table_df)
    # Rearrange rows
    table_df <- table_df[, c("ROW_ID", "ROW_VERSION", "study", "individualID", "specimenID", "assay", "species")]
    table_new <- Table(table_id, table_df)
    table <- synStore(table_new)
  }
}
