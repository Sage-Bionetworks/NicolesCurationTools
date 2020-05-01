#' @title Remove NA rows
#'
#' @description Removes NA rows. If columns is not `NULL`, then will only remove
#' rows in which all columns given have `NA`. If columns is `NULL`, then will
#' remove rows that are `NA` across all columns.
#'
#' @export
#' @param data Data frame with metadata
#' @param columns The columns to compare for `NA` values.
#' @return The data without rows that are `NA` across the given columns.
#' @examples
#' \dontrun{
#' dat <- data.frame(
#'   foo = c("bar0", NA, NA, "bar3"),
#'   bar = c(NA, "foo1", NA, "foo3")
#' )
#' dat1 <- remove_na_rows(dat)
#' dat2 <- remove_na_rows(dat, "foo")
#' dat3 <- remove_na_rows(dat, c("foo", "bar"))
#' }
remove_na_rows <- function(data, columns = NULL) {
  if (is.null(columns)) {
    na_rows <- apply(data, 1, function(x) {
      all(is.na(x))
    })
    data[!na_rows, ]
  } else {
    na_in_cols <- purrr::map(columns, function(x) {
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
#' @export
#' @param data Data frame with metadata
#' @param table_id synID for the table. If `NULL`, assumes the table needs to be
#'   created first.
#' @param project_id synID for the project the table should be uploaded to, if
#'   table needs to be created.
#' @param name Desired table name, if table needs to be created.
#' @param overwrite If overwrite is `TRUE`, the table contents will be deleted
#'   and the new data uploaded in its place. Note that the schema will not
#'   be updated so the new data must follow the same schema. If `FALSE`, the
#'   new data will be joined to the current table.
#' @param ... Additional parameters sent to [synapser::synStore()]
#' @return Synapse table entity
save_table_to_synapse <- function(data, table_id = NULL, project_id = NULL,
                                  name = NULL, overwrite = FALSE, ...) {
  if (is.null(table_id)) {
    table <- synapser::synBuildTable(name, project_id, data)
    synapser::synStore(table)
  } else {
    # Allow for either overwriting the table or adding to table
    if (overwrite) {
      table <- synapser::synTableQuery(sprintf("select * from %s", table_id))
      newtable <- synapser::Table(table_id, data)
      synapser::synDelete(table)
      synapser::synStore(newtable)
    } else {
      table <- synapser::synTableQuery(sprintf("select * from %s", table_id))
      table_df <- table$asDataFrame()
      table_df <- dplyr::full_join(table_df, data)
      table_new <- synapser::Table(table_id, table_df)
      table <- synapser::synStore(table_new, ...)
    }
  }
}
