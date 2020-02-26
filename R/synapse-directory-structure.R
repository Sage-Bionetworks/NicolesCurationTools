#' @title Get children of Synapse folder
#'
#' @description Get a list of the child folders/files within a Synapse parent
#' folder.
#'
#' @param parent The synID of the parent folder.
#' @return Synapse entity list with child folders/files.
#' @examples
#' \dontrun{
#' library(synapser)
#' synLogin()
#' get_children("syn123456")
#' }
get_children <- function(parent) {
  children <- synapser::synGetChildren(parent)
  children$asList()
}

#' @title Move a Synapse folder or file to new parent folder
#'
#' @description Move a Synapse folder or file to new parent folder.
#'
#' @param id synID of the folder or file to move.
#' @param parent_id synID of the new folder to move the folder or file to.
#' @return The Synapse entity that was stored in a new location.
move_folder_file <- function(id, parent_id) {
  entity <- synapser::synGet(id)
  entity$properties$parentId <- parent_id
  synapser::synStore(entity)
}
