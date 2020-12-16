#' @title Get subfolders as list
#'
#' @description Get the immediate subfolders in a parent folder.
#'
#' @param parent_id synID of parent folder 
#' @return list of subfolders in parent folder
get_subfolders_as_list <- function(parent_id) {
  if(is.null(parent_id) || is.na(parent_id)) {
    return(NA)
  }
  synapser::synGetChildren(
    parent = parent_id,
    includeTypes = list("folder")
  )$asList()
}

#' @title Get files as list
#'
#' @description Get a list of the files within a Synapse parent folder.
#'
#' @export
#' @param parent The synID of the parent folder.
#' @return Synapse entity list with child files.
#' @examples
#' \dontrun{
#' library(synapser)
#' synLogin()
#' get_children("syn123456")
#' }
get_files_as_list <- function(parent) {
  children <- synapser::synGetChildren(parent, includeTypes = list("file"))
  children$asList()
}

#' @title Move a Synapse folder or file to new parent folder
#'
#' @description Move a Synapse folder or file to new parent folder.
#'
#' @export
#' @param id synID of the folder or file to move.
#' @param parent_id synID of the new folder to move the folder or file to.
#' @return The Synapse entity that was stored in a new location.
move_folder_file <- function(id, parent_id) {
  entity <- synapser::synGet(id)
  entity$properties$parentId <- parent_id
  synapser::synStore(entity)
}
