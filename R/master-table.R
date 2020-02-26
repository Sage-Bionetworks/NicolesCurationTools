#' @title Append study column with study name
#'
#' @description Append column named 'study' to data, with all values as the
#'   study name. Does not change the data if a 'study' column already exists.
#'
#' @param data Data frame or tibble.
#' @param name Name of the study.
#' @return Tibble with the data and appended study column.
#' @examples
#' dat <- data.frame(
#'   foo = c("bar0", "bar1", "bar2"),
#'   bar = c("foo0", "foo1", "foo2")
#' )
#' study_dat <- append_column_study(dat, "cool_study")
#' # Now has study column so cannot change study column
#' # value with this function
#' study_dat <- append_column_study(study_dat, "neato_study")
append_column_study <- function(data, name) {
  if (!("study" %in% names(data))) {
    data <- tibble::add_column(data, study = name)
  } else {
    stop("study column already exists")
  }
  data
}

#' @title Get all metadata folder information
#'
#' @description Get all metadata folder information.
#'  Synapse directory structure is expected to be:
#'   parent_folder
#'     |_study_folder
#'     |   |_ metadata_folder
#'     |        |_ metadata_file_1
#'     |        |_ metadata_file_2
#'     |_study_folder
#'         |_ metadata_folder
#'              |_ metadata_file_3
#'
#' @param parent synID for the parent folder.
#' @return Dataframe with the metadata folder information.
get_all_metadata_folder_info <- function(parent) {
  studies <- get_children(parent)
  metadata_folders <- purrr::map_dfr(
    studies,
    function(study) {
      get_study_metadata_folder_info(study, study$name)
    }
  )

}

#' @title Get all metadata folder information helper
#'
#' @description Get study metadata folder information. Helper function for
#' [get_all_metadata_folder_info()]. Assumes the directory structure looks like:
#'     study_folder
#'       |_ metadata_folder
#'           |_ metadata_file_1
#'           |_ metadata_file_2
#'
#' @param study The synID for the study folder.
#' @param study_name Name of the study.
#' @return Dataframe with the metadata folder information.
get_study_metadata_folder_info <- function(study, study_name) {
  study_children <- get_children(study$id)
  # Check if there were any subfolders in the study folder
  if (!is.null(study)) {
    is_metadata_folder <- purrr::map_lgl(
      study_children,
      function(sub_folder) {
        sub_folder$name == "Metadata"
      }
    )
    metadata_folder <- study_children[is_metadata_folder]
    if (length(metadata_folder) == 0) {
      return(NULL)
    } else {
      metadata_folder <- tibble::enframe(unlist(metadata_folder))
      metadata_folder <- tibble::add_row(
        metadata_folder,
        name = "study",
        value = study_name
      )
      metadata_folder <- tidyr::spread(
        metadata_folder,
        key = name,
        value = value
      )
    }
  } else {
    return(NULL)
  }
  metadata_folder
}

#' @title Get metadata file information
#'
#' @description Get metadata file information. Assumes the directory structure
#' looks like:
#'    metadata_folder
#'      |_ metadata_file_1
#'      |_ metadata_file_2
#'
#' @param folder_id synID for the metadata folder.
#' @param study_name Name of the study.
#' @return Dataframe with all the metadata file information.
get_metadata_file_info <- function(folder_id, study_name) {
  if (is.null(folder_id)) {
    return(NULL)
  }
  children <- get_children(folder_id)
  if (length(children) > 0) {
    children_df <- purrr::map_dfr(
      children,
      function(child) {
        file_info <- tibble::enframe(unlist(child))
        file_info <- tibble::add_row(
          file_info,
          name = "study",
          value = study_name
        )
        file_info <- tidyr::spread(
          file_info,
          key = name,
          value = value
        )
      }
    )
    return(children_df)
  } else {
    return(NULL)
  }
}

#' @title Create the master table
#'
#' @description Create the master table with the given keys as columns.
#'
#' @param study_view Data frame with metadata file information, including
#'   column names 'id' and 'name'.
#' @param study_name Name of the study.
#' @param keys The names of the columns that are wanted in the table.
#' @return Data frame with master table data.
create_study_table <- function(study_view, study_name, keys) {
  all_files_annots <- purrr::map2(
    study_view$id,
    study_view$name,
    function(id, name) {
      get_metadata_file_and_annotations(id, name, keys)
    }
  )
  not_null <- purrr::map_lgl(all_files_annots, function(x) {!is.null(x)})
  all_files_annots <- all_files_annots[not_null]
  if (length(all_files_annots) > 0) {
    has_both_ids <- purrr::map_lgl(all_files_annots, function (x) {
      all(c("individualID", "specimenID") %in% names(x))
    })
    new_order <- c(which(has_both_ids), which(!has_both_ids))
    if (length(new_order) > 0) {
      all_files_annots <- all_files_annots[new_order]
    }
    study_metadata <- Reduce(dplyr::full_join, all_files_annots)
    study_metadata <- append_column_study(study_metadata, study_name)
    study_metadata <- remove_na_rows(study_metadata)
    study_metadata
  } else if (length(all_files_annots) == 0) {
    study_metadata <- all_files_annots[1]
    study_metadata <- append_column_study(study_metadata, study_name)
    remove_na_rows(study_metadata)
  } else {
    study_metadata <- NULL
  }
  study_metadata
}

#' @title Get data and annotations from metadata file
#'
#' @description Get data and annotations from metadata file.
#'
#' @param file_id Synapse ID for the metadata file.
#' @param file_name Name of metadata file.
#' @param keys The names of the columns that are wanted in the table.
#' @return Named list where `metadata` refers to a dataframe with the file's
#'   metadata, and `annotations` refers to a tibble with the file's non-null
#'   annotations
get_metadata_file_and_annotations <- function(file_id, file_name, keys) {
  file_info <- synapser::synGet(file_id)
  metadata <- get_file_data(file_info$path)
  if (is.null(metadata)) {
    return(NULL)
  }
  annots <- dict_to_list(file_info$annotations)
  annots <- annots[!purrr::map_lgl(annots, function(x) {is.null(x)})]
  annots <- tibble::as_tibble(annots)
  if ("assay" %in% names(annots) && !("assay" %in% names(metadata))) {
    metadata <- tibble::add_column(metadata, assay = annots$assay)
  }
  keys_present <- keys[keys %in% names(metadata)]
  if (length(keys_present) > 0) {
    metadata <- metadata[, keys_present]
    metadata
  } else {
    return(NULL)
  }
}

#' @title Generate master table for model
#'
#' @description Generate master table for model based on metadata files.
#' Synapse directory structure is expected to be:
#'   parent_folder
#'     |_study_folder
#'     |   |_ metadata_folder
#'     |        |_ metadata_file_1
#'     |        |_ metadata_file_2
#'     |_study_folder
#'         |_ metadata_folder
#'              |_ metadata_file_3
#'
#' @param parent Parent synID that holds study folders for a model.
#' @param keys The names of the columns that are wanted in the table.
#' @return The master data table.
generate_model_master_table <- function(parent, keys = c("individualID",
                                                         "specimenID",
                                                         "assay",
                                                         "species")) {
  metadata_folders <- get_all_metadata_folder_info(parent)
  # Get all metadata file information
  metadata_files <- purrr::map2_dfr(
    metadata_folders$id,
    metadata_folders$study,
    function(id, study){
      get_metadata_file_info(id, study)
    }
  )
  all_study_names <- unique(metadata_files$study)
  # For each study, go through the files and grab the metadata info
  # Only grab the columns that are wanted.
  all_studies_metadata <- purrr::map(all_study_names, function(study_name) {
    study_files <- metadata_files[metadata_files$study == study_name, ]
    study_metadata <- create_study_table(
      study_files,
      study_name,
      keys = keys
    )
    study_metadata
  })
  all_studies_metadata_joined <- Reduce(dplyr::full_join, all_studies_metadata)
  # Remove all rows that do not have either specimenID nor individualID
  all_studies_metadata_joined <- remove_na_rows(
    all_studies_metadata_joined,
    c("specimenID", "individualID")
  )
}


