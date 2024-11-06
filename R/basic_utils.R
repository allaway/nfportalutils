#' Create copy of entity
#'
#' Create a copy of syn entity; mostly used to create a copy on which to test out changes.
#' See https://python-docs.synapse.org/build/html/synapseutils.html?highlight=copy#synapseutils.copy_functions.copy
#' @param entity Entity to copy.
#' @param destination_id Id of destination project/container that entity will be copied to.
#' @param skip_copy_wiki_page Whether to skip copying wiki; defaults FALSE.
#' @param skip_copy_annotations Whether to skip copying annotations; defaults FALSE.
#' @keywords internal
copy <- function(entity,
                 destination_id,
                 skip_copy_wiki_page = FALSE,
                 skip_copy_annotations = FALSE) {

  .check_login()
  # load synapseutils as needed


  synapseutils$copy(.syn,
                    entity = entity,
                    destinationId = destination_id,
                    skipCopyWikiPage = skip_copy_wiki_page,
                    skipCopyAnnotations = skip_copy_annotations)

}


#' Download and read file to `data.table`
#'
#' Convenience function for reading a delimited local file or one on Synapse.
#'
#' @param file File Synapse id or local path.
#' @keywords internal
#' @import data.table
dt_read <- function(file) {
  if(file.exists(file)) {
    path <- file
  } else if(grepl("^syn", file)) {
    message("Getting file from Synapse...")
    .check_login()
    path <- .syn$get(file)$path
  } else {
    stop("File must be local file or accessible synapse file.")
  }
  dt <- data.table::fread(path)
  return(dt)
}

#' Extract synapse id from URI or other string
#'
#' @param uri URI or string containing embedded Synapse id.
#' @keywords internal
bare_syn_id <- function(uri) {
  not_na <- which(!is.na(uri))
  x <- uri[not_na]
  syn <- regmatches(x, regexpr("syn[0-9]{8,12}", x))
  uri[not_na] <- syn
  return(uri)
}

#' Validate a Synapse ID
#'
#' Returns the id if valid, throws error if not.
#' @param id Id string.
#' @keywords internal
is_valid_syn_id <- function(id) {
  result <- grepl("^syn[0-9]{4,}$", id)
  result
}

#' Walk through a directory
#'
#' For now, an internal util imported from `synapseutils`.
#' @param syn_id Synapse id of directory root to traverse.
#' @param as_list Whether to return as R list.
#' @return An R list or Py generator object.
#' @keywords internal
walk <- function(syn_id, as_list = TRUE) {
  .check_login()
  x <- synapseutils$walk(.syn, syn_id)
  if(as_list) reticulate::iterate(x) else x

}

