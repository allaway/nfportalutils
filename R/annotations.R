#' Wrapper around the Python `set_annotations` that pulls current annotations
#' and adds new annotations with given annotations data or replaces
#' data for annotations with the same keys existing on the entity.
#' @param id Synapse entity id.
#' @param annotations A flat list representing annotation key-value pairs,
#' e.g. `list(foo = "bar", rank = 1, authors = c("jack", "jane"))`
#' @export
set_annotations <- function(id, annotations) {
  e_annotations <- .syn$get_annotations(id)
  for (k in names(annotations)) {
    e_annotations[k] <- annotations[[k]]
  }
  .syn$set_annotations(e_annotations)
}

#' Set annotations from a manifest
#'
#' The [Synapse docs](https://help.synapse.org/docs/Managing-Custom-Metadata-at-Scale.2004254976.html)
#' suggest doing batch annotations through a fileview. However, it is often simpler to
#' modify or set new annotations directly given a table of just the entities (rows) and props (cols) we want.
#' This is like how schematic works, except without any validation (so works best for power-users who know the data model well).
#' Some desired defaults are taken into account, such as not submitting key-values with `NA` and empty strings.
#'
#' @param manifest A `data.frame` representing a manifest.
#' Needs to contain `entityId` (if parsed from a standard manifest.csv, the df should already contain `entityId`).
#' @param ignore_na Whether to ignore annotations that are `NA`; default TRUE.
#' @param ignore_blank Whether to ignore annotations that are that empty strings; default TRUE.
#' @param verbose Be chatty, default FALSE.
#' @export
annotate_with_manifest <- function(manifest, ignore_na = TRUE, ignore_blank = TRUE, verbose = FALSE) {
  # Split by `entityId`
  annotations <- as.data.table(manifest)
  if("Filename" %in% names(annotations)) annotations[, Filename := NULL]
  annotations[, entityId := as.character(entityId)]
  annotations <- split(annotations, by = "entityId", keep.by = FALSE)
  filterNA <- if(ignore_na) function(x) !any(is.na(x)) else TRUE # will ignore entirely if list with NA, e.g. c(NA, 1, 2) -- should warn if list
  filterBlank <- if(ignore_blank) function(x) !any(x == "") else TRUE # same as above
  annotations <- lapply(annotations, function(x) Filter(function(x) filterNA(x) & filterBlank(x) & length(x), unlist(x, recursive = F)))
  for(entity_id in names(annotations)) {
    set_annotations(entity_id, annotations[[entity_id]])
  }
  if (verbose) message("Annotations submitted")
}


#' Copy annotations
#'
#' Copy annotations (all or selectively) from a source entity to one or more target entities.
#' If same annotation keys already exist on target entities, the copy will replace the current values.
#'
#' @param entity_from Syn id from which to copy.
#' @param entity_to One or more syn ids to copy annotations to.
#' @param select Vector of properties to selectively copy if present on the entity.
#' If not specified, will copy over everything, which may not be desirable.
#' @param update Whether to immediately update or return annotation objects only.
#' @export
copy_annotations <- function(entity_from,
                             entity_to,
                             select = NULL,
                             update = FALSE) {

  .check_login()

  from_annotations <- .syn$get_annotations(entity_from)
  # Check `select`
  if(is.null(select)) {
    select <- names(from_annotations)
  } else {
    select <- select[select %in% names(from_annotations)]
  }

  for(id in entity_to) {
    to_annotations <- .syn$get_annotations(id)
    for(k in select) {
      to_annotations[k] <- from_annotations[k]
    }
    if(update) .syn$set_annotations(to_annotations) else return(to_annotations)
  }
}

