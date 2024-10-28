#' Register a NEW project for the NF Data Portal in **Portal - Project View**
#'
#' Add relevant study metadata to the project as annotations.
#' Add to scope of NF-OSI data portal and management in **Portal - Project View**.
#'
#' @param id Synapse id of study.
#' @param study_meta List of annotations representing study meta.
#' @param summary Large summary string.
#' @param study_summary_table Id of where to store summary (can be any table with a `summary` LARGETEXT column).
#' @param portal_project_view View of DCC-managed projects (studies).
#' @export
register_study <- function(id,
                           study_meta,
                           summary,
                           study_summary_table,
                           portal_project_view = "syn52677631") {

    add_new_study_meta(id, study_meta)
    add_study_summary(id, summary, study_summary_table)
    add_to_scope(portal_project_view, id)
    message(glue::glue("Successfully added {id} to DCC study scope!"))
}


#' Register a project's files in **Portal - Files**
#'
#' Add a project to the scope of the **Portal - Files** fileview so that
#' files for that project are "registered" and surfaced in portal.
#'
#' @param project_id The project id, i.e. container, that will be added to the scope of the view.
#' @param portal_fileview Synapse id of "Portal - Files" entity view.
#' @export
register_study_files <- function(project_id,
                                 portal_fileview = "syn16858331") {

  add_to_scope(portal_fileview, project_id)
}


#' Helpers ---------------------------------------------------------------------#

#' Add meta for new studies as annotations
#'
#' Put *selected* metadata into Synapse annotations for the study project entity.
#'
#' @param id Id of container representing the study, usually a Synapse project.
#' @param study_meta Study meta as a list.
#' @keywords internal
add_new_study_meta <- function(id, study_meta) {

  study_meta <- study_meta[names(study_meta) %in% c(
    "studyName",
    "fundingAgency",
    "initiative",
    "studyLeads",
    "institutions",
    "diseaseFocus",
    "manifestation",
    "studyStatus",
    "dataStatus",
    "grantDOI",
    "relatedStudies",
    "studyFileviewId",
    "grantStartDate",
    "grantEndDate",
    "embargoEndDate")]

  if(is.null(study_meta$studyStatus) || is.na(study_meta$studyStatus)) study_meta$studyStatus <- "Active"
  if(is.null(study_meta$dataStatus) || is.na(study_meta$dataStatus)) study_meta$dataStatus <- "Data Pending"

  study <- .syn$setAnnotations(id, study_meta)
  invisible(study)
}


#' Add to scope
#'
#' Convenience function to add container to view scope.
#'
#' @param view_id Id of view
#' @param container_id Id of container to add.
#' @export
add_to_scope <- function(view_id, container_id) {

  view <- .syn$get(view_id)
  new_scope_id <- sub("syn", "", container_id) # should be integer id
  view$add_scope(new_scope_id)
  view <- .syn$store(view)
  invisible(view)
}


#' Add studyId-summary key-value only
#'
#' @keywords internal
add_study_summary <- function(study_id, summary, table_id = "syn16787123") {

  BASE_URL <- "https://repo-prod.prod.sagebase.org/repo/v1"
  url <- glue::glue("{BASE_URL}/entity/{table_id}/table/transaction/async/start")

  studyId_col_model <- "82686"
  summary_col_model <- "82091"

  new_row <- list(etag = NULL,
                  rowId = NULL,
                  values = list(
                    list(key = studyId_col_model, value = study_id), # id val in row
                    list(key = summary_col_model, value = summary) # summary value in row
                  ))

  p_rowset <- list(concreteType = "org.sagebionetworks.repo.model.table.PartialRowSet",
                   tableId = table_id,
                   rows = list(new_row))

  row_update <- list(concreteType = "org.sagebionetworks.repo.model.table.AppendableRowSetRequest",
                     entityId = table_id,
                     toAppend = p_rowset)

  payload <- c(concreteType = "org.sagebionetworks.repo.model.table.TableUpdateTransactionRequest",
               entityId = table_id,
               changes = list(list(row_update)),
               createSnapshot = FALSE,
               snapshotOptions = list(list(snapshotComment = NULL, snapshotLabel = NULL, snapshotActivityId = NULL))) %>%
    jsonlite::toJSON(auto_unbox = TRUE, null = "null", na = "null")

  key <- .syn$credentials$secret
  response <- httr::POST(url, httr::add_headers(Authorization = paste("Bearer",key)),
                         body = payload, httr::content_type_json())

  if(httr::status_code(response) %in% c(201L, 200L)) {
    token <- httr::content(response)$token
    Sys.sleep(1)
    url_check <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/asynchronous/job/{token}")
    job_get <- httr::GET(url_check, httr::add_headers(Authorization = paste("Bearer",key)))
    status <- httr::status_code(job_get)
    if(status == 200L) message(glue::glue("Successfully submitted summary for {study_id}"))
  } else {
    stop("Error submitting summary")
  }
}


#' Convert delimited record to JSON representation needed by a stringlist col schema
#'
#' Internal helper that reuses and extends the utility of `.delim_string_to_vector`.
#'
#' @inheritParams .delim_string_to_vector
#' @param record Character vector of length one representing a single record.
#' @keywords internal
strlist_JSON <- function(record, sep = ",", trim_ws = T) {
  .delim_string_to_vector(string = record, sep, trim_ws = T) %>%
    jsonlite::toJSON() %>%
    as.character()
}
