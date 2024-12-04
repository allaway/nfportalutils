#' Generate manifest via schematic service
#'
#' See [schematic manifest generation](https://schematic.api.sagebionetworks.org/v1/ui/#/Manifest%20Operations/schematic_api.api.routes.get_manifest_route).
#' Note that this uses the access token of user that should already be logged in with `syn_login`.
#'
#' @param data_type Data type of the manifest to generate (aka Component).
#' @param dataset_id Optional, if given this fills out manifest for existing dataset instead of generating a blank manifest.
#' @param title Optional, custom title.
#' @param schema_url Optional, defaults to main NF 'latest' data model.
#' @param asset_view Optional, defaults to main NF portal fileview.
#' @param output_format Format of 'excel', 'google_sheet', or 'dataframe'. Defaults to 'excel'.
#' @param use_annotations Use annotations if filling out manifest for existing dataset. Defaults to TRUE for NF.
#' @param service Service endpoint to use. Defaults to the schematic production endpoint.
#' @returns For excel, path to local file; for google_sheet, URL to sheet; for dataframe, JSON string of data.
#' @export
manifest_generate <- function(data_type,
                              dataset_id = NULL,
                              title = data_type,
                              schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                              asset_view = "syn16858331",
                              output_format = "google_sheet",
                              use_annotations = TRUE,
                              service = "https://schematic.api.sagebionetworks.org/v1/manifest/generate") {

  # yes, param needs to be re-encoded like this for 'dataframe'
  output_format_param <- if (output_format == "dataframe") "dataframe (only if getting existing manifests)" else output_format
  access_token <- .syn$credentials$secret
  use_annotations <- tolower(as.character(use_annotations))

  req <- httr::GET(service,
                   query = list(
                     schema_url = schema_url,
                     title = title,
                     data_type = data_type,
                     use_annotations = use_annotations,
                     dataset_id = dataset_id,
                     asset_view = asset_view,
                     output_format = output_format_param),
                   httr::add_headers(Authorization = paste("Bearer", access_token)))

  status <- httr::status_code(req)
  if(status != 200L) stop("Unsuccessful request, received status code: ", status)

  if(output_format == "excel") {
    if(!is.null(dataset_id)){
       file <- glue::glue("manifest_{dataset_id}.xlsx")
    }else{
       file <- glue::glue("manifest.xlsx")
    }
    message(glue::glue("Manifest generated and saved as {file}"))
    bin <- httr::content(req, "raw")
    writeBin(bin, file)
    return(file)
  }

  if(output_format == "google_sheet") {
    message("Manifest generated as Googlesheet(s)")
    url <- httr::content(req)
    return(url)
  }

  if(output_format == "dataframe") {
    message("Manifest(s) generated as JSON doc")
    json_str <- httr::content(req, "text", encoding = "UTF-8")
    # json_str <- jsonlite::minify(json_str)
    return(json_str)
  }
}


#' Validate manifest via schematic service
#'
#' See [schematic validation](https://schematic.api.sagebionetworks.org/v1/ui/#/Model%20Operations/schematic_api.api.routes.validate_manifest_route).
#' Get validation results from schematic service. Downstream utils can consume these results for custom display/report.
#'
#' @inheritParams manifest_generate
#' @param json_str JSON string representing metadata.
#' @param file_name Path to file, has to be `.csv`. Ignored if `json_str` is given.
#' @param restrict_rules Use only basic schematic validation instead of extended validation with Great Expectations, default `FALSE`.
#' @export
manifest_validate <- function(data_type,
                              json_str = NULL,
                              file_name = NULL,
                              restrict_rules = FALSE,
                              schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                              service = "https://schematic.api.sagebionetworks.org/v1/model/validate") {

  restrict_rules <- tolower(as.character(restrict_rules))

  # json_str can't be more than 4-8 KiB (nginx server typical limits), otherwise header overflow error
  if(!is.null(json_str)) {
    if(object.size(json_str) > 40000) stop("Data of this size should be submitted as a file instead of JSON string")
    req <- httr::POST(service,
                      query = list(
                        json_str = json_str,
                        schema_url = schema_url,
                        data_type = data_type,
                        restrict_rules = restrict_rules))
  } else if(!is.null(file_name)) {
    if(!file.exists(file_name)) stop("Trying to validate ", file_name, " that can't be found.")
    req <- httr::POST(service,
                      query = list(
                        schema_url = schema_url,
                        data_type = data_type,
                        restrict_rules = restrict_rules),
                      body = list(file_name = httr::upload_file(file_name), type = "text/csv"))
  } else {
    stop("Must provide manifest data as either JSON or local file path.")
  }

  status <- httr::status_code(req)
  if(status != 200L) stop("Unsuccessful request, received status code: ", status)
  result <- httr::content(req)
  result
}

#' Terse error messages please
#'
#' @param error An error object from schematic.
#' @keywords internal
tersely <- function(error) {
  row <- error[[1]]
  column <- error[[2]]
  message <- error[[3]]
  value <- error[[4]]

  wording <- if(grepl("is not one of", message)) paste(shQuote(value), "is not a valid value for", column) else message
  wording
}

#' Provide a pass/fail summary result
#'
#' @param result Result list data from schematic service.
#' @returns Boolean for whether passed.
#' @returns List of structure `list(result = result, notes = notes)`, where `result` indicates whether the dataset passed.
#' @export
manifest_passed <- function(result) {

  errors <- length(result$errors)
  if(errors) {
    messages <- unique(sapply(result$errors, tersely))
    notes <- paste(messages, collapse = ", ")
    return(list(result = FALSE, notes = notes))
  } else {
    return(list(result = TRUE, notes = "No errors"))
  }
}

#' Infer data type of a dataset folder
#'
#' Infer the data type by checking the first few files.
#' TODO: Check `dataType` instead of Component and derive Component
#' because some older files does not have Component explicitly.
#'
#' @inheritParams manifest_generate
#' @return List of structure `list(result = result, notes = notes)`, where `result` can be `NA`.
#' @export
infer_data_type <- function(dataset_id) {

  children <- .syn$getChildren(dataset_id)
  children <- reticulate::iterate(children)
  if(!length(children)) return(list(result = NA, notes = "Empty dataset folder"))
  children <- first(children, 3)
  data_type <- c()
  for (entity in children) {
    e <- .syn$get_annotations(entity)
    data_type <- append(data_type, e$Component)
  }
  data_type <- unique(data_type)
  if(is.null(data_type)) return(list(result = NA, notes = "Metadata insufficient to infer data type."))
  if(length(data_type) > 1) return(list(result = NA, notes = "Conflicting data types observed."))
  return(list(result = data_type, notes = ""))
}


#' QC dataset metadata with pass/fail result
#'
#' R wrapper for validation workflow with schematic. Because there is no validation-only service endpoint,
#' we move metadata around twice (generating manifest from server and submitting back to server),
#' so once schematic has a validation-only service endpoint that would be much more efficient.
#' A dataset in this context is a folder, usually tagged with `contentType` = "dataset".
#'
#' Note that we prefer to wrap the schematic web API over a local installation because:
#' - Will not require user to go through local schematic setup for this to be functional
#' - API more likely reflects an up-to-date version of schematic and consistent with current DCA deployment
#'
#' When `data_type` can't be inferred based on annotations, this is treated as a fail.
#'
#' Status: alpha and likely to change based on changes in `schematic`.
#'
#' @param dataset_id Id of folder that represents a dataset, not actual Synapse dataset entity -- see details.
#' @param data_type A specific data type to validate against, otherwise tries to infer based on annotations. See details.
#' @param asset_view A reference view, defaults to the main NF portal fileview.
#' @param schema_url Schema URL, points by default to 'latest' main NF schema, can change to use a specific released version.
#' @param cleanup Whether to automatically remove reconstituted manifests once done. Default `TRUE`.
#' @returns List of structure `list(result = result, notes = notes)`,
#' where `result` indicates passing or `NA` if no data or if couldn't be validated for other reasons.
#' @export
meta_qc_dataset <- function(dataset_id,
                            data_type = NULL,
                            asset_view = "syn16787123",
                            schema_url = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/NF.jsonld",
                            cleanup = TRUE) {

  dataset_name <- .syn$get(dataset_id)$properties$name

  files <- reticulate::iterate(.syn$getChildren(dataset_id))
  if(!length(files)) {
    return(list(result = NA,
                notes = "Empty dataset with no files",
                dataset_name = dataset_name,
                dataset_id = dataset_id,
                data_type = data_type))
  }

  if(is.null(data_type)) {
    data_type <- infer_data_type(dataset_id)$result
    if(is.na(data_type)) {
      return(list(result = FALSE,
                  notes = "Metadata quality insufficient to even infer data type",
                  dataset_name = dataset_name,
                  dataset_id = dataset_id,
                  data_type = data_type))
    }
  }

  # Reconstitute metadata manifest via excel as the best option for now
  tryCatch({
    message(glue::glue("Generating manifest files for dataset {dataset_id}..."))
    xl_file <- manifest_generate(data_type, dataset_id, output_format = "excel")
    csv_file <-  glue::glue("manifest_{dataset_id}.csv")
    csv <- readxl::read_excel(xl_file, sheet = 1)
    write.csv(csv, file = csv_file)
    # Validate
    results <- manifest_validate(data_type = data_type, file_name = csv_file)
    if(cleanup) {
      file.remove(xl_file, csv_file)
      message(glue::glue("Temp manifest files removed for dataset {dataset_id}"))
    }
    results <- manifest_passed(results)
  }, error = function(e) {
    results <- list(result = NA, notes = e$message) # API errors
  })

  results$dataset_name <- dataset_name
  results$data_type <- data_type
  results$dataset_id <- dataset_id
  results
}


#' QC metadata at the project level with pass/fail result
#'
#' An adequate wrapper to go through project datasets and do basic QC in one-stop-shop manner
#' **for projects that have standard structure corresponding to what DCA expects**.
#'
#' For selective validation or other (e.g. milestone-based) structures, look at `meta_qc_dataset`.
#'
#' @param project_id Synapse project id.
#' @param result_file If not NULL, *also* write to output to `.csv` file.
#' @param ... Params passed to `meta_qc_dataset`.
#' @return A table of with rows for the datasets QC'd, with dataset id, name, TRUE/FALSE pass result, and summary;
#' otherwise `NA`.
#' @export
meta_qc_project <- function(project_id, result_file = NULL, ...) {

  datasets <- list_project_datasets(project_id)
  if(!length(datasets)) {
    message("Problem with detecting datasets. Check project structure or drop down to manual dataset-by-dataset assessment.")
    return(NA)
  }

  dataset_ids <- sapply(datasets, `[[`, "id")
  dataset_names <- sapply(datasets, `[[`, "name")
  message("Datasets found for QC:\n", glue::glue_collapse(dataset_names, sep = "\n"))
  results <- lapply(dataset_ids, meta_qc_dataset, ...)
  report <- rbindlist(results, fill = TRUE)
  if(!is.null(result_file)) write.csv(report, file = result_file, row.names = T)
  report

}


#' List datasets in project
#'
#' Return a list of datasets. Datasets can be folders in the expected location in the project or actual dataset entities.
#' Note that dataset-folders will always be what exists first in the project, as sort of the dataset precursor.
#' The files in dataset-folders can be translated to dataset entities later.
#' When not found, will return NULL w/ explanatory message.
#'
#' @inheritParams find_data_root
#' @param type Whether to list datasets as immediate folders under "Raw Data" root (default, see details) or actual dataset entities in the project.
#' @export
list_project_datasets <- function(project_id,
                                  type = c("folder", "dataset")) {

  type <- match.arg(type)
  if(type == "folder") {
    data_root <- find_data_root(project_id)
    if(is.null(data_root)) {

      warning("Data root could not be located.")
      return()

    } else {

      in_data <- .syn$getChildren(data_root)
      in_data <- reticulate::iterate(in_data)
      datasets <- Filter(function(x) x$type == "org.sagebionetworks.repo.model.Folder", in_data)
      if(!length(datasets)) warning("No datasets found under data root.")
      datasets
    }
  } else {
    children <- .syn$getChildren(project_id)
    datasets <- reticulate::iterate(children)
    datasets <- Filter(function(x) x$type == "org.sagebionetworks.repo.model.table.Dataset", datasets)
    if(!length(datasets)) warning("No dataset entities found in project.")
    datasets
  }
}

#' Precheck a manifest
#'
#' Precheck before sending manifest off to schematic validation service.
#' Provides additional context and helpful recommendations.
#'
#' @param manifest_csv Path to manifest_csv.
#' @param official_props (Optional) Doc listing official model attributes. Currently this requires the LinkML format.
#' @export
precheck_manifest <- function(manifest_csv,
                              official_props = "https://raw.githubusercontent.com/nf-osi/nf-metadata-dictionary/main/modules/props.yaml") {

  manifest <- fread(manifest_csv)
  attributes <- names(manifest)

  #-- ERRORS --#
  if(!"Component" %in% attributes) {
    message(glue::glue())
  } else {
    unique_components <- unique(manifest$Component)
    if(length(unique_components) > 1) {
      which_components <- glue::glue_collapse(shQuote(unique_components), ", ")
      message(glue::glue("{emoji::emoji('x')} Multiple components detected in a single manifest: {which_components}. This can happen when files were annotated at different eras.
                          Suggestions: 1) Split up the manifest because schematic can only validate one type at a type. 2) Harmonize the components if this is sensible.
                          For example, RNASeqTemplate is an alias for GenomicsAssayTemplate"))
    }

    if("" %in% unique_components) {
      message(glue::glue("{emoji::emoji('x')} Blank value '' for Component detected. This can happen because files were annotated before 2022, when Component was introduced for most DCCs."))
    }

  }

  # Duplicate columns like age..1 etc.
  likely_dups <- grep("[.][0-9]+", attributes, value = TRUE)
  if(length(likely_dups)) {
    likely_dups <- glue::glue_collapse(shQuote(likely_dups), ", ")
    message(glue::glue("{emoji::emoji('x')} The pattern of these attribute names suggest duplicates: {likely_dups}. This may happen when metadata is supplemented programmatically with a data-type mismatch"))
  }

  #-- WARNINGS --#
  # These technically don't break present-day schematic revalidation but should be cleaned up; many are from earlier schematic issues.

  # See https://sagebionetworks.slack.com/archives/C01ANC02U59/p1681418154850589
  if("Uuid" %in% attributes) {
    message(crayon::yellow(glue::glue("{emoji::emoji('warning')} An attribute `Uuid` is present and should preferably be removed.")))
  }

  # See https://github.com/Sage-Bionetworks/schematic/issues/476#issuecomment-848853193
  if("eTag" %in% attributes) {
    message(crayon::yellow(glue::glue("{emoji::emoji('warning')} An attribute `eTag` is present and preferably be removed.")))
  }

  #-- INFO only --#
  if(!is.null(official_props)) props <- names(yaml::read_yaml(official_props)$slots)
  custom_attributes <- setdiff(attributes, props)
  if(length(custom_attributes)) {
    custom_attributes <- glue::glue_collapse(shQuote(custom_attributes), ", ")
    message(crayon::blue(glue::glue("{emoji::emoji('information')} Custom attributes (not documented in data model) were found: {custom_attributes}. In general, custom attributes added by the researcher to help with data management are fine.
                Just check that they are not PHI or added by mistake. If they are deemed generally useful or important enough, they can also be documented officially in the data model for others to reference.")))
  }

}
