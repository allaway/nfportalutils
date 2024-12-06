# Utils to help overcome nested folders

#' Get path for a Synapse id
#'
#' Small helper fun to get path for given Synapse entity.
#' @param id Synapse id.
#' @param path Path builder value.
#' @export
get_path <- function(id, path = NULL) {
  entity <- .syn$get(id)
  path <- c(entity$properties$name, path)
  if(entity$properties$concreteType == "org.sagebionetworks.repo.model.Project") return(paste0(path, collapse = "/"))
  get_path(id = entity$properties$parentId, path)
}


#' Find in path
#'
#' Get the Synapse id of an entity nested several folder layers deep without
#' having to click through the UI or create a fileview as long as the structure/path is known.
#'
#' @param scope Id of the container (project or folder) to begin search.
#' @param path Path string in format "subdir1/subdir2/file.txt", where the last-level element will be the id returned.
#' @export
find_in <- function(scope, path) {

  path_list <- strsplit(path, split = "/", fixed = TRUE)[[1]]
  here <- scope
  id <- NULL
  while(length(path_list)) {
    child <- first(path_list)
    path_list <- path_list[-1]
    here <- find_child(child_name = child, parent = here)
    id <- here
  }
  id
}

#' Find id of a child entity in a container
#'
#' @param parent Parent container (project or folder).
#' @param child_name Name of child entity.
#' @export
find_child <- function(child_name, parent) {

  q <- .syn$getChildren(parent)
  child_id <- NULL
  repeat {
    x <- reticulate::iter_next(q)
    if(is.null(x) || x$name == child_name) {
      child_id <- x$id
      break
    }
  }
  child_id
}


#' Find children of type
#'
#' Small utility like `find_child` but retrieves files by type rather than by specific name.
#' Returns a vector of ids, with entity names set as names.
#'
#' @inheritParams find_child
#' @param child_type Type(s) as a list, even for only one type. Defaults to "file".
#' @export
find_child_type <- function(parent, child_type = list("file")) {

  x <- .syn$getChildren(parent, includeTypes = child_type)
  y <- reticulate::iterate(x)
  if(!length(y)) return()
  z <- setNames(sapply(y, `[[`, "id"), sapply(y, `[[`, "name"))
  return(z)
}


#' Find data folder
#'
#' Convenience function to find data folder, which can have slight name variations, in a project.
#'
#' @param project_id Synapse project id.
#' @export
find_data_root <- function(project_id) {

  data_root <- find_child("Data", parent = project_id)
  if(is.null(data_root)) data_root <- find_child("Raw Data", parent = project_id)
  data_root
}


# Find nextflow assets --------------------------------------------------------- #

# Convenience functions for getting Synapse ids of nextflow assets

#' Find a standard nextflow workflow output asset
#'
#' Note that samplesheets became part of the output only for newer versions of nf-core/rna-seq;
#' older runs may not find samplesheets.
#' Paths default to known working paths corresponding to the latest major workflow version,
#' but this may change and may need to be updated as part of util maintenance.
#'
#' @param syn_out Id of top-level folder that corresponds to `publishDir` in a nextflow workflow.
#' @param asset Name of asset to find.
#' @param workflow Specify workflow, "rna-seq" or "sarek"; defaults to "rna-seq"
#' @returns Id of samplesheet.
#' @export
find_nf_asset <- function(syn_out,
                          asset = c("software_versions", "multiqc_report", "samplesheet", "samtools_stats"),
                          workflow = "rna-seq") {

  asset <- match.arg(asset)
  # Assets and paths can differ slightly depending on workflow, except for `software_versions.yml`, get workflow first
  if(workflow == "rna-seq") {
    path <- switch(asset,
                   software_versions = "pipeline_info/software_versions.yml",
                   multiqc_report = "multiqc/star_salmon/multiqc_report.html",
                   samplesheet = "pipeline_info/samplesheet.valid.csv",
                   samtools_stats = "multiqc/star_salmon/multiqc_data/multiqc_samtools_stats.txt"
    )
  } else if(workflow == "sarek") {
    path <- switch(asset,
                   software_versions = "pipeline_info/software_versions.yml",
                   multiqc_report = "multiqc/multiqc_report.html",
                   # samplesheet not yet stored for sarek
                   samtools_stats = "multiqc/multiqc_data/multiqc_samtools_stats.txt")
  } else {
    stop("Unrecognized workflow.")
  }

  id <- find_in(syn_out, path)
  if(is.null(id)) stop("File not found. Is this the right output directory/path?")
  id
}


#' Return workflow version according to workflow meta
#'
#' @inheritParams find_nf_asset
#' @returns Version string.
#' @export
nf_workflow_version <- function(syn_out) {

  version_meta <- find_nf_asset(syn_out, asset = "software_versions")
  file <- .syn$get(version_meta, downloadFile = TRUE)
  yml <- yaml::read_yaml(file$path)
  list(workflow = names(yml$Workflow[2]), version = yml$Workflow[[2]])
}

