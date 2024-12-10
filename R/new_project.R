#' Create a new project
#'
#' Set up a new NF project with wiki, folders, fileview, and permissions.
#' Most parameters come from a project intake & data sharing plan (DSP) form.
#' Aside from default folders, folders are tailored for data mentioned in DSP.
#' The NF-OSI team is hard-coded to be admin in addition to the funder team indicated by `funder`.
#' Since this is intended for actual new projects, it will fail if a same existing project is detected.
#'
#' After project is created, NF Portal representation requires registration in backend:
#' - New study row added to the Portal - Studies table.
#' - Project added to Portal - Files scope.
#'
#' @param name Name of the project/study.
#' @param pi Name of the principal investigator.
#' @param lead Name(s) of the project lead/data coordinator, comma-sep if multiple, e.g. "Jane Doe, John Doe".
#' @param admin_user (Optional) Single id or list of ids of users to be made admin(s).
#' @param abstract Project abstract/description.
#' @param institution Affiliated institution(s), **semicolon-sep if multiple**, e.g. "Stanford University; University of California, San Francisco".
#' @param funder The funding agency. The relevant funder team will be made admin.
#' @param initiative Title of funding initiative, e.g. "Young Investigator Award".
#' @param datasets (Optional) List of datasets for which folders will be created under main data folder ("Raw Data").
#' Attributes set on the list items become annotations on the dataset folders.
#' @param other_resources (Optional) List of non-data resource types for which folders will be created.
#' Attributes set on the list items become annotations on these folders.
#' @param publicview Whether to put this project in the public view instead of staying private (registered or non-registered users can see project).
#' @param webview Whether to open web browser to view newly created project. Defaults to FALSE.
#' @param ... Additional arguments. Not used.
#' @return The project object.
#' @export
new_project <- function(name,
                        pi,
                        lead,
                        admin_user = NULL,
                        abstract,
                        institution,
                        funder,
                        initiative,
                        datasets = NULL,
                        other_resources = NULL,
                        publicview = FALSE,
                        webview = FALSE,
                        ...) {

  .check_login()

  project <- new_project_strict(name)

  # WIKI -----------------------------------------------------------------------#

  wiki <- add_default_wiki(project,
                           name,
                           pi,
                           lead,
                           funder,
                           initiative,
                           abstract,
                           institution)

  # PERMISSIONS ----------------------------------------------------------------#
  # Set NF-OSI Sage Team permissions to full admin
  NF_sharing <- make_admin(project, principal_id = "3378999")

  # Set grant funding team to full admin -- ignore funders not associated with a team (e.g. NIH-NCI)
  funder <- switch(funder,
                   CTF = "3359657", ##CTF team
                   GFF = "3406072", ##GFF Admin team
                   NTAP = "3331266") ##NTAP Admin team
  if(!is.null(funder)) funder_sharing <- make_admin(project, funder)

  # Set project lead/pi user to full admin user if given
  if(!is.null(admin_user)) {
    user_sharing <- lapply(admin_user, function(user) make_admin(project, user))
  }

  if(publicview) {
    public_sharing <- make_public_viewable(project)
  }

  # ASSETS ---------------------------------------------------------------------#

  # Folder structure looks something like this,
  # where * elements are conditionally present on data given:
  #├── Analysis
  #├── Milestone Reports
  #├── Protocols*
  #├── Raw Data
  #│   ├── IHC dataset*
  #│   └── RNA-seq dataset*
  #└── Scripts*

  # Note that all protocol files, etc. is expected to live in a single protocol folder and not further nested.

  # Create default upper-level folders
  folders <- add_default_folders(project)
  data_folder_id <- folders[["Raw Data"]]$properties$id

  # Bind JSON schema so children folders have NF's dataset schemas, see
  # https://repo-prod.prod.sagebase.org/repo/v1/schema/type/registered/org.synapse.nf-superdataset
  bind_schema(id = data_folder_id, schema_id = "org.synapse.nf-superdataset", derived_annotations = TRUE)

  # Create data-specific folders in "Raw Data"
  if(length(datasets)) {
    make_folder(parent = data_folder_id, folders = datasets)
  }

  # Create homes for non-data resources alongside "Raw Data"
  if(length(other_resources)) {
    other_resource_folders <- make_folder(parent = project, folders = other_resources)
    if ("Protocols" %in% names(other_resource_folders)) {
      bind_schema(id = other_resource_folders$Protocols$properties$id, schema_id = "org.synapse.nf-protocol", derived_annotations = FALSE)
    }
  }

  # Aside from dataset schema, currently only have protocols schema


  # Add Project Files and Metadata fileview, add NF schema; currently doesn't add facets
  fv <- add_default_fileview(project)

  if(webview) .syn$onweb(project)

  attr(project, "fileview") <- fv$properties$id
  return(project)
}

#' Create a strictly new project
#'
#' Internal handler for creating a project that
#' first checks whether project already exists and disallows overwriting.
#' For a less strict version that allows overwriting with a warning,
#' e.g. named `update_project`, implement with
#' `createOrUpdate = TRUE` and then compare createdOn and modifiedOn to issue a warning
#' (which would be more informative than current Python client).
#' @param project_name Name of project to be created.
#' @keywords internal
new_project_strict <- function(project_name) {
  id <- try(.syn$findEntityId(project_name))
  if(class(id) == "try-error") {
    # Error with 403 code if exists without writable permissions
    stop("Project not created because of name collision with a non-NF project.", call. = FALSE)
  } else if(class(id) == "character") {
    # Likely already administering project if returns actual synID
    stop("Project not created because of name collision with a current project. Check project entity?" , call. = FALSE)
  } else { # NULL
    project <- synapseclient$Project(project_name)
    project <- .syn$store(project, createOrUpdate = FALSE)
    project
  }
}

#' Create default project fileview
#'
#' @param project A project entity.
#' @export
add_default_fileview <- function(project) {
  view <- synapseclient$EntityViewSchema(
    name = "Project Files and Metadata",
    columns=list(
      synapseclient$Column(name="contentType", columnType="STRING", maximumSize="20"),
      synapseclient$Column(name="resourceType", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="assay", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="dataType", columnType="STRING", maximumSize="30"),
      synapseclient$Column(name="dataSubtype", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="fileFormat", columnType="STRING", maximumSize="13"),
      synapseclient$Column(name="diagnosis", columnType="STRING", maximumSize="39"),
      synapseclient$Column(name="tumorType", columnType="STRING", maximumSize="90"),
      synapseclient$Column(name="individualID", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="specimenID", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="nf1Genotype", columnType="STRING", maximumSize="8"),
      synapseclient$Column(name="nf2Genotype", columnType="STRING", maximumSize="20"),
      synapseclient$Column(name="species", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="modelSystemName", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="cellType", columnType="STRING_LIST", maximumSize="50", maximumListLength = "50"),
      synapseclient$Column(name="sex", columnType="STRING", maximumSize="30"),
      synapseclient$Column(name="age", columnType="STRING", maximumSize="50"),
      synapseclient$Column(name="experimentalCondition", columnType="STRING", maximumSize="100"),
      synapseclient$Column(name="progressReportNumber", columnType="INTEGER")
    ),
    parent = project,
    scopes = project,
    includeEntityTypes = list(synapseclient$EntityViewType$FILE, synapseclient$EntityViewType$FOLDER),
    add_default_columns = TRUE,
    addAnnotationColumns = FALSE)
  view <- .syn$store(view)
  invisible(view)
}


#' Make a user or group full admin of a Synapse entity
#'
#' Convenience method to set admin permissions
#' @param entity The Synapse entity, e.g. project or folder.
#' @param principal_id User/team name or id (e.g. "NF-OSI Sage Team", "3378999", "nf-bot", or "3423450") that will have the configured access to the entity.
#' @export
make_admin <- function(entity, principal_id) {
  if(is_valid_user(principal_id) || is_valid_team(principal_id)) {
    admin <- .syn$setPermissions(entity = entity,
                                 principalId = principal_id,
                                 accessType = list('DELETE', 'CHANGE_SETTINGS', 'MODERATE', 'CREATE', 'READ','DOWNLOAD', 'UPDATE', 'CHANGE_PERMISSIONS'))
  } else {
    warning("Principal specified is not a valid user or team. Ignoring, please correct and resolve manually.")
    return(FALSE)
  }
}

#' Create project folders
#'
#' Use to set up a scaffold of standard upper-level folders as well as
#' customized data folders within "Raw Data" for a new project.
#' @param parent The Synapse id or object that should be the parent container, i.e. the project or another folder.
#' @param folders List giving one or more folder names of folder(s) to create.
#' @return A list of the created folder object(s).
#' @export
#' @examples
#' \dontrun{
#' datasets <- list("sequencing data", "imaging data")
#' assays <- c("rnaSeq", "immunohistochemistry")
#' for(i in seq_along(datasets)) attr(datasets[[i]], "assay") <- assays[[i]]
#' make_folder(parent = "syn26462036", datasets)
#'}
make_folder <- function(parent, folders) {

  refs <- list()
  for (i in folders) {
    # If datasets have "assay" or other attributes, this is added as annotations
    folder <- synapseclient$Folder(i, parent = parent, annotations = attributes(i))
    folder <- .syn$store(folder)
    refs[[i]] <- folder
  }
  return(refs)
}

#' Add default wiki
#'
#' Add the default wiki at project at creation or
#' use to retrofit projects where creators have not created a wiki.
#' @inheritParams new_project
#' @param project Synapse id of project.
#' @export
add_default_wiki <- function(project,
                             name,
                             pi,
                             lead,
                             funder,
                             initiative,
                             abstract,
                             institution) {

  content <- glue::glue("
  # {name}
  ## {funder} - {initiative}
  ### Principal Investigator: {pi}
  ### Project Lead / Data Coordinator: {lead}
  ### Institution: {institution}
  ### Project Description:
  {abstract}
  ")

  wiki <- synapseclient$Wiki(owner = project,
                             title = name,
                             markdown = content)

  # Push wiki to Synapse
  wiki <- .syn$store(wiki)

  # Add a subpage w/ links to the Data Curator App as of Dec 2021
  wiki <- data_curator_app_subpage(project_id = project, dry_run = FALSE)

  return(wiki)

}

#' Create default folders
#'
#' A convenience wrapper around `make_folder` with NF defaults.
#' @param project The project Synapse id or object.
#' @param folders Names of the standard set of folders.
#' @export
add_default_folders <- function(project, folders = c("Analysis", "Milestone Reports", "Raw Data")) {
   make_folder(parent = project, folders)
}

#' Check that is valid user in Synapse
#' @keywords internal
is_valid_user <- function(id) {
  status <- tryCatch(
    .syn$getUserProfile(id), error = function(e) return(NULL)
    )
  if(length(status)) return(TRUE) else return(FALSE)
}

#' Check that is valid team in Synapse
#' @keywords internal
is_valid_team <- function(id) {
  status <- tryCatch(
    .syn$getTeam(id), error = function(e) return(NULL)
  )
  if(length(status)) return(TRUE) else return(FALSE)
}

#' Wrapper for JSON schema binding
#'
#' See https://help.synapse.org/docs/JSON-Schemas.3107291536.html
#'
#' @param id Id of entity to which schema will be bound
#' @param schema_id Schema id as registered on Synapse.
#' @param derived_annotations Whether to enabled derived annotations.
#' Default `FALSE` as this is the API default.
bind_schema <- function(id, schema_id, derived_annotations = FALSE) {

  bind_schema_request <- jsonlite::toJSON(list(entityId = id,
                                               `schema$id` = schema_id,
                                               enableDerivedAnnotations = derived_annotations),
                                          auto_unbox = TRUE)
  binding_uri <- glue::glue("https://repo-prod.prod.sagebase.org/repo/v1/entity/{id}/schema/binding")
  try(.syn$restPUT(binding_uri, bind_schema_request))
}
