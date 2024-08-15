#' Add activity to entity
#'
#' Util for adding activity info to a file entity.
#' See also https://help.synapse.org/docs/Provenance.1972470373.html
#'
#' @param entity Synapse entity id.
#' @param act_name Name of activity.
#' @param act_executed Reference to the the activity executed (URL preferred).
#' @param used_inputs Vector of inputs for this act, e.g. syn ids, links to other data sources, etc.
#' @export
add_activity <- function(entity,
                         act_name,
                         act_executed,
                         used_inputs) {

    act <- synapseclient$Activity(name = act_name,
                                  executed = act_executed,
                                  used = used_inputs)
    p <- .syn$setProvenance(entity, activity = act)
    p
}

#' Add activity to multiple entities
#'
#' Wrapper provenance function that does a little more work to
#' expand many-to-many mappings to create records of entity, activity, and input.
#'
#' @param entities Vector or list of entities.
#' @param act_name Vector or list of activity name.
#' @param act_executed Vector or list of reference activity executed.
#' @param used_inputs Vector or list of inputs for each entity.
#' @import data.table
#' @export
add_activity_batch <- function(entities,
                               act_name,
                               act_executed,
                               used_inputs
                             ) {

  stopifnot(lengths(list(entities, act_name, act_executed, used_inputs)) > 0)
  if(is.list(entities)) {
    if(length(used_inputs) > 1) used_inputs <- rep(used_inputs, lengths(entities))
    if(length(act_name) > 1) act_name <- rep(act_name, lengths(entities))
    if(length(act_executed) > 1) act_executed <- rep(act_executed, lengths(entities))
    entities <- unlist(entities)
  }
  Map(add_activity, entities, act_name, act_executed, used_inputs)
}


#' Remove provenance info
#'
#' @inheritParams add_activity_batch
#' @export
delete_provenance <- function(entities) {
  for(i in entities) {
    .syn$deleteProvenance(entity)
  }
}

