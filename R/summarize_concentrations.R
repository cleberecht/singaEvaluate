#' Parses concentrations from a json trajectory.
#'
#' @param trajectory The parsed trajectory
#' @param entity_mapping The entity to collector mapping created by \code{\link{map_collector}} or manually.
#' @import dplyr
#' @export
summarize_concentrations <- function(trajectory, entity_mapping) {
  # create hash environment
  collector_hash <- new.env(hash = TRUE)
  # add entity mapping
  for (entry_index in 1:nrow(entity_mapping)) {
    entry <- entity_mapping[entry_index,]
    collector_hash[[entry$entity]] <- entry$collector
  }
  # apply entity transformations
  trajectory %>%
    # replace entitys with mapped collectors from hash table
    mutate(entity = unlist(mget(entity, envir = collector_hash))) %>%
    group_by(time, node, section, entity) %>%
    # sum equivalent entitiies
    summarise(concentration = sum(concentration))
}
