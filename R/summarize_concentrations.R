#' Parses concentrations from a json trajectory.
#'
#' @param trajectory The parsed trajectory
#' @param entity_mapping The entity to collector mapping created by \code{\link{map_collector}} or manually.
#' @import dplyr
#' @export
summarize_concentrations <- function(trajectory, entity_mapping) {
  # retrieve collector names from mapping
  collector_names <- entity_mapping %>%
    distinct(collector) %>%
    pull(collector)
  # list to cellect rows
  collecting_list = list()
  # for each collector
  for (filter_name in collector_names) {
    # get mapping for entity
    entities <- entity_mapping %>%
      filter(collector == filter_name) %>%
      pull(entity)
    # add to list
    collecting_list[[filter_name]] <- trajectory %>%
      # look for entity in mapping
      filter(entity %in% entities) %>%
      # group without entity since it was filtered
      group_by(time, node, section) %>%
      # sum equivalent entitiies
      summarise(concentration = sum(concentration)) %>%
      ungroup() %>%
      # add entity column
      mutate(entity = filter_name)
  }
  # cosmetics and return
  bind_rows(collecting_list) %>%
    arrange(time) %>%
    select(time, node, section, entity, concentration)
}
