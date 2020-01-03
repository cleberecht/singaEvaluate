#' Parses all unique entities from a json trajectory
#'
#' @param json_file The file with the trajectory data.
#' @import jsonlite
#' @import purrr
#' @import tibble
#' @export
extract_species <- function(json_file) {

  data_raw <- fromJSON(json_file)

  entities <- expanding_list()

  data_times <- pluck(data_raw, "trajectory-data")
  time_stamps <- names(data_times)

  for (time in time_stamps) {
    data_nodes <- pluck(data_times[time], 1, "data")
    node_stamps <- names(data_nodes)
    for (node in node_stamps) {
      data_sections <- pluck(data_nodes[node], 1, "subsections")
      section_stamps <- names(data_sections)
      for (section in section_stamps) {
        current_concentrations <- pluck(data_sections[section], 1, "concentrations")
        entity_stamps <- names(current_concentrations)
        for (entity in entity_stamps) {
          entities$add(entity)
        }
      }
    }
  }

  entity = unique(unlist(entities$as.list()))

  tibble(entity)
}
