#' Parses concentrations from a json trajectory.
#' 
#' @param json_file The file with the trajectory data.
#' @param select_nodes A vector with nodes that will be considered, if no nodes are given all are considered
#' @param select_sections A vector with sections that will be considered, if no sections are given all are considered
#' @param select_entities A vector with entities that will be considered, if no entities are given all are considered
#' @import jsonlite
#' @import purrr
#' @import tibble
#' @export
parse_concentrations <- function(json_file, select_nodes = vector(), select_sections = vector(), select_entities = vector()) {
  
  data_raw <- fromJSON(json_file)
  
  times <- expanding_list()
  nodes <- expanding_list()
  sections <- expanding_list()
  entities <- expanding_list()
  concentrations <- expanding_list()
  
  data_times <- pluck(data_raw, "trajectory-data")
  time_stamps <- names(data_times)
  
  for (time in time_stamps) {
    data_nodes <- pluck(data_times[time], 1, "data")
    node_stamps <- names(data_nodes)
    for (node in node_stamps) {
      if (length(select_nodes) == 0 || node %in% select_nodes) {
        data_sections <- pluck(data_nodes[node], 1, "subsections")
        section_stamps <- names(data_sections) 
        for (section in section_stamps) {
          if (length(select_sections) == 0 || section %in% select_sections) {
            current_concentrations <- pluck(data_sections[section], 1, "concentrations")
            entity_stamps <- names(current_concentrations) 
            for (entity in entity_stamps) {
              if (length(select_entities) == 0 || entity %in% select_entities) {
                times$add(time)
                nodes$add(node)
                sections$add(section)
                entities$add(entity)
                concentrations$add(current_concentrations[[entity]])
              }
            }
          }
        }
      }
    }
  }
  
  time = as.double(times$as.list())
  node = unlist(nodes$as.list())
  section = unlist(sections$as.list())
  entity = unlist(entities$as.list())
  concentration = as.double(concentrations$as.list())
  
  tibble(time, node, section, entity, concentration)
}