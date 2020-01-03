#' Attach pameters to the corresponsing trajectory
#'
#' @param trajectory The parsed trajectory
#' @param parameter_file The file with the stored parameters
#' @param parameter_names An optinal mapping that replaces integer identifiers with names for easy access
#' @import dplyr
#' @import readr
#' @export
simplify_experiment <- function(experiment_directory, entity_mapping, parameter_names = vector()) {
  
  trajectory_file = file.path(experiment_directory, "trajectory.json")
  log_file = file.path(experiment_directory, "variations.json")
  
  parse_concentrations(trajectory_file, select_entities = pull(entity_mapping)) %>%
    # summarize entities as supplied in entitiy mapping
    summarize_concentrations(entity_mapping) %>%
    # write reduced data
    write_csv(path = file.path(directory, "simplified_trajectory.csv"))
  
  simplify_parameters(log_file, parameter_names) %>%
    write_csv(path = file.path(directory, "simplified_parameters.csv"))
}