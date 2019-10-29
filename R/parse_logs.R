#' Parses all logs from an observation folder.
#'
#' @param observation_directory The path to the observation directories (containing directories with a simulation each)
#' @param parameter_names A reference for renaming parameter identifiers to parameter names
#' @import jsonlite
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @export
parse_logs <- function(observation_folder, parameter_names = vector()) {
  
  setwd(observation_folder);
  folder_name <- dir()
  file_name = "variations.json";
  
  # generate paths
  file_paths <- as.data.frame(x = folder_name) %>%
    # create file path from observation folder
    mutate(path = file.path(folder_name, file_name))
  
  # read files
  data_log_raw = file_paths %>%
    # read each file to a list
    mutate(content = map(path, ~ fromJSON(.))) %>%
    # unnest list
    unnest() %>%
    # unnest dataframe
    unnest() %>%
    # convert identifiers to factors for matching
    mutate(identifier = as.factor(identifier)) %>%
    # map name to parameter
    mutate(name = ifelse(identifier %in% names(parameter_names), parameter_names[identifier], identifier)) %>%
    # reduce data
    select(folder_name, identifier, name, quantity)
  
}