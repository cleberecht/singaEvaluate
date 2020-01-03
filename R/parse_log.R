parse_log <- function(log_file, parameter_names = vector()) {
  fromJSON(log_file) %>%
    # get first element from list
    pluck(1) %>%
    # concert to tibble
    as_tibble() %>%
    # cosmetics
    rename("alternative_values" = "alternative-values") %>%
    # convert identifiers to factors for matching
    mutate(identifier = as.factor(identifier)) %>%
    # map name to parameter
    mutate(identifier = ifelse(identifier %in% names(parameter_names), parameter_names[identifier], identifier))
}