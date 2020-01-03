#' creates pairs of identifier and value from pameters
#'
#' @param parameter_file The file with the stored parameters
#' @param parameter_names An optinal mapping that replaces integer identifiers with names for easy access
#' @import dplyr
#' @export
simplify_parameters <- function(parameter_file, parameter_names = vector()) {
  # get log
  parse_log(log_file, parameter_names) %>%
    # reduce to relevant data
    select(identifier, quantity) %>%
    # transpose
    spread(key = "identifier", value = "quantity")
}