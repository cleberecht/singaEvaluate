#' Attach pameters to the corresponsing trajectory
#'
#' @param trajectory The parsed trajectory
#' @param parameter_file The file with the stored parameters
#' @param parameter_names An optinal mapping that replaces integer identifiers with names for easy access
#' @import dplyr
#' @export
attach_parameters <- function(trajectory, parameter_file, parameter_names = vector()) {
  # add to every row
  merge(trajectory, simplify_parameters(parameter_file, parameter_names))
}