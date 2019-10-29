#' Parses the states and positions of all vesicles over the course of the simulation
#' @param json_file The file with the trajectory data.
#' @import jsonlite
#' @import purrr
#' @import tibble
#' @import stringr
#' @export
parse_vesicle_trajectories <- function(json_file) {
  
  data_raw <- fromJSON(json_file)
  
  times <- expanding_list()
  vesicles <- expanding_list()
  states <- expanding_list()
  positions_x <- expanding_list()
  positions_y <- expanding_list()
  
  data_times <- pluck(data_raw, "trajectory-data")
  time_stamps <- names(data_times)
  
  for (time in time_stamps) {
    data_nodes <- pluck(data_times[time], 1, "data")
    node_stamps <- names(data_nodes)
    for (node in node_stamps) {
      # skip nodes
      if (str_starts(string = node, pattern = "n")) {
        next;
      }
      state = pluck(data_nodes[node], 1, "state")
      data_positions <- pluck(data_nodes[node], 1, "subsections") %>%
        pluck("vesicle lumen") %>%
        pluck("positions")
      times$add(time)
      vesicles$add(node)
      states$add(state)
      positions_x$add(data_positions$x)
      positions_y$add(data_positions$y)
    }
  }
  
  time = as.double(times$as.list())
  vesicle = unlist(vesicles$as.list())
  state = unlist(states$as.list())
  x = as.double(positions_x$as.list())
  y = as.double(positions_y$as.list())
  
  tibble(time, state, vesicle, x, y)
  
}