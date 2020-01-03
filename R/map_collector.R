#' Maps the given entities to the given collectors.
#'
#' @param entites The entities, which should be considered
#' @param criteria The criteria for mapping a entity to a collector
#' @import dplyr
#' @import purrr
#' @export
map_collector <- function(entites, criteria) {
  map_dfr(criteria,
           ~ subset(entites,
                    pull(model.frame(., data = entites))),
           .id = "collector")
}
