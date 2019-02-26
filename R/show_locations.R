#' Show geographical coordinates
#'
#' Function to show exact sampling locations
#'
#' @return A table with sampling locations.
#' @author Raphael Scherrer
#' @export

# Function to show exact sampling locations
show_locations <- function() {
  # Exact sampling locations
  locations <- readRDS("data/locale.rds")
  locations <- locations[,-1]
  locations <- locations[,-3]
  locations <- droplevels(locations[locations[,2] != "secondary",])
  locations <- droplevels(locations[locations[,1] != "Nassau",])
  locations <- unique(locations)
  # print(xtable(locations, digits = 2), include.rownames = F)
}
