#' Install dependencies of the package
#'
#' This function installs or update the dependencies of the sagreicolor package.
#'
#' @param update If \code{TRUE} all packages in the list of dependencies are reinstalled. If not (default) only those that are missing.
#' @author Raphael Scherrer
#' @export

# Function to install dependencies
install_dependencies <- function(update = F) {

  dependencies <- c("mvnormtest", "ape", "geosphere", "nlme", "MASS", "DescTools", "caret", "pbapply", "rminer", "vegan", "pavo", "extrafont", "ggplot2", "multcomp", "nparcomp")

  # Find the packages that are missing
  # if update is T all packages are reinstalled
  if(!update) {
    libpaths <- .libPaths()
    packages <- lapply(lapply(libpaths, function(path) installed.packages(lib.loc = path)), as.data.frame)
    packages <- do.call("rbind", packages)
    packages <- packages$Package

    dependencies <- dependencies[!dependencies %in% packages]
  }

  # Install the missing packages
  install.packages(dependencies)

}
