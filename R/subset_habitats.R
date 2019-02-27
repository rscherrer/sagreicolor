#' Subset of the spectral dataset
#'
#' This function takes a subset of the spectral dataset in order to have no empty cell in the two-way analysis design. Given habitats to retain, the function eliminates all the observations that are not from these habitats, and eliminates all islands that do not have all these habitats present.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param whatHabitats A character vector. What habitats to retain?
#' @param inclusive Do we include the islands with some but not all of the wanted habitats (Conception)? If \code{FALSE}, keep only the islands with all the wanted habitats.
#' @return A data frame, a subset of \code{specdata}.
#' @author Raphael Scherrer
#' @export

# Function to keep only some habitats in the data, to avoid empty cells in the two-way design
subset_habitats <- function(specdata, whatHabitats, inclusive = F) {

  # Keep all islands that have at least the wanted habitats
  if(inclusive) {

    specdata <- subset(specdata, habitat %in% whatHabitats)

  } else {

    # Or keep only the islands with all wanted habitats
    # For each island...
    whatIslands <- sapply(levels(specdata$island), function(curr.island) {

      # Are all habitats in the list present on that island?
      all(whatHabitats %in% levels(droplevels(specdata[specdata$island == curr.island,]$habitat)))

    })
    whatIslands <- names(whatIslands)[whatIslands]

    # Subset of the data
    specdata <- subset(specdata, island %in% whatIslands & habitat %in% whatHabitats)

  }

  return(droplevels(specdata))

}
