#' Habitat classification with neural networks per island
#'
#' This function applies the neural network analysis on each island separately
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param seed Seed for random number generators
#' @param plotit Whether to plot the success or not
#' @return A list of lists, each the output of dewlap_neural.
#' @author Raphael Scherrer
#' @export

# This function applies the neural network analysis on each island separately
dewlap_neural_nested <- function(specdata, vars, nRepet = 1000, seed = 42, plotit = F) {

  # Repeat the analysis for each island this time
  islands <- levels(specdata$island)

  nested.res <- lapply(islands, function(curr.island) {

    message(paste("Current island:", curr.island))

    # Create a directory for the current island if does not already exist
    dir.create(curr.island, showWarnings = FALSE)

    # Go into the right directory
    homedir <- getwd()
    setwd(curr.island)

    # Subset the data
    specdata <- droplevels(subset(specdata, island == curr.island))

    # Apply the neural network function, without seed
    curr.res <- dewlap_neural(specdata, vars, nRepet = nRepet, plotit)

    # Go back to home directory
    setwd(homedir)

    return(curr.res)

  })

  return(nested.res)

}
