#' Habitat classification with neural networks per island
#'
#' This function applies the neural network analysis on each island separately
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param seed Seed for random number generators
#' @return A list of lists, each the output of dewlap_neural.
#' @author Raphael Scherrer
#' @export

# This function applies the neural network analysis on each island separately
dewlap_neural_nested <- function(specdata, vars, nRepet = 1000, seed) {

  if(!missing("seed")) set.seed(seed)

  nislands <- nlevels(specdata$island)

  out <- list()

  # Loop through islands
  for(i in seq_len(nislands)) {

    message(paste0("Island ", i, "/", nislands, ":"))

    curr.island <- levels(specdata$island)[i]
    curr.specdata <- droplevels(specdata[specdata$island == curr.island,])

    # Apply neural networks
    out[[i]] <- dewlap_neural(curr.specdata, vars, nRepet)

  }

  names(out) <- levels(specdata$island)
  return(out)

}
