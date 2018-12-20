#' Habitat classification with neural networks per island
#'
#' This function applies the neural network analysis on each island separately
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param saveto Path to the folder where to save the PDFs. One folder will be created for each island.
#' @param seed Seed for random number generators
#' @return Just plots.
#' @author Raphael Scherrer
#' @export

# This function applies the neural network analysis on each island separately
dewlap_neural_nested <- function(specdata, vars, nRepet = 1000, saveto, seed) {

  if(!missing("seed")) set.seed(seed)

  nislands <- nlevels(specdata$island)

  # Loop through islands
  for(i in seq_len(nislands)) {

    message(paste0("Island ", i, "/", nislands, ":"))

    curr.island <- levels(specdata$island)[i]
    curr.specdata <- droplevels(specdata[specdata$island == curr.island,])

    folder.name <- curr.island
    if(!missing(saveto)) folder.name <- paste(saveto, folder.name, sep = '/')

    dir.create(folder.name)

    # Apply neural networks
    neural.res <- dewlap_neural(curr.specdata, vars, nRepet, plot_success, plot_importance, saveto = folder.name)

  }

}
