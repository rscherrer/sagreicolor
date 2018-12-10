#' Habitat classification with neural networks per island
#'
#' This function applies the neural network analysis on each island separately
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param plot_success Whether to plot histograms of the success of the machines on the empirical data compared to the null distribution.
#' @param plot_importance Whether to plot the importance of each variable in discriminating among habitats, according to the top 5\% machines.
#' @param save_plot Whether to save plots as pdfs to the plot/ folder or, if false, to display them in R.
#' @param seed Seed for random number gnerators
#' @return Just plots.
#' @note If \code{plot_importance = T} and reflectances at certain wavelengths are present in the variables (their name start with "wl" in specdata) then a first linear plot is drawn along the light spectrum, then a barplot is generated with the remaining variables.
#' @author Raphael Scherrer
#' @export

# This function applies the neural network analysis on each island separately
dewlap_neural_nested <- function(specdata, vars, nRepet = 1000, plot_success = T, plot_importance = T, save_plot = T, seed) {

  if(!missing("seed")) set.seed(seed)

  nislands <- nlevels(specdata$island)

  # Loop through islands
  for(i in seq_len(nislands)) {

    message(paste0("Island ", i, "/", nislands, ":"))

    curr.island <- levels(specdata$island)[i]
    curr.specdata <- droplevels(specdata[specdata$island == curr.island,])

    pdfnames <- c("plots/success_neural_network",
                  "plots/pvalues_neural_network",
                  "plots/importance_along_spectrum",
                  "plots/importance")
    pdfnames <- paste0(pdfnames, "_", curr.island, ".pdf")

    # Apply neural networks
    neural.res <- dewlap_neural(curr.specdata, vars, nRepet, plot_success, plot_importance, save_plot, pdfnames)

  }

}
