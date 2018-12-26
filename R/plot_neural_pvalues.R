#' Plot neural network classification
#'
#' This function plots the P-values assessing the classification output of the neural networks.
#'
#' @param X A data frame containing P-values for randomized and empirical data.
#' @param saveto Where to save the PDF plot, optional
#' @param font Font to be used in the plots. Defaults to Helvetica.
#' @return Just plots.
#' @author Raphael Scherrer
#' @export
# Function to plot the results of a neural network classification analysis
plot_neural_pvalues <- function(X, saveto, font) {

  library(extrafont)
  loadfonts(quiet = T)
  if(missing(font)) font <- "Helvetica"

  p <- ggplot(results, aes(x = p.value, fill=label))  + geom_histogram(position="identity", alpha=0.5, bins = 100 ) + theme_bw() + xlab("Binomial test P-value") + ylab("Count") + theme(legend.title = element_blank())

  if(!missing(saveto)) {
    pdfname <- paste(saveto, "pvalues_neural_network.pdf", sep = '/')
    ggsave(pdfname, p, device = "pdf", width = 4, height = 2.5, family = font)
  } else print(p)

}
