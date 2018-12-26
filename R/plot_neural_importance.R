#' Plot importance of variables in discriminating between habitats
#'
#' This function plots the importance of non-wavelengths in discriminating habitats by the 5\% best neural networks
#'
#' @param X The importance table returned by plot_neural.
#' @param saveto Where to save the PDF plot, optional
#' @param font Font to be used in the plots. Defaults to Helvetica.
#' @return Just plots.
#' @author Raphael Scherrer
#' @export
#'
# Function to plot the importance of non-wavelength variables according to the neural networks
plot_neural_importance <- function(X, saveto, font) {

  library(extrafont)
  loadfonts(quiet = T)
  if(missing(font)) font <- "Helvetica"

  # Remove wavelengths, if any
  if(length(grep("wl", names(X))) != 0) {

    wl_id <- grep("wl", names(X))
    X <- X[-wl_id]

  }

  if(length(X) == 0) stop("no variable other than wavelengths")

  if(!missing(saveto)) {
    pdfname <- paste(saveto, "importance.pdf", sep = "/")
    pdf(pdfname, width = 5, height = 4, family = font)
  }
  barplot(X, las = 1, ylab = "Importance")
  if(!missing(saveto)) dev.off()

}
