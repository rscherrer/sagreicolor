#' Plot importance of variables in discriminating between habitats
#'
#' This function plots the importance of (wavelength and non-wavelength) variables in discriminating habitats by the 5\% best neural networks. Importance is defined as the cumulative weights of the input nodes.
#'
#' @param importance The importance table returned by \code{plot_neural} (the \code{imp} field).
#' @param saveto Where to save the PDF plot, optional
#' @param font Font to be used in the plots. Defaults to Helvetica.
#' @author Raphael Scherrer
#' @export
#'
# Function to plot the importance of (wavelength and non-wavelength) variables according to the neural networks
plot_neural_importance <- function(importance, saveto, font) {

  library(extrafont)
  loadfonts(quiet = T)
  if(missing(font)) font <- "Helvetica"

  imp_wavelengths <- importance[grep("^wl", names(importance))]
  imp_others <- importance[-grep("^wl", names(importance))]
  ylim <- c(0, max(importance))

  if(!missing(saveto)) {
    dir.create(saveto, showWarnings = F)
    pdfname <- paste(saveto, "importance.pdf", sep = "/")
    pdf(pdfname, width = 6, height = 3, family = font)
  }

  par(mar = c(5.1, 1.1, 4.1, 1.1), oma = c(0, 5, 0, 0))
  matlayout <- matrix(c(1,1,1,2), nrow = 1)
  layout(matlayout)

  plot(imp_wavelengths, ylab="", xlab="", type="l", las = 1, ylim = ylim)
  mtext(text = "Importance", outer = T, side = 2, line = 2, cex = 1)
  mtext(text = "Wavelength (nm)", outer = F, side = 1, line = 2.5, cex = 1)

  barplot(imp_others, las = 1, ylab = "", axes = F, ylim = ylim, cex.names = 0.9, space = 0.5)

  if(!missing(saveto)) dev.off()

}
