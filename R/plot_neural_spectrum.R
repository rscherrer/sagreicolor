#' Plot importance of wavelengths in discriminating between habitats
#'
#' This function plots the importance of wavelengths in discriminating habitats by the 5\% best neural networks
#'
#' @param X The importance table returned by plot_neural.
#' @param saveto Where to save the PDF plot, optional
#' @param font Font to be used in the plots. Defaults to Helvetica.
#' @return Just plots.
#' @author Raphael Scherrer
#' @export
#'
# Function to plot the importance of wavelengths according to the neural networks
plot_neural_spectrum <- function(X, saveto, font) {

  library(extrafont)
  loadfonts(quiet = T)
  if(missing(font)) font <- "Helvetica"

  if(length(grep("wl", names(X))) != 0) {

    wl_id <- grep("wl", names(X))
    imp <- X[wl_id]
    wls <-  as.numeric(gsub("wl", "", names(X)[wl_id]))
    imp_along_spectrum <- cbind(wls, imp)

    if(!missing(saveto)) {
      dir.create(saveto, showWarnings = F)
      pdfname <- paste(saveto, "importance_along_spectrum.pdf", sep = "/")
      pdf(pdfname, width = 5, height = 4, family = font)
    }
    plot(imp_along_spectrum, ylab="Importance", xlab="Wavelength (nm)", type="l", las = 1)
    if(!missing(saveto)) dev.off()

  } else stop("no wavelength found")

}
