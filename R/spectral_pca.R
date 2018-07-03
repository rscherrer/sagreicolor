#' Principal Component Analysis of dewlap reflectance
#'
#' This function calculates the principal components of a reflectance dataset.
#'
#' @param specdata A data frame with reflectance in columns. The columns with reflectance information must have "wl" in their name, e.g. "wl500" for 500nm.
#' @return A \code{prcomp} object.
#' @author Raphael Scherrer
#' @export

spectral_pca <- function(specdata) {

  # Locate wavelength columns
  isWavelength <- grep("wl", colnames(specdata))

  # Calculate principal components
  dewlaPCA <- prcomp(specdata[,isWavelength], scale = T)

  return(dewlaPCA)

}
