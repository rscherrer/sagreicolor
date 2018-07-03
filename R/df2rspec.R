#' Convert to \code{rspec} with transposition
#'
#' This function converts a reflectance data frame with wavelengths in columns into an \code{rspec} data frame (package \code{pavo}) with wavelengths as rows.
#'
#' @param df A data frame of reflectance to convert. Columns for wavelengths must have "wl" preceding the wavelength in their name, e.g. "wl500" for 500nm. There is one row per measurement.
#' @return An \code{rspec} data frame.
#' @author Raphael Scherrer

df2rspec <- function(df) {

  library(pavo)
  isWavelength <- grep('wl', colnames(df))
  wl <- as.numeric(gsub('wl', '', colnames(df)[isWavelength]))
  specs <- pavo::as.rspec(cbind(wl = wl, t(df[, isWavelength])))
  return(specs)

}
