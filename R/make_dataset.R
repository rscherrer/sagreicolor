#' Make spectral dataset
#'
#' This function produces the dataset from raw data.
#'
#' @param what_habitats A character vector. What habitats to retain?
#' @param spot What sport on the dewlap? 1 = near the throat, 2 = center.
#' @return A data frame, a subset of \code{specdata}.
#' @author Raphael Scherrer
#' @export

# Function to produce the dataset from raw data
make_dataset <- function(what_habitats, spot = 2) {

  library(pavo)

  # Read spectral data
  specs <- readRDS("data/specs.rds")

  # Read locality data
  locale <- readRDS("data/locale.rds")

  # Process the spectra into a data frame
  specdata <- sagreicolor::process_spectra(specs, locale, spot)

  # Reduce to the wanted habitats
  specdata <- sagreicolor::subset_habitats(specdata, what_habitats, inclusive = T)

  # Remove missing entries from spectral dataset
  specs <- sagreicolor::df2rspec(specdata)

  # Calculate mean reflectance
  specdata$meanrefl <- unlist(summary(specs, 'B2'))

  # Calculate cut-on wavelength
  specdata$cuton <- unlist(summary(specs, 'H3', wlmin = 450, wlmax = 700))

  # Calculate red and UV reflectance
  specdata$Red <- unlist(summary(specs, 'S1R'))
  specdata$UV <- unlist(summary(specs, 'S1U'))

  # Compute Principal Components
  dewlaPCA <- sagreicolor::spectral_pca(specdata)

  # Bind Principal Components to the data frame
  specdata <- cbind(specdata, dewlaPCA$x)

  # Save the data
  write.csv(specdata, "data/specdata.csv")

  return(specdata)

}
