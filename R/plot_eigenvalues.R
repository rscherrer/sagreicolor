#' PCA eigenvalues
#'
#' This function plots the eigenvalues of the color space.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @return The cumulative distribution of explained variance.
#' @author Raphael Scherrer
#' @export

# Function to plot PC eigenvalues
plot_eigenvalues <- function(specdata) {
  # Compute Principal Components
  dewlaPCA <- sagreicolor::spectral_pca(specdata)

  plot(dewlaPCA, main = NULL, las = 1)

  return(cumsum(dewlaPCA$sdev) / sum(dewlaPCA$sdev))
  # 91% of the variance is explained with only 4 components!
}
