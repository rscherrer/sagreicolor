#' Plot PC1 against mean reflectance
#'
#' This function plots PC1 against mean reflectance.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @return The results of a Pearson's correlation test between the two.
#' @author Raphael Scherrer
#' @export

# Function to plot PC1 against mean reflectance
plot_pc1_brightness <- function(specdata) {

  plot(PC1 ~ meanrefl, data = specdata, pch = 16, xlab = "Mean reflectance", ylab = "PC1")
  with(specdata, cor.test(PC1, meanrefl))

}
