#' Multivariate analysis of variance (MANOVA) of dewlap color
#'
#' This function performs a two-way MANOVA of dewlap color data cross islands and habitats. It can also perform multiple two-way ANOVAs, one on each dependent variable (equivalent to the \code{dewlap_anova} function).
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param perVariable Logical. Whether to perform multiple two-way ANOVAs, one on each dependent variable. Defaults to \code{TRUE}.
#' @return Nothing is returned. The summary of the MANOVA (and the multiple ANOVAs, if applicable) is printed to the command prompt.
#' @author Raphael Scherrer
#' @export

# Function to perform MANOVA on spectral data
dewlap_manova <- function(specdata, vars, perVariable = T) {
  
  # Extract a matrix of principal components
  Y <- as.matrix(specdata[,vars])
  
  # Performing MANOVA
  MANOVA <- manova(Y ~ island*habitat, data = specdata)
  
  # Checking the result
  print(summary(MANOVA, test = "Wilks"))
  
  # Which dimensions differ across groups?
  if(perVariable) summary.aov(MANOVA)
  
}