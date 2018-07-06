#' Permutational multivariate analysis of variance (PERMANOVA) of dewlap color
#'
#' This function performs a two-way PERMANOVA of dewlap color data cross islands and habitats. It is the non-parametric version of MANOVA.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nperm Integer. The number of permutations to be computed.
#' @return The output of the \code{adonis} function from vegan, containing the PERMANOVA results.
#' @author Raphael Scherrer
#' @export

# Function to perform non-parametric MANOVA
dewlap_permanova <- function(specdata, vars, nperm = 999) {
  
  library(vegan)
  
  # Extract dependent variables
  Y <- as.matrix(specdata[,vars])
  
  # Distance matrix
  D <- dist(Y)
  
  # Perform (sequential) PERMANOVA
  perm <- vegan::adonis(D ~ island*habitat, data = specdata, permutations = nperm)
  
  return(perm)
  
}
