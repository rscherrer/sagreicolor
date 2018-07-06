#' Test spatial autocorrelation of the residuals
#'
#' This function tests for positive spatial autocorrelation of the (uni- or multivariate) residuals, using either Moran's I or Mantel's test.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot residuals versus latitude and longitude.
#' @param test A character, what test to perform. \code{"moran"} (default) performs Moran's I test on each dependent variable. \code{"mantel"} performs Mantel's test to compare distance matrices in residual space and geographic space, meaning that it can perform a single test on multivariate residuals if several dependent variables are input.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return Either the output of the \code{mantel.test} function from ape, or a table summarizing the (multiple) Moran's I test(s). If \code{plotit = T}, also returns a scatter plot of the residuals versus latitude and longitude.
#' @note Beware that Mantel's test is a permutation test (999 permutations), and will take longer tha Moran's I test to run.
#' @author Raphael Scherrer
#' @export

# Function to check (uni or multivariate) spatial autocorrelation of the residuals
# Mantel allows one test for multivariate dataset
# Moran performs several tests, one for each
check_spatialcorr <- function(specdata, vars, plotit = T, test = "moran", method = "bonferroni") {
  
  library(ape)
  
  # Extract residuals
  residuals <- sapply(vars, function(curr.variable) {
    
    specdata$X <- specdata[,curr.variable]
    
    # Fit a linear model
    mod <- lm(X ~ island*habitat, data = specdata)
    
    # Extract residuals
    residuals <- resid(mod)
    
  })
  
  # Plots
  if(plotit) {
    ylab <- "Residuals"
    xlab1 <- "Latitude"
    xlab2 <- "Longitude"
    par(mfrow = c(2, 2))
    apply(residuals, 2, function(residuals) {
      plot(residuals ~ specdata$latitude, ylab = ylab, xlab = xlab1)
      plot(residuals ~ specdata$longitude, ylab = ylab, xlab = xlab2)
    })
    par(mfrow = c(1,1))
  }
  
  
  # Distance matrix in residual space
  dR <- as.matrix(dist(residuals))
  
  # Distance matrix in geography
  dG <- as.matrix(dist(specdata[,c("latitude", "longitude")]))
  
  # Weigth matrix is the inverse of the distance matrix (for Moran)
  w <- 1/dG
  
  # Replace infinite values (same locality) with zeros
  w[w == Inf] <- 0
  
  # Perform Mantel's test...
  if(test == "mantel") {
    
    print("Mantel's test")
    
    res <- ape::mantel.test(dR, dG, graph = F, alternative = "greater")
    
  } else if(test == "moran") {
    
    # Or perform (multiple) Moran's I tests
    res <- apply(residuals, 2, function(residuals) {
      ape::Moran.I(residuals, weight = w, alternative = "greater")
    })
    
    print("Moran's I test")
    
    res <- as.data.frame(do.call("rbind", res))
    
    res$p.adj <- p.adjust(res$p.value, method)
    
  }
  
  return(res)
  
}