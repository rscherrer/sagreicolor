#' Plot residuals of dewlap color
#'
#' This function plots the residuals of a set of dependent variables across islands and habitats. Residuals are taken as the deviation from the group mean. Residuals are plotted against group means (fitted values).
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @return Nothing. Just one (or more) scatter plot(s).
#' @author Raphael Scherrer
#' @export

# Function to plot residuals of a two-way ANOVA (simple regression)
plot_residuals <- function(specdata, vars) {
  
  residuals <- sapply(vars, function(curr.variable) {
    
    specdata$X <- specdata[,curr.variable]
    
    # Fit a linear model
    mod <- lm(X ~ island*habitat, data = specdata)
    
    # Check the residuals
    par(mfrow = c(2,2))
    plot(mod, main = curr.variable)
    par(mfrow = c(1,1))
    
    return(resid(mod))
    
  })
  
  return(residuals)
  
}