#' Test multivariate normality across groups
#'
#' This function tests for multivariate normality of the data across groups (island-habitat combinations). If performs multiple Shapiro-Wilk's multivariate normality tests. P-values are adjusted to account for multiple testing.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot adjusted P-values.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A vector of adjusted P-values for each dependent variable and each group. If \code{plotit = T}, also returns a bar plot of the adjusted P-values with a dashed line at 0.05.
#' @author Raphael Scherrer
#' @export


# Function to check multivariate normality across groups
check_multinorm <- function(specdata, vars, plotit = T, method = "bonferroni") {
  
  library(mvnormtest)
  
  # Extract dependent variables
  Y <- as.matrix(specdata[,vars])
  
  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)
  
  # Check within-group multivariate normality assumption
  pShapiro <- sapply(groups, function(curr.group) {
    
    # Take a subset of Y
    Y <- Y[groups == curr.group,]
    
    # Apply normality test
    shapiro <- mvnormtest::mshapiro.test(t(Y))
    
    # Extract p-value
    return(shapiro$p.value)
    
  })
  
  # Correct for multiple testing
  padjShapiro <- p.adjust(pShapiro, method)
  
  # Visualize the results
  if(plotit) {
    barplot(padjShapiro, ylab = "Adjusted P-value", main = "Multivariate test of normality")
    abline(h = 0.05, lty = 2)
  }
  
  return(padjShapiro)
  
}