#' Test homoscedasticity of the residuals across groups
#'
#' This function tests for the homogeneity of the residuals across groups. It performs one Bartlett's test for each dependent variable. P-values are corrected.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A vector of adjusted P-values, one for each dependent variable.
#' @author Raphael Scherrer
#' @export

# Function to test for homogenetiy of variance among groups for each dependent variable
check_variances <- function(specdata, vars, method = "bonferroni") {
  
  # Extract dependent variables
  Y <- as.matrix(specdata[,vars])
  
  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)
  
  pBartlett<- apply(Y, 2, function(curr.variable) {
    
    bartlett <- bartlett.test(curr.variable ~ grouping)
    return(bartlett$p.value)
    
  })
  
  return(p.adjust(pBartlett, method))
  
}