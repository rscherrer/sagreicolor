#' Test homoscedasticity of the residuals across groups
#'
#' This function tests for the homogeneity of the residuals across groups. It performs one Bartlett's test for each dependent variable. P-values are corrected.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A data frame with Bartlett's K^2 statistic, the degrees of freedom of the approximate chi-squares distribution, P-value and adjusted P-value, for each dependent variable.
#' @author Raphael Scherrer
#' @export

# Function to test for homogenetiy of variance among groups for each dependent variable
check_variances <- function(specdata, vars, method = "bonferroni") {

  # Extract dependent variables
  Y <- as.matrix(specdata[,vars])

  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)

  bartlett.res <- apply(Y, 2, function(curr.variable) {

    bartlett.res <- bartlett.test(curr.variable ~ grouping)

    bartlett.res <- with(bartlett.res, c(statistic, parameter, p.value))

    return(bartlett.res)

  })

  bartlett.res <- t(bartlett.res)
  colnames(bartlett.res) <- c("K2", "df", "p.value")
  bartlett.res <- as.data.frame(bartlett.res)
  bartlett.res$padj <- p.adjust(bartlett.res$p.value, method)

  return(bartlett.res)

}
