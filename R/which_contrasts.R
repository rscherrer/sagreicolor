#' Get significant contrasts
#'
#' This function finds what habitat contrasts are significantly different from zero.
#'
#' @param pvalues A vector of P-values from a multiple comparison procedure (see \code{?test_contrasts} and \code{?test_multiContrasts}).
#' @param W A matrix of contrast weights.
#' @param groups A vector of character strings. The names of the groups corresponding to the columns in \code{W} (the groups compared in the multiple comparison).
#' @param alpha A numeric. The significance threshold to declare a contrast significant. Defaults to 0.05.
#' @return A table showing each pair of habitats that differed significantly in mean.
#' @author Raphael Scherrer
#' @export

# Function to know what contrasts are significant
which_contrasts <- function(pvalues, W, groups, alpha = 0.05) {

  # What contrasts are significant?
  whichContrast <- W[pvalues < alpha,]
  whichGroups <- apply(whichContrast, 1, function(W) {
    idx <- W %in% c(-1,1)
    whichGroups <- groups[idx]
    return(whichGroups)
  })
  return(whichGroups)
}
