#' Test multivariate normality across groups
#'
#' This function tests for multivariate normality of the data across groups (island-habitat combinations). If performs multiple Shapiro-Wilk's multivariate normality tests. P-values are adjusted to account for multiple testing.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot adjusted P-values.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A data frame with each group, its multivariate normality W statistic, P-value and adjusted P-value. If \code{plotit = T}, also returns a bar plot of the adjusted P-values with a dashed line at 0.05.
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
  mshapiro.res <- sapply(groups, function(curr.group) {

    # Take a subset of Y
    Y <- Y[groups == curr.group,]

    # Apply normality test
    mshapiro.res <- mvnormtest::mshapiro.test(t(Y))

    mshapiro.res <- with(mshapiro.res, c(statistic, p.value))

    # Extract p-value
    return(mshapiro.res)

  })

  mshapiro.res <- t(mshapiro.res)
  colnames(mshapiro.res) <- c("statistic", "p.value")
  mshapiro.res <- as.data.frame(mshapiro.res)

  # Correct for multiple testing
  mshapiro.res$padj <- p.adjust(mshapiro.res$p.value, method)

  mshapiro.res <- cbind(groups, mshapiro.res)

  colnames(mshapiro.res)[1] <- "group"

  # Visualize the results
  if(plotit) {
    barplot(mshapiro.res$padj, ylab = "Adjusted P-value", main = "Multivariate test of normality")
    abline(h = 0.05, lty = 2)
  }

  return(mshapiro.res)

}
