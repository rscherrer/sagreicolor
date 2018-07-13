#' Test normality across groups
#'
#' This function tests for normality of the data across groups (island-habitat combinations). If performs multiple Shapiro-Wilk's normality tests. P-values are adjusted to account for multiple testing.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot adjusted P-values.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A list with one element per dependent variable. Each element is a data frame with Shapiro's W statistic, its P-value and adjusted P-value for each group (island-habitat combination). If \code{plotit = T}, also returns a bar plot of the adjusted P-values with a dashed line at 0.05.
#' @author Raphael Scherrer
#' @export

# Function to check normality of each variable
check_normality <- function(specdata, vars, plotit = T, method = "bonferroni") {

  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)

  shapiro.res <- lapply(vars, function(curr.variable) {

    shapiro.res <- sapply(groups, function(curr.group) {

      specdata <- droplevels(subset(specdata, grouping == curr.group))

      Y <- specdata[,curr.variable]

      # Shapiro-Wilk test
      shapiro.res <- shapiro.test(Y)

      shapiro.res <- with(shapiro.res, c(statistic, p.value))

      return(shapiro.res)

    })

    shapiro.res <- t(shapiro.res)
    colnames(shapiro.res) <- c("W", "p.value")
    shapiro.res <- as.data.frame(shapiro.res)
    shapiro.res$padj <- p.adjust(shapiro.res$p.value, method)

    return(shapiro.res)

  })

  # Plot
  if(plotit) {
    barplot(do.call("c", lapply(shapiro.res, function(x) x$padj)), main = "Normality", ylab = "Adjusted P-value", xlab = NULL)
    abline(h = 0.05, lty = 2)
  }

  names(shapiro.res) <- vars

  return(shapiro.res)

}
