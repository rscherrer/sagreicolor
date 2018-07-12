#' Test normality across groups
#'
#' This function tests for normality of the data across groups (island-habitat combinations). If performs multiple Shapiro-Wilk's normality tests. P-values are adjusted to account for multiple testing.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot adjusted P-values.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A vector of adjusted P-values for each dependent variable and each group. If \code{plotit = T}, also returns a bar plot of the adjusted P-values with a dashed line at 0.05.
#' @author Raphael Scherrer
#' @export

# Function to check normality of each variable
check_normality <- function(specdata, vars, plotit = T, method = "bonferroni") {

  # What are the groups?
  grouping <- with(specdata, island:habitat)
  groups <- unique(grouping)

  # Perform Shapiro test on each variable
  shapiro.res <- apply(specdata[,vars], 2, function(x) {

    res <- tapply(x, grouping, function(x) {
      res <- shapiro.test(x)
      W <- res$statistic
      p <- res$p.value
      return(c(W, p))
    })
  })

  # Reshape the output
  shapiro.res <- lapply(shapiro.res, function(shapiro.res) {
    shapiro.res <- do.call("rbind", shapiro.res)
    colnames(shapiro.res) <- c("W", "p")
    return(shapiro.res)
  })

  # Extract p-values
  shapiro.p <- do.call("c", lapply(shapiro.res, function(shapiro.res) shapiro.res[,2]))

  # Compute adjusted p-values
  shapiro.padj <- p.adjust(shapiro.p, method)

  # Plot
  if(plotit) {
    barplot(shapiro.padj, main = "Normality", ylab = "Adjusted P-value", xlab = NULL)
    abline(h = 0.05, lty = 2)
  }

}
