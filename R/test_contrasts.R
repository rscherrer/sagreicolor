#' Test (multiple) univariate contrasts
#'
#' This function tests for significant contrasts between habitat-means within islands in one or several dependent variables independently. The procedure is either parametric (package multcomp) or non-parametric (package nparcomp). The function can also plot the 95\% confidence intervals of the differences in means.
#'
#' @param W A matrix indicating all contrasts to be tested, or a vector if only one contrast. The number of rows is the number of contrasts to test. The number of columns is the number of groups. The matrix/vector is filled with zeros, except for a 1 and a -1 at the position of the groups that are to be contrasted.
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param parametric Logical. If \code{TRUE} (default), parametric multiple comparisons are performed, otherwise a non-parametric procedure is used.
#' @param plotit Logical. If \code{TRUE}, plots 95\% confidence intervals of the contrasts. One plot per dependent variable.
#' @param method Correction method for adjusting p-values (parametric procedure).
#' @return A list with one element for each dependent variable in \code{vars}. Each element is a data frame with columns:
#' @author Raphael Scherrer
#' @note Parametric workflow as per Salvatore S. Mangiafico (https://rcompanion.org/rcompanion/h_01.html). Non-parametric workflow from the package description of nparcomp. Note also that performing multiple comparisons one dependent variable at a time is not so bad if variables have limited correlation, as in the case of principal components (Krishnaiah and Reising 2006).
#' @export

# Function to test for significant contrasts in a set of dependent variables independently (parametric or non-parametric)

test_contrasts <- function(W, specdata, vars, parametric = T, plotit = T, method = "bonferroni") {

  # Load packages
  if(parametric) {
    library(multcomp)
  } else {
    library(nparcomp)
  }

  # Security check
  if(!inherits(W, "matrix")) {
    if(!inherits(W, "numeric")) {
      stop("W must be either a matrix or a numeric vector.")
    } else {
      W <- rbind(W) #turn vector into single-row matrix
    }
  }

  # Matrix of dependent variables
  Y <- as.matrix(specdata[,vars])

  # Grouping
  grouping <- with(specdata, island:habitat)
  groups <- levels(as.factor(grouping))

  # Apply to each dependent variable
  testContrasts <- lapply(seq_len(ncol(Y)), function(i) {

    # Current dependent variable
    Y <- Y[,i]

    if(parametric) {

      message("Performing parametric multiple comparisons...")

      # Fit a linear model
      fit <- lm(Y ~ grouping)

      # General linear hypotheses with linear functions specified by multiple comparisons among groups as determined by the user-defined contrast weight matrix
      multcomp.res <- multcomp::glht(fit, linfct = mcp(grouping = W))

      # Plot 95% confidence intervals
      if(plotit) plot(multcomp.res)

      # Simultaneously test the general linear hypotheses
      testContrasts <- summary(multcomp.res, test = adjusted(method))

      # Adjusted confidence intervals
      confInt <- confint(testContrasts)

      testContrasts <- data.frame(
        estimate = testContrasts$test$coefficients,
        lower = confInt$confint[,"lwr"],
        upper = confInt$confint[,"upr"],
        statistic = testContrasts$test$tstat,
        padj = testContrasts$test$pvalues
      )

    } else {

      message("Performing non-parametric multiple comparisons...")

      testContrasts <- nparcomp::nparcomp(Y ~ grouping, data = specdata, type = "UserDefined", contrast.matrix = W, asy.method = "mult.t", info = F)

      # Plot 95% confidence intervals
      if(plotit) plot(testContrasts)

      testContrasts <- testContrasts$Analysis[,-1]

      colnames(testContrasts) <- c("estimate", "lower", "upper", "statistic", "padj")

    }

    # Add identity of the contrasts
    contrasts <- apply(W, 1, function(W) {
      idx <- W %in% c(-1,1)
      return(groups[idx])
    })

    contrasts <- t(contrasts)

    differences <- apply(contrasts, 1, function(contrast) {
      mean(Y[grouping == contrast[1]]) - mean(Y[grouping == contrast[2]])
    })

    testContrasts <- cbind(contrasts, differences, testContrasts)

    colnames(testContrasts)[c(1,2,3)] <- c("hab1", "hab2", "diff")

    return(testContrasts)

  })

  names(testContrasts) <- vars

  return(testContrasts)

}
