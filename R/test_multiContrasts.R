#' Test multivariate contrasts
#'
#' This function tests for significant contrasts between multivariate habitat-means within islands. The procedure is parametric and involves calculation of Wilk's lambda and P-value adjustment.
#'
#' @param W A matrix indicating all contrasts to be tested, or a vector if only one contrast. The number of rows is the number of contrasts to test. The number of columns is the number of groups. The matrix/vector is filled with zeros, except for a 1 and a -1 at the position of the groups that are to be contrasted.
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param method Correction method for adjusting p-values.
#' @return A data frame with the results of each Wilk's lambda test in rows. In columns,
#' \itemize{
#' \item{\code{hab1}, \code{hab2} Habitats being compared.}
#' \item{\code{Wilks} Wilk's lambda.}
#' \item{\code{approx.F} F-value computed from Wilk's lambda.}
#' \item{\code{df1}, \code{df2} Numerator and denominator degrees of freedom of the F distribution.}
#' \item{\code{p.value} P-value computed from the F-distribution.}
#' \item{\code{p.adj} Corrected P-value.}
#' }
#' @author Raphael Scherrer
#' @note The parametric procedure was adapted from Charles Zaiontz's post on multivariate contrast testing in Excel: http://www.real-statistics.com/multivariate-statistics/multivariate-analysis-of-variance-manova/manova-follow-up-contrasts/.
#' @export

# Function to test for significant contrasts between multivariate means (parametric)
test_multiContrasts <- function(W, specdata, vars, method = "bonferroni") {

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
  groups <- unique(grouping)

  # Multivariate means for each component in each group
  M <- as.matrix(apply(Y, 2, function(y) tapply(y, grouping, mean)), ncol = nlevels(specdata$island), nrow = nlevels(specdata$habitat))
  M <- na.exclude(M)

  # Counts in each group (including empty groups)
  N <- table(grouping)
  N <- N[N != 0]

  # For each contrast (row in the matrix)
  testContrasts <- apply(W, 1, function(W) {

    # Calculate weighted sum of squared weights
    SW <- sum(W^2 / N)

    # Calculate the vector of contrasts
    C <- M[W == 1,] - M[W == -1]

    # Calculate the hypothesis matrix
    H <- (cbind(C) %*% rbind(C)) / SW

    # Calculate the residual error matrices in each group
    residuals <- lapply(seq_along(groups), function(i) {

      # Current group
      curr.group <- groups[i]

      # Subset of the data
      X <- subset(Y, grouping == curr.group)

      # Calculate all deviations from the mean
      deviations <- X - na.exclude(M)[i,]

      # Multiply each deviation vector by its transpose
      devTdev <- lapply(1:nrow(deviations), function(j) deviations[j,] %*% t(deviations[j,]))

      return(sagreicolor::add_matrices(devTdev))
    })

    # Calculate the total residual error matrix
    E <- sagreicolor::add_matrices(residuals)

    # Calculate Wilk's lambda
    Lambda <- det(E) / det(H + E)

    # Calculate degrees of freedom
    df1 <- ncol(Y) #no.variables
    df2 <- nrow(Y) - length(groups) - ncol(Y) + 1 #no.observations - no.groups - no.variables + 1

    # Calculate approx. F
    F.stat <- (1 - Lambda) / Lambda * df2 / df1

    # Calculate p-value from an F distribution
    p.value <- 1 - pf(F.stat, df1, df2)

    # Output
    out <- c(Lambda, F.stat, df1, df2, p.value)
    names(out) <- c("Wilks", "approx.F", "df1", "df2", "p.value")
    return(out)

  })

  # Convert to data frame
  testContrasts <- as.data.frame(t(testContrasts))

  # Adjust p-values to correct for multiple testing
  testContrasts$padj <- p.adjust(testContrasts$p.value, method = method)

  # Names of the habitats in each contrast
  contrasts <- apply(W, 1, function(W) {
    idx <- W %in% c(-1, 1)
    return(groups[idx])
  })

  contrasts <- t(contrasts)

  testContrasts <- cbind(contrasts, testContrasts)
  colnames(testContrasts)[c(1,2)] <- c("hab1", "hab2")

  return(testContrasts)

}
