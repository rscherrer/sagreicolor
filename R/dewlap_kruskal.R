#' Perform Kruskal Wallis test on each island
#'
#' This function tests for differences in one or several dependent variables between habitats on each island. P-values are adjusted. The procedure is non-parametric and is rank-based.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @return A table showing each pair of habitats that differed significantly in mean.
#' @author Raphael Scherrer
#' @export

# Function to perform Kruskal-Wallis's test on each island separately
dewlap_kruskal <- function(specdata, vars, method = "bonferroni") {

  # For each variable
  kruskal.res <- lapply(vars, function(curr.var) {

    # For each island
    kruskal.res <- sapply(levels(specdata$island), function(curr.island) {

      # Subset of the data
      specdata <- droplevels(subset(specdata, island == curr.island))

      # Current variable
      Y <- specdata[,curr.var]

      # Kruskal-Wallis test
      kruskal.res <- kruskal.test(Y ~ habitat, data = specdata)

      # Return chi square, df and p-value
      out <- with(kruskal.res, c(statistic, parameter, p.value))

      return(out)

    })

    kruskal.res <- t(kruskal.res)
    colnames(kruskal.res) <- c("chisq", "df", "p.value")
    kruskal.res <- as.data.frame(kruskal.res)

    # Adjust p-values
    kruskal.res$padj <- p.adjust(kruskal.res$p.value, method)

    return(kruskal.res)

  })

  names(kruskal.res) <- vars

  return(kruskal.res)

}
