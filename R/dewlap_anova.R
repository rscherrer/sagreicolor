#' Multiple analyses of variance (ANOVA)
#'
#' This function performs multiple two-way ANOVAs, one on each dependent variable. The effects of island, habitat and their interaction are tested.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @return Nothing is returned. The output of the \code{anova} function is printed to the command prompt.
#' @author Raphael Scherrer
#' @export

# Function to perform (multiple) ANOVA
dewlap_anova <- function(specdata, vars) {

  anova.res <- lapply(vars, function(curr.variable) {

    specdata$X <- specdata[,curr.variable]

    # Fit a linear model
    mod <- lm(X ~ island*habitat, data = specdata)

    anova.res <- anova(mod)

    return(anova.res)

  })

  return(anova.res)

}
