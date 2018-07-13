#' Fit and analyze generalized least squares models
#'
#' This function fits GLS models to a set of dependent variables, one after the other. It compares several residual variance structures with AIC and retains the best. The fixed effects of the model are tested by model comparison using likelihood ratio tests.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot the residuals of the best model or not.
#' @return A list one element per dependent variable. Each element contains an AIC table comparing variance structures, and the ANOVA table summarizing the sequential LRTs of fixed effects. The function also plots the residuals (if \code{plotit = T}) and prints to the prompt the best model with the best variance structure.
#' @author Raphael Scherrer
#' @details GLS models are used to account for heteroscedasticity of the residuals across groups (Pinheiro and Bates 2000). The workflow here follows recommendations of Zuur et al. 2009.
#' @export

dewlap_gls <- function(specdata, vars, plotit = F) {

  library(nlme)

  if(plotit) par(mfrow = c(2,2))

  # For each dependent variable...
  gls.res <- lapply(vars, function(curr.var) {

    message(curr.var)

    specdata$X <- specdata[,curr.var]

    message("Fitting generalized least squares model")

    # Fit a generalized least square model and allow one residual variance for each group
    mod.full <- nlme::gls(X ~ island*habitat, data = specdata, weights = varIdent(form = ~ 1 | island*habitat))

    # Does it perform better than simpler models?

    message("Testing the residual variance structure")

    # Model with one residual variance per island
    mod.red1 <- nlme::gls(X ~ island*habitat, data = specdata, weights = varIdent(form = ~ 1 | island))

    # Model with one residual variance per habitat
    mod.red2 <- nlme::gls(X ~ island*habitat, data = specdata, weights = varIdent(form = ~ 1 | habitat))

    # Model with a single, global residual variance
    mod.null <- nlme::gls(X ~ island*habitat, data = specdata)

    # Compare AIC
    aic.res <- AIC(mod.full, mod.red1, mod.red2, mod.null)

    # Retain the best model
    mod.best <- get(rownames(aic.res)[aic.res$AIC == min(aic.res$AIC)])

    message("Refitting the best model with maximum likelihood")

    # Re-fit the model with maximum likelihood to estimate the fixed effect parameters accurately
    mod.best.ml <- update(mod.best, method = "ML")

    message("Testing the fixed effects")

    # Sequentially remove terms to produce reduced models
    mod.seq1 <- update(mod.best.ml, model. = ~ island + habitat)
    mod.seq2 <- update(mod.best.ml, model. = ~ island)
    mod.seq3 <- update(mod.best.ml, model. = ~ 1)

    # Test the parameters of the model
    anova.res <- anova(mod.best.ml, mod.seq1, mod.seq2, mod.seq3)

    # Plot residuals versus fitted values
    residuals <- resid(mod.best)
    fitted <- fitted(mod.best)
    plot(residuals ~ fitted, main = curr.var)

    # Output
    gls.res <- list(aic.res, anova.res)
    names(gls.res) <- c("AIC", "ANOVA")

    # Show best model
    print(mod.best)

    return(gls.res)

  })

  # Plot the residuals
  if(plotit) par(mfrow = c(1,1))


  return(gls.res)

}
