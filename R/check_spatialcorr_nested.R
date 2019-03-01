#' Test spatial autocorrelation of the residuals within islands
#'
#' This function tests for positive spatial autocorrelation of the (uni- or multivariate) residuals, using either Moran's I or Mantel's test. This function essentially performs the task of \code{check_spatialcorr} but within each island.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param plotit Logical. Whether to plot residuals versus latitude and longitude.
#' @param test A character, what test to perform. \code{"moran"} (default) performs Moran's I test on each dependent variable. \code{"mantel"} performs Mantel's test to compare distance matrices in residual space and geographic space, meaning that it can perform a single test on multivariate residuals if several dependent variables are input.
#' @param method A character, the method used for P-value correction. See \code{?p.adjust}.
#' @param model A character string, either \code{lm} or \code{gls}, defining what type of linear model to extract residuals from. If GLS is chosen, AIC comparison of variance structures is performed (one residual variance per island, per habitat or per island per habitat), and the Pearson residuals of the best model are extracted (note that Pearson residuals are identical to raw residuals for a regular linear model, whereas they differ for GLS models with particular variance structures -- they are adjusted such that heteroscedasticity is reduced).
#' @return A list of outputs, one per island. Either the output of the \code{mantel.test} function from ape, or a table summarizing the (multiple) Moran's I test(s). If \code{plotit = T}, also returns a scatter plot of the residuals versus latitude and longitude.
#' @note Beware that Mantel's test is a permutation test (999 permutations), and will take longer tha Moran's I test to run. Also, note that latitudes and longitudes are used to calculate geographical distances at the surface of the Earth using the Haversine distance function implemented in the package \code{geosphere}.
#' @author Raphael Scherrer
#' @export

# Function to perform autocorrelation tests within islands
check_spatialcorr_nested <- function(specdata, vars, plotit = T, test = "moran", method = "bonferroni", model = "gls") {

  library(ape)
  library(geosphere)
  library(nlme)

  res <- lapply(levels(specdata$island), function(curr_island) {

    specdata <- subset(specdata, island == curr_island)

    # Extract residuals
    residuals <- sapply(vars, function(curr.variable) {

      specdata$X <- specdata[,curr.variable]

      # Fit a linear model
      if(model == "lm") {

        mod <- lm(X ~ island*habitat, data = specdata)

      } else if (model == "gls") {

        # Or a generalized least square model
        # Design several variance structures first
        mod1 <- gls(X ~ habitat, data = specdata, weights = varIdent(form = ~1 | habitat))
        mod2 <- gls(X ~ habitat, data = specdata)

        # AIC comparison and best model selection
        aicValues <- AIC(mod1, mod2)$AIC
        bestIdx <- which(aicValues == min(aicValues))
        mod <- get(c("mod1", "mod2")[bestIdx])


      }

      # Extract residuals
      # Note: Pearson residuals are the same as raw residuals for a regular linear model. They do differ for a GLS with more complex variance structure.
      residuals <- resid(mod, type = "pearson")

    })

    # Plots
    if(plotit) {
      ylab <- "Residuals"
      xlab1 <- "Latitude"
      xlab2 <- "Longitude"
      par(mfrow = c(2, 2))
      apply(residuals, 2, function(residuals) {
        plot(residuals ~ specdata$latitude, ylab = ylab, xlab = xlab1)
        plot(residuals ~ specdata$longitude, ylab = ylab, xlab = xlab2)
      })
      par(mfrow = c(1,1))
    }


    # Distance matrix in residual space
    dR <- as.matrix(dist(residuals))

    # Distance matrix in geography
    dG <- as.matrix(distm(specdata[, c("longitude", "latitude")], fun = distHaversine))

    # Weigth matrix is the inverse of the distance matrix (needed for Moran's test)
    w <- 1/dG

    # Replace infinite values (same locality) with zeros
    # These will not be taken into account in Moran's I calculations
    w[w == Inf] <- 0

    # Perform Mantel's test...
    if(test == "mantel") {

      message("Performing Mantel's test...")

      res <- ape::mantel.test(dR, dG, graph = F, alternative = "greater")
      res <- with(res, c(Z = z.stat, p.value = p))

    } else if(test == "moran") {

      message("Performing Moran's I test...")

      # Or perform (multiple) Moran's I tests
      res <- apply(residuals, 2, function(residuals) {
        ape::Moran.I(residuals, weight = w, alternative = "greater")
      })

      res <- as.data.frame(do.call("rbind", res))

      res$padj <- p.adjust(res$p.value, method)

    }

    return(res)

  })

  names(res) <- levels(specdata$island)

  return(res)

}
