#' Plot correlation between variables
#'
#' This function plots a correlation heatmap between PCs and wavelengths
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @return A correlation matrix.
#' @author Raphael Scherrer
#' @export

# Function to plot a correlation heatmap between PCs and wavelengths
plot_heatmap <- function(specdata) {

  library(fields)

  # Wavelengths
  X <- c('wl300', 'wl350', 'wl400', 'wl450', 'wl500', 'wl550', 'wl600', 'wl650', 'wl700')

  # Components
  Y <- c('PC2', 'PC3', 'PC4')

  # Calculate correlation coefficients and p-values
  cortest <- sapply(Y, function(Y) {
    sapply(X, function(X) {
      cortest <- cor.test(specdata[,Y], specdata[,X])
      cortest <- round(cortest$estimate,4)
      return(cortest)
    })
  }, simplify=F)

  # Rearrange the matrix
  cortest <- t(do.call("rbind", cortest))

  # Create a color gradient
  make_gradient <- colorRampPalette(c("#3288BD", "#FFFFBF", "#D53E4F"))
  col <- make_gradient(21)

  # Plot correlation heatmap
  image.plot(t(cortest), axes=F, col=col)

  # Add labels
  at = c(0, .5, 1)
  mapply(function(Y, at) mtext(Y, 3, .5, at=at), Y, at)
  at = seq(0, 1, 1/(length(X)-1))
  mapply(function(X, at) mtext(X, 2, .5, at=at, las=1), X, at)

  return(cortest)

}
