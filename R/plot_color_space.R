#' Plot color in PC space
#'
#' This wrap up function displays biplots along PC1, 2 and 3
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param plot_centroids Whether to plot only group centroids or all individual points.
#' @author Raphael Scherrer
#' @export

# Wrap up function to display all biplots
plot_color_space <- function(specdata, plot_centroids = T) {

  # Set groups
  groups <- specdata[,c("island", "habitat")]

  # Compute Principal Components
  dewlaPCA <- sagreicolor::spectral_pca(specdata)

  par(mfrow = c(2,2))

  # Plot centroids...
  if(plot_centroids) {

    par(mfrow = c(2,2))
    for(i in 1:2) {
      for(j in (i+1):3) {
        sagreicolor::dewlap_biplot(dewlaPCA, groups, i = i, j = j, plotCentroids = T)
      }
    }

  } else {

    # Or all points
    for(i in 1:2) {
      for(j in (i+1):3) {
        sagreicolor::dewlap_biplot(dewlaPCA, groups, i = i, j = j, plotCentroids = F)
      }
    }

    plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
    legend("topleft", legend = c("coppice", "coastal", "mangrove"), pch = c(18, 17, 16), title = "Habitat", bty= "n")
    legend("topright", legend = unique(specdata$island), pch = 16, col = seq_along(unique(specdata$island)), title = "Island", bty= "n", ncol = 3)
    par(mfrow = c(1,1))

  }

  par(mfrow = c(1,1))

}
