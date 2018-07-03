#' Dewlap color space biplot
#'
#' This function plots individual dewlaps in the color space defined by principal components based on reflectance. It can plot each individual lizard, or centroids of habitats per island.
#'
#' @param pca A \code{prcomp} object.
#' @param groups A data frame containing information about island and habitat for each lizard.
#' @param i,j Which components to plot against each other? Defaults to i = 2 and j = 3.
#' @param plotCentroids Logical. Whether to plot habitat and island centroids (T, default) or all observations (F).
#' @return A plot.
#' @author Raphael Scherrer
#' @export

dewlap_biplot <- function(pca, groups, i = 2, j = 3, plotCentroids = T){

  # Explained variance
  expvars <- sapply(c(i,j), calc_expvar, pca = pca)

  # General graphical parameters
  main <- "Biplot"
  xlab <- paste0("PC", i, " (", round(expvars[1]*100, 2), "% variance)")
  ylab <- paste0("PC", j, " (", round(expvars[2]*100, 2), "% variance)")

  # If you want to plot only centroids (clarity)...
  if(plotCentroids) {

    # Get island-wide centroids
    iCentroids <- t(sapply(levels(groups$island), function(curr.lev) {
      colMeans(pca$x[groups$island == curr.lev,])
    }))

    # Convert groups into a factor
    fac <- as.factor(with(groups, paste(island, habitat)))
    levs <- levels(fac)

    # Get habitat-specific centroids
    hCentroids <- t(sapply(levs, function(curr.lev) {
      colMeans(subset(pca$x, fac == curr.lev))
    }))

    # Reduce group data frame to centroid level
    groups <- as.data.frame(do.call("rbind", strsplit(as.character(levs), " ")))
    colnames(groups) <- c("island", "habitat")

    # Specific graphical parameters
    pch <- 16 + as.numeric(groups$habitat)
    col <- groups$island

    # Biplot of habitat centroids
    plot(hCentroids[,i], hCentroids[,j], pch = pch, col = col, main = main, xlab = xlab, ylab = ylab, las = 1, asp = 1)

    # Connect habitats with islands with segments
    x0 <- hCentroids[,i]
    y0 <- hCentroids[,j]
    x1 <- unlist(mapply(function(x, y) rep(x, y), iCentroids[,i], summary(groups$island)))
    y1 <- unlist(mapply(function(x, y) rep(x, y), iCentroids[,j], summary(groups$island)))

    segments(x0, y0, x1, y1, col = groups$island)

    # Add text at island centroid positions
    labels <- substr(levels(groups$island), 1, 2)
    text(iCentroids[,i], iCentroids[,j], labels)

  } else {

    # Otherwise, plot all the points

    # Specific graphical parameters
    pch <- 16 + as.numeric(groups$habitat)
    col <- groups$island

    # Biplot
    with(pca, plot(x[,i], x[,j], main = main, xlab = xlab, ylab = ylab, pch = pch, col = col, las = 1, asp = 1))

  }

  # Legend
  legend("topleft", legend = unique(groups$island), col = unique(col), pch = 16, bty = "n")
  legend("bottomright", legend = unique(groups$habitat), pch = unique(pch), col = "black", bty = "n")

}
