#' Boxplot of color metrics across islands and habitats
#'
#' This function makes a boxplot of a given color metric across all islands and all habitats. It can save the plot as a PDF file.
#'
#' @param specdata A data frame containing the variable to plot. It should have a column "island" and a column "habitat".
#' @param var The name of the variable to plot, as it appears in \code{colnames(specdata)}.
#' @param filename A string. Name of the PDF file in which to save the plot. Does not save PDF if not specified. Can also be a path to the file to be created. Don't forget to add ".pdf" at the end!
#' @param isJitter Logical. Whether to plot jittered points instead of boxplots. Good way to view outliers.
#' @return A plot.
#' @author Raphael Scherrer
#' @export

do_boxplots = function(specdata, var, filename = NULL, isJitter = F, ...) {

  Y = specdata[,var]

  # Set colors
  col <- factor(rep(inames, each=nhab))
  if(isJitter) {
    colBoxes <- "white"
    bordBoxes <- "white"
  } else {
    colBoxes <- col
    bordBoxes <- "black"
  }

  # Get names
  habitatNames <- levels(specdata$habitat)
  islandNames <- levels(specdata$island)

  # Create labels
  habitatLabels <- rep(substr(habitatNames, 1, 3), length(islandNames))
  islandLabels <- gsub("([a-z])([A-Z])", "\\1 \\2", islandNames)

  # Offset parameter
  a <- 0.2

  # Position of habitat labels
  yHabitatLabels <- min(Y) - a / 4 * (max(Y) - min(Y))
  xHabitatLabels <- 1:length(habitatLabels)

  # Position of island labels
  yIslandLabels <- min(Y) - a / 2 * (max(Y) - min(Y))
  xIslandLabels <- seq(from = median(seq_along(habitatNames)), by = 4, length.out = length(islandNames))

  # Plot
  if(!is.null(filename)) pdf(filename)

  # Set graphical parameters
  defaultPar <- par()
  par(oma = c(4,0,0,0), xpd = NA)

  # Boxplot
  with(specdata, boxplot(Y ~ habitat:island, col=colBoxes, border=bordBoxes, axes=F, outline=F))

  # Fill with points if wanted
  if(isJitter) with(specdata, stripchart(Y ~ habitat:island, vertical = T, method = "jitter", add = T, pch = 20, col = col))

  # Add Y-axis
  axis(2, las=1)

  # Add habitat labels
  text(x = xHabitatLabels, y = yHabitatLabels, habitatLabels, cex = .8, adj = 1, srt = 60)

  # Add island labels
  text(x = xIslandLabels, y = yIslandLabels, islandLabels, cex = .8, adj = 1, srt = 60)

  # Reset graphical parameters
  par(oma = defaultPar$oma, xpd = defaultPar$xpd)

  if(!is.null(filename)) dev.off()

}
