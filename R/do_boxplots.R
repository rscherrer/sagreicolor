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

do_boxplots = function (specdata, var, filename = NULL, isJitter = F, ...)
{
  Y = specdata[, var]
  inames <- levels(specdata$island)
  nhab <- nlevels(specdata$habitat)
  col <- factor(rep(inames, each = nhab))
  if (isJitter) {
    colBoxes <- "white"
    bordBoxes <- "white"
  }
  else {
    colBoxes <- col
    bordBoxes <- "black"
  }
  habitatNames <- levels(specdata$habitat)
  islandNames <- levels(specdata$island)
  habitatLabels <- rep(substr(habitatNames, 1, 3), length(islandNames))
  islandLabels <- gsub("([a-z])([A-Z])", "\\1 \\2", islandNames)
  a <- 0.1
  yHabitatLabels <- min(Y) - a * (max(Y) - min(Y))
  xHabitatLabels <- 1:length(habitatLabels)
  yIslandLabels <- min(Y) - 2 * a * (max(Y) - min(Y))
  xIslandLabels <- seq(from = median(seq_along(habitatNames)),
                       by = length(habitatNames), length.out = length(islandNames))
  ylim <- c(min(Y) - a * (max(Y) - min(Y)), max(Y))
  if (!is.null(filename))
    pdf(filename)
  defaultPar <- par()
  par(oma = c(5, 0, 0, 0), xpd = NA, pin = c(5, 4))
  with(specdata, boxplot(Y ~ habitat:island, col = colBoxes,
                         border = bordBoxes, axes = F, outline = F, ylim = ylim,
                         ylab = var))
  if (isJitter)
    with(specdata, stripchart(Y ~ habitat:island, vertical = T,
                              method = "jitter", add = T, pch = 20, col = col))
  axis(2, las = 1)
  text(x = xHabitatLabels, y = yHabitatLabels, habitatLabels,
       cex = 0.8, adj = 1, srt = 60)
  text(x = xIslandLabels, y = yIslandLabels, islandLabels,
       cex = 0.8, adj = 1, srt = 60)
  with(defaultPar, par(oma = oma, xpd = xpd, pin = pin))
  if (!is.null(filename))
    dev.off()
}
