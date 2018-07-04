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

  inames = levels(specdata$island)
  nhab = nlevels(specdata$habitat)

  # Set colors
  col <- factor(rep(inames, each=nhab))
  if(isJitter) {
    colBoxes <- "white"
    bordBoxes <- "white"
  } else {
    colBoxes <- col
    bordBoxes <- "black"
  }

  # Plot
  if(!is.null(filename)) pdf(filename)
  with(specdata, boxplot(Y ~  habitat:island, col=colBoxes, border=bordBoxes, axes=F, outline=F))
  if(isJitter) with(specdata, stripchart(Y ~ habitat:island, vertical = T, method = "jitter", add = T, pch = 20, col = col))
  axis(2, las=1)
  if(!is.null(filename)) dev.off()

}
