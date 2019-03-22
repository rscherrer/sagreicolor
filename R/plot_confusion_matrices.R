#' Plot neural network confusion matrices
#'
#' This function plots the cumulated confusion matrices of the neural networks.
#'
#' @param sumconfs A list of confusion matrices.
#' @param saveto Where to save the PDF plot, optional
#' @param font Font to be used in the plots. Defaults to Helvetica.
#' @param colors Two colors to be used for incorrect (1) and correct (2) classifications
#' @author Raphael Scherrer
#' @export

# Function to plot the confusion matrices
plot_confusion_matrices <- function(sumconfs, saveto, font, colors = c("#fc8d62", "#66c2a5")) {

  library(extrafont)
  loadfonts(quiet = T)
  if (missing(font)) font <- "Helvetica"

  pdf(paste(saveto, "confusion_matrices.pdf", sep = "/"), width = 5.5, height = 8, family = font)
  par(mfrow = c(4, 3), mar = c(4, 1, 1, 1), oma = c(3, 0, 1, 0))
  lims <- c(-1.7, 1.7)
  inames <- add_spaces(names(sumconfs))
  mapply(function(confmat, curr.island) {

    cex <- c(confmat)
    y <- rep(rev(seq(-1, 1, length.out = ncol(confmat))), ncol(confmat))
    x <- rep(seq(-1, 1, length.out = ncol(confmat)), each = ncol(confmat))
    cols <- matrix(colors[1], ncol = ncol(confmat), nrow = nrow(confmat))
    diag(cols) <- colors[2]
    cols <- c(cols)
    plot(x, y, cex = cex / sum(cex) * 20, pch = 16, col = cols, axes = F, xlab = "", ylab = "", asp = 1, xlim = lims, ylim = c(-3, lims[2]))
    mtext(curr.island, side = 3, line = 0)
    text(x = c(-1, 0, 1), y = rep(-2.5, 3), labels = c("coa", "cop", "man"), srt = 60, cex = 1.2)
    text(x = rep(-2.5, 3), y = c(-1, 0, 1), labels = c("man", "cop", "coa"), cex = 1.2)

  }, sumconfs, inames)
  dev.off()

}
