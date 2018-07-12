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
  a <- 0.1

  # Position of habitat labels
  yHabitatLabels <- min(Y) - a * (max(Y) - min(Y))
  xHabitatLabels <- 1:length(habitatLabels)

  # Position of island labels
  yIslandLabels <- min(Y) - 2 * a * (max(Y) - min(Y))
  xIslandLabels <- seq(from = median(seq_along(habitatNames)), by = 4, length.out = length(islandNames))

  # Plot
  if(!is.null(filename)) pdf(filename)

  # Set graphical parameters
  defaultPar <- par()
  par(oma = c(10,0,0,0), xpd = NA, pin = c(4,3))

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
  with(defaultPar, par(oma = oma, xpd = xpd, pin = pin))

  if(!is.null(filename)) dev.off()

}
