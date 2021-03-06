% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dewlap_biplot.R
\name{dewlap_biplot}
\alias{dewlap_biplot}
\title{Dewlap color space biplot}
\usage{
dewlap_biplot(pca, groups, i = 2, j = 3, plotCentroids = T,
  whatLegend = "habitat")
}
\arguments{
\item{pca}{A \code{prcomp} object.}

\item{groups}{A data frame containing information about island and habitat for each lizard.}

\item{i, j}{Which components to plot against each other? Defaults to i = 2 and j = 3.}

\item{plotCentroids}{Logical. Whether to plot habitat and island centroids (T, default) or all observations (F).}

\item{whatLegend}{A character vector. What legend to display? If contains "island", the legend of colors for island will be displayed in the top-left corner. If contains "habitat", the legend of shapes for habitats will be displayed in the bottom right corner. "n" if legend should not be plotted.}
}
\value{
A plot.
}
\description{
This function plots individual dewlaps in the color space defined by principal components based on reflectance. It can plot each individual lizard, or centroids of habitats per island.
}
\author{
Raphael Scherrer
}
