% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/matrix_contrasts.R
\name{matrix_contrasts}
\alias{matrix_contrasts}
\title{Matrix of contrast weights}
\usage{
matrix_contrasts(specdata)
}
\arguments{
\item{specdata}{A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".}
}
\value{
A matrix of contrast weights. Each row is a contrast. Each column is a habitat on a given island. Each row is filled with zeros, except the two habitats to compare, which are given values 1 and -1.
}
\description{
This function assembles multiple vectors of contrasts weights into a matrix. All pairwise contrasts between habitats on each island are computed separately. Contrasts between habitats across islands are not considered.
}
\author{
Raphael Scherrer
}
