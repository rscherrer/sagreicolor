% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dewlap_kruskal.R
\name{dewlap_kruskal}
\alias{dewlap_kruskal}
\title{Perform Kruskal Wallis test on each island}
\usage{
dewlap_kruskal(specdata, vars, method = "bonferroni")
}
\arguments{
\item{specdata}{A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".}

\item{vars}{A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.}

\item{method}{A character, the method used for P-value correction. See \code{?p.adjust}.}
}
\value{
A table showing each pair of habitats that differed significantly in mean.
}
\description{
This function tests for differences in one or several dependent variables between habitats on each island. P-values are adjusted. The procedure is non-parametric and is rank-based.
}
\author{
Raphael Scherrer
}
