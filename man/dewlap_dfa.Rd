% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dewlap_dfa.R
\name{dewlap_dfa}
\alias{dewlap_dfa}
\title{Discriminant Function Analysis (DFA) of dewlap color}
\usage{
dewlap_dfa(specdata, vars, type = "linear", plotit = T, CV = F,
  method = "bonferroni")
}
\arguments{
\item{specdata}{A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".}

\item{vars}{A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.}

\item{type}{A character. Type of discriminant analysis. \code{"linear"} (default) or \code{"quadratic"}.}

\item{plotit}{Logical. Whether to plot the loadings of the data points on the discriminant functions (applicable only if \code{type == "linear"}). Also, whether to plot adjusted P-values, across islands, of the MANOVAs and binomial tests.}

\item{CV}{Logical. Whether to jacknife the predictions (cross-validation).}

\item{method}{A character. The method to use for P-value correction.}
}
\value{
A data frame with columns:
\itemize{
\item "observed", "expected", "n", "p.binom", "df", "wilks", "approx.F", "num.df", "denom.df", "p.manova"
\item \code{observed}: the observed number of succesful assignments
\item \code{expected}: the number of succesful assignments expected by change
\item \code{n}: the number of assignments
\item \code{p.binom}: the adjusted P-value of the binomial test
\item \code{df}: the degrees of freedom of the MANOVA
\item \code{wilks}: Wilk's lambda
\item \code{approx.F}: the approximate F-value calculated from Wilk's lambda
\item \code{num.df}: numerator degrees of freedom for F-test
\item \code{denom.df}: denumerator degrees of freedom for F-test
\item \code{p.manova}: adjusted P-value of the MANOVA
}
}
\description{
This function performs multiple DFA on each island separately. The algorithm fits functions that best discriminate among habitats based on dewlap color data. The type of DFA can be linear (LDA) or quadratic (QDA). Each data point is then classified i.e. its original habitat is predicted based on the discriminant functions. Predictions can be jacknifed (leave-one-out). The significance of the classification is assessed in two ways. First, a MANOVA tests for differences in dependent variables across predicted groups. Second, a binomial test assesses the departure of the observed number of successful predictions from the null expectation.
}
\note{
Quadratic discriminant analysis doesn't require homogeneous covariance matrices among groups, unlike linear (Robert I. Kabacoff, Quick-R, https://www.statmethods.net/advstats/discriminant.html).
}
\author{
Raphael Scherrer
}
