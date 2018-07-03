#' Explained variance of a principal component
#'
#' This function calculates the explained variance of a given principal component in a PCA.
#'
#' @param pca A \code{prcomp} object.
#' @param n An integer: what principal component?
#' @return A numeric, the proportion of variance in the dataset explained by the principal component.
#' @author Raphael Scherrer

calc_expvar <- function(pca, n) pca$sdev[n] / sum(pca$sdev)
