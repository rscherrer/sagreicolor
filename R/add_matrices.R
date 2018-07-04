#' Matrix addition
#'
#' This function calculates the sum of several matrices.
#'
#' @param L A list of matrices.
#' @return The sum of all matrices.
#' @author Raphael Scherrer
#' @export

# Function to add matrices
add_matrices <- function(L) {

  # Security check
  if(!inherits(L, "list")) stop("L must be a list.")
  if(!all(lapply(L, class) == "matrix"))stop("all elements of L must be matrices")
  if(!do.call("all", lapply(L, dim))) stop("all matrices in L must have the same dimensions.")

  # Initialize the summation
  sumL <- L[[1]]

  # Loop through all matrices and add them one by one
  for(i in seq_along(L)[-1]) {
    sumL <- sumL + L[[i]]
  }

  return(sumL)
}
