#' Show SVM parameters
#'
#' Function to display the parameters of the Support Vector Machine analysis
#'
#' @param svm.res The output of \code{save_svm_results}.
#' @return A table of SVM parameters.
#' @author Raphael Scherrer
#' @export

# Function to show parameters of the SVM analyses
show_svm_params <- function(svm.res) {

  # Table with parameters of each analysis
  trainsizes <- sapply(svm.res, function(x) mean(x$res$trainingSize))
  quants <- sapply(svm.res, function(x) x$quant95)
  mean_sigmas <- sapply(svm.res, function(x) mean(x$res$sigma))
  sderr_sigmas <- sapply(svm.res, function(x) sqrt(var(x$res$sigma) / length(x$res$sigma)))
  mean_success <- sapply(svm.res, function(x) mean(x$res$propSuccess))
  svm_summary <- data.frame(trainSize = trainsizes, quant95 = quants, sigma = mean_sigmas, sderr_sigma = sderr_sigmas, cost = 1, mean_success = mean_success)
  return(svm_summary)

}
