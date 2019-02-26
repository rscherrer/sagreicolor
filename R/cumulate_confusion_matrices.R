#' Cumulative confusion matrices
#'
#' Function to compute the cumulative confusion matrices of the Support Vector Machine analysis
#'
#' @param svm.res The output of \code{save_svm_results}.
#' @return A list of cumulative SVM confusion matrices
#' @author Raphael Scherrer
#' @export

# Function to compute cumulated confusion matrices
cumulate_confusion_matrices <- function(svm.res) {

  # Cumulated confusion matrices
  sumconfs <- lapply(svm.res, function(x) {

    confmats <- lapply(x$conf, function(confmat) matrix(confmat, ncol = ncol(confmat)))
    sumconf <- sagreicolor::add_matrices(confmats)
    colnames(sumconf) <- rownames(sumconf) <- colnames(x$conf[[1]])
    return(sumconf)

  })

}
