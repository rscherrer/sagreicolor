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

  # Make sure all confusion matrices have the same dimensions (some islands don't have the same number of habitats e.g. Conception)
  habitats <- unlist(lapply(sumconfs, colnames)[sapply(sumconfs, ncol) == max(sapply(sumconfs, ncol))][1])
  cols2add <- sapply(sumconfs, function(x) length(habitats) - ncol(x))

  sumconfs <- mapply(function(confmat, cols2add) {

    if(cols2add > 0) {

      confmat <- cbind(confmat, matrix(0, nrow = nrow(confmat), ncol = cols2add))
      confmat <- rbind(confmat, matrix(0, nrow = cols2add, ncol = ncol(confmat)))

      missinghabitats <- habitats[!habitats %in% colnames(confmat)]
      colnames(confmat)[grep("^$", colnames(confmat))] <- missinghabitats
      rownames(confmat)[grep("^$", rownames(confmat))] <- missinghabitats

      neworder <- match(colnames(confmat), habitats)

      confmat <- confmat[,neworder]
      confmat <- confmat[neworder,]

    }

    return(confmat)

  }, sumconfs, cols2add, SIMPLIFY = F)

  return(sumconfs)

}
