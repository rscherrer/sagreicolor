#' Save SVM output
#'
#' Function to save the results of the Support Vector Machine analysis
#'
#' @param neural.res The output of \code{dewlap_neural}.
#' @param nested.res The output of \code{dewlap_neural_nested}.
#' @param saveto Where to save the output?
#' @return A list of SVM outputs.
#' @author Raphael Scherrer
#' @export


# Function to save the results of the Support Vector Machine analysis
save_svm_results <- function(neural.res, nested.res, saveto = '.') {

  # Save the output
  svm.res <- nested.res
  svm.res[[length(svm.res) + 1]] <- neural.res
  names(svm.res) <- c(levels(specdata$island), "Archipelago")
  saveRDS(svm.res, paste(saveto, "svm_results.rds", sep = "/"))
  return(svm.res)

}
