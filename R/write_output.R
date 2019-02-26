#' Save output
#'
#' Function to write output to text file
#'
#' @param results A list of results.
#' @param whatHabitats What habitats?
#' @author Raphael Scherrer
#' @export

# Function to write output to text file
write_output <- function(results, whatHabitats) {

  headers <- c(
    "\nAnalyses of Variance\n\n",
    "\nMultivariate Analysis of Variance\n\n",
    "\nPermutational Multivariate Analysis of Variance\n\n",
    "\nGeneralize Least Squares models\n\n",
    "\nKruskal-Wallis test\n\n",
    "\nSupport Vector Machine analysis summary\n\n",
    "\nSupport Vector Machine cumulative confusion matrices\n\n",
    "\nShapiro-Wilk's test\n\n",
    "\nMultivariate Shapiro-Wilk's test\n\n",
    "\nBartlett's test\n\n",
    "\nMantel's test\n\n",
    "\nMoran's test\n\n",
    "\nMutliple comparisons of means\n\n",
    "\nNon-parametric multiple comparisons of means\n\n",
    "\nMultiple comparisons of multivariate means\n\n",
    "\nExact locations\n\n"
  )

  headers <- headers[sapply(results, function(x) !is.null(x))]
  results <- results[sapply(results, function(x) !is.null(x))]

  textFile <- paste0(c(NA, "R2", "R3", "R4")[length(whatHabitats)], ".txt")
  textFile <- paste0("results/", textFile)

  # Print to text file
  sink(textFile)
  for(i in 1:length(headers)) {
    curr_header <- headers[[i]]
    curr_result <- results[[i]]
    cat(curr_header)
    print(curr_result)
  }
  sink()

}
