#' Add spaces between words in labels
#'
#' This function adds spaces between words to create clean labels for plotting.
#'
#' @param inames A vector of character strings; the names to apply the function to.
#' @return A vector of character strings, with spaces.
#' @author Raphael Scherrer
#' @export

add_spaces = function(inames) {
  library(stringr)
  uppers = str_locate_all(inames, '[A-Z]')
  w = sapply(uppers, nrow)==2
  pos = sapply(uppers[w], function(x) x[2,1])
  inames[w] = mapply(function(x,y) {
    paste(substr(x, 1, y-1), substr(x, y, nchar(x)))
  }, inames[w], pos)
  return(inames)
}
