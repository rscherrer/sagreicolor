#' Smooth reflectance artifacts
#'
#' This function removes artifactual peaks in reflectance profiles by applying linear smoothing on a portion of the profile.
#'
#' @param specs An \code{rspec} data frame (pavo).
#' @param where A numeric vector of two numbers defining between what and what wavelength is the smoothing to be applied (e.g. \code{where = c(620, 630)} for smoothing between 620 and 630nm).
#' @param cols A vector of column numbers, indicating what measurements to apply the smoothing to. Defaults to all. Note: first measurement is column 2.
#' @return An \code{rspec} data frame with artifacts removed.
#' @author Raphael Scherrer
#' @export

linearize = function(specs, where, cols=2:ncol(specs)) {
  x1 = which(specs$wl==where[1])
  x2 = which(specs$wl==where[2])
  portion.specs = specs[x1:x2,c(1,cols)]
  portion.specs = apply(portion.specs, 2, function(spec, wl) {
    loess.smooth(x=wl,
                 y=as.data.frame(spec),
                 span=50,
                 degree=1,
                 family='gaussian',
                 evaluation=length(wl))$y
  }, portion.specs[,1])
  specs[x1:x2,cols] = portion.specs[,-1]
  return(specs)
}
