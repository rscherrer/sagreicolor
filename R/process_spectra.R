#' Process reflectance spectra
#'
#' This function performs a few steps of processing on the raw reflectance data. These include removing juveniles and females, removing lizards from New Providence, keeping only one dewlap spot of interest, fixing reflectance artifacts, smoothing curves and removing outliers.
#' @param specs An \code{rspec} data frame containing the reflectance profiles of all dewlaps.
#' @param locale A data frame containing locality information for each lizard (island, habitat, plot, longitude and latitude). Optional.
#' @param spot An integer: what spot to analyze? 1 (default) or 2.
#' @return A summary data frame with processed spectra. Wavelengths are in columns, as well as extra details like island and habitats.
#' @author Raphael Scherrer
#' @export

process_spectra <- function(specs, locale = NULL, spot = 1) {

  library(pavo)

  # Convert data frame to rspec if not already
  if(!'rspec' %in% class(specs)) specs <- pavo::as.rspec(specs)

  # Remove juveniles
  juvs <- c(grep('juv', colnames(specs)), grep('JUV', colnames(specs)))
  specs <- specs[,-juvs]

  # Remove spots 3 and 4 on the dewlap
  patch34 <- c(grep('_3_', colnames(specs)), grep('_4_', colnames(specs)))
  specs <- specs[,-patch34]

  # Subset the spot of interest (1 or 2)
  specs <- specs[,c(1,grep(paste0('_', as.character(spot), '_'), colnames(specs)))]

  # Remove females
  fems <- grep('_F_', colnames(specs))
  if(length(fems)>0) specs = specs[,-fems]

  # Remove lizards from New Providence
  nassau <- grep('_Nassau_', colnames(specs))
  if(length(nassau)>0) specs = specs[,-nassau]

  # Linearize the big artifacts at ~620nm and ~550nm
  specs <- sagreicolor::linearize(specs, where=c(540,560))
  specs <- sagreicolor::linearize(specs, where=c(539,561))
  specs <- sagreicolor::linearize(specs, where=c(600,630))
  specs <- sagreicolor::linearize(specs, where=c(599,631))


  # Fix negative values
  specs <- pavo::procspec(specs, fixneg = 'zero')# Smooth the spectra

  # Smooth
  specs <- pavo::procspec(specs, opt='smooth', span=0.5)
  specs <- pavo::procspec(specs, fixneg = 'zero')

  # Remove refl > 100%
  toohigh <- which(apply(specs[,-1], 2, function(x) any(x > 100)))+1
  if(length(toohigh)>0) specs <- specs[,-toohigh]

  # Remove very dark spectra
  toolow <- which(apply(specs[,-1], 2, function(x) mean(x)<1))+1
  if(length(toolow)>0) specs <- specs[,-toolow]

  # Get details from file names
  details <- do.call('rbind', strsplit(colnames(specs)[-1], '_'))
  details <- details[,-1]
  details <- as.data.frame(details)
  colnames(details) <- c('island', 'specimen', 'spot', 'sex', 'habitat')


  # Get locality data and match them to individuals
  if(!is.null(locale)) {
    coords <- t(sapply(as.character(details$specimen), function(currspecimen) {
      locale <- subset(locale, specimen==currspecimen)
      coord <- with(locale, c(latitude, longitude))
      return(coord)
    }))
    rownames(coords) <- NULL
    details <- cbind(details, coords)
    colnames(details)[c(ncol(details)-1, ncol(details))] <- c('latitude', 'longitude')
  }


  # Final data frame
  wlcol <- paste0('wl', specs[,1])
  specdf <- as.data.frame(t(specs[,-1]))
  colnames(specdf) <- wlcol
  specdf <- cbind(specdf, details)

  # Remove outlier (new feature)
  if(spot == 2) {

    outliers <- grep('RGR0928', rownames(specdf))
    specdf <- specdf[-outliers,]

  }

  return(specdf)

}
