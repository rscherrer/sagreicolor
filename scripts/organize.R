# Organize

rm(list = ls())

library(devtools)
devtools::install_github("rscherrer/sagreicolor")

library(pavo)
library(sagreicolor)

# Read spectral data
specs <- readRDS("data/specs.rds")

# Read locality data
locale <- readRDS("data/locale.rds")

# What habitats do we want?
whatHabitats <- c("coastal", "coppice", "mangrove")

# Choose a spot to study
spot <- 2

# Process the spectra into a data frame
specdata <- sagreicolor::process_spectra(specs, locale, spot)

# Reduce to the wanted habitats
specdata <- sagreicolor::subset_habitats(specdata, whatHabitats)

# Remove missing entries from spectral dataset
specs <- sagreicolor::df2rspec(specdata)

# Calculate mean reflectance
specdata$meanrefl <- unlist(summary(specs, 'B2'))

# Calculate cut-on wavelength
specdata$cuton <- unlist(summary(specs, 'H3', wlmin = 450, wlmax = 700))

# Calculate red and UV reflectance
specdata$Red <- unlist(summary(specs, 'S1R'))
specdata$UV <- unlist(summary(specs, 'S1U'))

# Compute Principal Components
dewlaPCA <- sagreicolor::spectral_pca(specdata)

# Save the prcomp object
saveRDS(dewlaPCA, "data/dewlaPCA.rds")

# Bind Principal Components to the data frame
specdata <- cbind(specdata, dewlaPCA$x)

# Save the data
write.csv(specdata, "data/specdata.csv")
