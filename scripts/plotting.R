# Plotting

rm(list = ls())

#library(devtools)
devtools::install_github("rscherrer/sagreicolor")

# Load packages
library(sagreicolor)
library(fields)
library(RColorBrewer)
library(pavo)

defaultPar <- par()

#dewlaPCA <- readRDS("data/dewlaPCA.rds")

# Load and process spectra
# This is not done through the organize.R script
# Because here, we want to include Conception Island (which does not have all habitats and must then be excluded from some analyses)
specs <- readRDS("data/specs.rds")
specs <- as.rspec(specs)
spot <- 2
locale <- readRDS("data/locale.rds")
specdata <- sagreicolor::process_spectra(specs, locale, spot)

# What habitats do we want?
whatHabitats1 <- c("coastal", "coppice")
whatHabitats2 <- c("mangrove")

# Reduce to the wanted habitats
specdata1 <- sagreicolor::subset_habitats(specdata, whatHabitats1)
specdata2 <- sagreicolor::subset_habitats(specdata, whatHabitats2)

specdata <- rbind(specdata1, specdata2)

# Load principal components
dewlaPCA <- sagreicolor::spectral_pca(specdata)

specdata <- cbind(specdata, dewlaPCA$x)

# Set a nice looking color palette
palette(RColorBrewer::brewer.pal(n = nlevels(specdata$island), "Set3"))


#### Make PCA biplots ####

# Set groups
groups <- specdata[,c("island", "habitat")]

# Plot PC1, 2 and 3
par(mfrow = c(2,2))
for(i in 1:2) {
  for(j in (i+1):3) {
    sagreicolor::dewlap_biplot(dewlaPCA, groups, i = i, j = j, plotCentroids = F)
  }
}

plot(1,1, type = "n", axes = F, xlab = "", ylab = "")
legend("topleft", legend = c("coppice", "coastal", "mangrove"), pch = c(18, 17, 16), title = "Habitat", bty= "n")
legend("topright", legend = unique(specdata$island), pch = 16, col = seq_along(unique(specdata$island)), title = "Island", bty= "n", ncol = 3)

par(mfrow = c(1,1))

# Same but with centroids only
par(mfrow = c(2,2))
for(i in 1:2) {
  for(j in (i+1):3) {
    sagreicolor::dewlap_biplot(dewlaPCA, groups, i = i, j = j, plotCentroids = T)
  }
}
par(mfrow = c(1,1))

#### Plot PCA eigenvalues ####

plot(dewlaPCA, main = NULL)

cumsum(dewlaPCA$sdev) / sum(dewlaPCA$sdev)
# 91% of the variance is explained with only 4 components!

#### Plot variables against each other ####

specdata$meanrefl <- rowMeans(specdata[,grep("wl", colnames(specdata))])

plot(PC1 ~ meanrefl, data = specdata, pch = 16, xlab = "Mean reflectance", ylab = "PC1")

with(specdata, cor.test(PC1, meanrefl))

#### What wavelengths contribute to what components? ####

# Wavelengths
X <- c('wl300', 'wl350', 'wl400', 'wl450', 'wl500', 'wl550', 'wl600', 'wl650', 'wl700')

# Components
Y <- c('PC2', 'PC3', 'PC4')

# Calculate correlation coefficients and p-values
cortest <- sapply(Y, function(Y) {
  sapply(X, function(X) {
    cortest <- cor.test(specdata[,Y], specdata[,X])
    cortest <- round(cortest$estimate,4)
    return(cortest)
  })
}, simplify=F)

# Rearrange the matrix
cortest <- t(do.call("rbind", cortest))

# Create a color gradient
make_gradient <- colorRampPalette(c("#3288BD", "#FFFFBF", "#D53E4F"))
col <- make_gradient(21)

# Plot correlation heatmap
image.plot(t(cortest), axes=F, col=col)

# Add labels
at = c(0, .5, 1)
mapply(function(Y, at) mtext(Y, 3, .5, at=at), Y, at)
at = seq(0, 1, 1/(length(X)-1))
mapply(function(X, at) mtext(X, 2, .5, at=at, las=1), X, at)


#### Make boxplots ####

do_boxplots(specdata, 'UV', isJitter = T)
do_boxplots(specdata, 'Red', isJitter = T)
do_boxplots(specdata, 'meanrefl', isJitter = T)
do_boxplots(specdata, 'cuton', isJitter = T)
do_boxplots(specdata, 'PC2', isJitter = T)
do_boxplots(specdata, 'PC3', isJitter = T)
do_boxplots(specdata, 'PC4', isJitter = T)
