# Statistical analysis

# Workflow:
# 1. Perform MANOVA
# 2. Check assumptions and residuals
# 3. Perform non-parametric MANOVA
# 4. Perform univariate linear models for each dependent variable
# 5. Multiple post hoc comparison of means
# 6. Discriminant Function Analyses
# 7. Kruskal Wallis on each island: full non-parametric
# 8. Neural network analysis
# 9. Write outputs into text file
# Notes

# Empty environment
rm(list = ls())

reinstall <- T
if(reinstall) {
  library(devtools)
  install_github("rscherrer/sagreicolor", force = T)
}



set.seed(42)

# Load packages
library(sagreicolor)



# Load the data
specdata <- read.csv("data/specdata.csv")

# For a proper analysis, subset the dataset to avoid empty-cells in our two-way design

# Choose habitats
whatHabitats <- c("coastal", "coppice", "mangrove")

# Keep only the habitats in the list to avoid empty cells
specdata <- subset_habitats(specdata, whatHabitats)

# Select the dependent variables to analyze (here principal components)
vars <- c("PC1", "PC2", "PC3", "PC4")

#### 1. MANOVA ####

# Perform MANOVA on color space
manova.res <- dewlap_manova(specdata, vars, perVariable = F)
manova.res

#### 2. Check assumptions ####

# Now check the residuals to see if our analysis is valid.

# Check multivariate normality in PC space across groups
mshapiro.res <- check_multinorm(specdata, vars)
mshapiro.res

# In many cases the multivariate normality assumption is violated

# Linearity
# Not sure how to test that

# Homogeneity of variances across each group for each dependent variable
bartlett.res <- check_variances(specdata, vars)
bartlett.res

# Dependent variables are largely heteroscedastic

# Homogeneity of covariance matrices
# According to Robert I Kabacoff (https://www.statmethods.net/stats/anovaAssumptions.html) Box's M test should be used, but it is very sensitive to violations of normality, which is the case here! So it will most likely reject H0.

# Outliers (MANOVA is very sensitive to outliers)
# There are outliers in some habitats. Look at the jittered version of the boxplots to have an idea. That may bias the results of the MANOVA.

# Check normality of each variable
shapiro.res <- check_normality(specdata, vars, method = "bonferroni")
shapiro.res

# Normality of each dependent variable is not so bad

# Look at the residuals of each dependent variable
residuals <- plot_residuals(specdata, vars)

# Heteroscedasticity seems to be the biggest problem

# Check spatial autocorrelation of the residuals
mantel.res <- check_spatialcorr(specdata, vars, test = "mantel")
mantel.res
moran.res <- check_spatialcorr(specdata, vars, test = "moran", model = "gls")
moran.res

# PC2 significant, there may be some spatial autocorrelation for this variable.

#### 3. Non-parametric MANOVA (PERMANOVA) ####

# Non-parametric MANOVA (takes a little while, it's a permutation test)
permanova.res <- dewlap_permanova(specdata, vars)
permanova.res

#### 4. Analyze each dependent variable ####

# Perform multiple two-way ANOVA
anova.res <- dewlap_anova(specdata, vars)
anova.res

# Fit a linear model with generalized least squares (Pinheiro and Bates 2000).
gls.res <- dewlap_gls(specdata, vars)
gls.res

#### 5. Multiple comparisons of means ####

# Create a matrix of contrast weights between habitats on each island
W <- matrix_contrasts(specdata)

# Test contrasts between multivariate means
contrasts.multi <- test_multiContrasts(W, specdata, vars)
contrasts.multi

# Test contrasts for each dependent variable (parametric)
contrasts.uni <- test_contrasts(W, specdata, vars, parametric = T, plotit = T)
contrasts.uni

# Same but non-parametric
contrasts.uni.np <- test_contrasts(W, specdata, vars, parametric = F, plotit = T)
contrasts.uni.np

#### 6. Discriminant function analysis ####

# Discriminant function analysis
# If we find the combinations of variables that maximize the differences between habitats, can we discriminate among habitats?

par(mfrow = c(2,2))
lda.res <- dewlap_dfa(specdata, vars, type = "linear")
qda.res <- dewlap_dfa(specdata, vars, type = "quadratic")
par(mfrow = c(1,1))

lda.res
qda.res

# Global

qdaGlobal.res <- dewlap_dfa_global(specdata, vars, type = "quadratic")

#### 7. Kruskal-Wallis ####

# Perform KW test on each island, correct for multiple testing
kruskal.res <- dewlap_kruskal(specdata, vars)
kruskal.res

#### 8. Vector Support Machine ####

# Train neural networks to recognize differences between habitats, globally and on a per island basis
# The output should contain information about most important cues to distinguish among habitats
vars <- colnames(specdata)[grep("wl", colnames(specdata))]
vars <- c(vars, c("meanrefl", "cuton"))

neural.res <- dewlap_neural(specdata, vars, nRepet = 1000, seed = 42)
saveRDS(neural.res, "tmp/neuralRes.rds")

# Plot the results
plot_neural_success(neural.res[[1]])
plot_neural_pvalues(neural.res[[1]])
plot_neural_spectrum(neural.res[[2]])
plot_neural_importance(neural.res[[2]])

# Now nested within each island
neural.nested.res <- dewlap_neural_nested(specdata, vars, nRepet = 1000, seed = 42)
saveRDS(neural.nested.res, "tmp/neuralRes_nested.rds")

# Plot the results
lapply(neural.nested.res, function(neural.res) {
  plot_neural_success(neural.res[[1]])
  plot_neural_pvalues(neural.res[[1]])
  plot_neural_spectrum(neural.res[[2]])
  plot_neural_importance(neural.res[[2]])
})

# 26/12/18: save to specifif folder for each island and add island name somewhere on the plots

#### 9. Write to text file ####

textFile <- paste0(c(NA, "R2", "R3", "R4")[length(whatHabitats)], ".txt")
textFile <- paste0("output/results/", textFile)

sink(textFile)

# Print tests of effects

cat("\nMultivariate Analysis of Variance\n\n")
print(manova.res)

cat("\nAnalyses of Variance\n\n")
print(anova.res)

cat("\nPermutational Multivariate Analysis of Variance\n\n")
print(permanova.res)

cat("\nGeneralize Least Squares models\n\n")
print(gls.res)

cat("\nKruskal-Wallis test\n\n")
print(kruskal.res)

cat("\nLinear Discriminant Analyses\n\n")
print(lda.res)

cat("\nQuadratic Discriminant Analyses\n\n")
print(qda.res)

cat("\nGlobal Quadratic Discriminant Analysis\n\n")
print(qdaGlobal.res)

# Print contrasts

cat("\nMultiple comparisons of multivariate means\n\n")
print(contrasts.multi)

cat("\nMutliple comparisons of means\n\n")
print(contrasts.uni)

cat("\nNon-parametric multiple comparisons of means\n\n")
print(contrasts.uni.np)

# Print assumption checking

cat("\nMultivariate Shapiro-Wilk's test\n\n")
print(mshapiro.res)

cat("\nShapiro-Wilk's test\n\n")
print(shapiro.res)

cat("\nBartlett's test\n\n")
print(bartlett.res)

cat("\nMantel's test\n\n")
print(mantel.res)

cat("\nMoran's test\n\n")
print(moran.res)

sink()

#### Notes ####

# 1. About MANOVA

# Impossible to performe type III MANOVA. According to Ron Wehrens (Chemometrics with R book), p. 148, singular system arises when there are more independent variables than dependent variables. Type III (partial) MANOVA (Manova function in the car package) is impossible to conduct with so many islands and habitats; it needs a reduced number of combinations to perform OK.

# The procedure I am following (reducing the dimensionality with principal component analysis before using multivariate analysis of variance on principal components) is exactly the workflow of the package micompr. However, it's a parametric package and my data are violating common assumptions of the MANOVA...

# The package npmv seems able to run a non-parametric MANOVA that is able to detect on what dimension the effects are significant. It uses either permutation or approximation of F depending on the sample size. But it doesn't deal with missing data, so I must convert to a one-way design and the p-value is not very informative. The package may be useful if I can do non-parametric multiple comparisons with it, but apparently I cannot.

# The best solution seems to be performing multiple ANOVAs on each principal component (in addition to the PERMANOVA of course), because by using GLS-type of fitting on univariate linear model. I could perform a non-parametric Kruskall-Wallis test but it would be less powerful.

# There are less tools to accomodate violations to assumptions in MANOVA than in ANOVA, that's why I must go for PERMANOVA (a non-parametric test) if I want to keep analyzing multivariate data. If I split the principal components and analyze them separately, though, I can afford using parametric tools by using procedures to correct for violoations of the assumptions. Of course, I can also go for non-parametric equivalents of ANOVA such as Kruskall-Wallis test.

# A problem is the non-independence of the residuals, since plots may be spatially autocorrelated, and that populations on islands are phylogenetically not-independent. Do a phylogenetic analysis?

# 2. Multiple comparisons

# Using confidence intervals of the parameters is feasible, but CI, as P-values, should be adjusted for multiple testing (Ludbrook 2000 Clin Exp Pharmacol Physiol)

# Discriminant function analysis

# Quadratic discriminant analysis does not make the assumption of equal covariance matrices unlike linear. Since we have unbalanced sampling, our covariance matrices are very likely to be heterogenous across habitats. Box's M test would test that but again, it is very sensitive to non-normality, case in which we are.
