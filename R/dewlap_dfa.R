#' Discriminant Function Analysis (DFA) of dewlap color
#'
#' This function performs multiple DFA on each island separately. The algorithm fits functions that best discriminate among habitats based on dewlap color data. The type of DFA can be linear (LDA) or quadratic (QDA). Each data point is then classified i.e. its original habitat is predicted based on the discriminant functions. Predictions can be jacknifed (leave-one-out). The significance of the classification is assessed in two ways. First, a MANOVA tests for differences in dependent variables across predicted groups. Second, a binomial test assesses the departure of the observed number of successful predictions from the null expectation.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "island" and a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param type A character. Type of discriminant analysis. \code{"linear"} (default) or \code{"quadratic"}.
#' @param plotit Logical. Whether to plot the loadings of the data points on the discriminant functions (applicable only if \code{type == "linear"}). Also, whether to plot adjusted P-values, across islands, of the MANOVAs and binomial tests.
#' @param CV Logical. Whether to jacknife the predictions (cross-validation).
#' @param method A character. The method to use for P-value correction.
#' @return A data frame with columns:
#' \itemize{
#' \item "observed", "expected", "n", "p.binom", "df", "wilks", "approx.F", "num.df", "denom.df", "p.manova"
#' \item \code{observed}: the observed number of succesful assignments
#' \item \code{expected}: the number of succesful assignments expected by change
#' \item \code{n}: the number of assignments
#' \item \code{p.binom}: the adjusted P-value of the binomial test
#' \item \code{df}: the degrees of freedom of the MANOVA
#' \item \code{wilks}: Wilk's lambda
#' \item \code{approx.F}: the approximate F-value calculated from Wilk's lambda
#' \item \code{num.df}: numerator degrees of freedom for F-test
#' \item \code{denom.df}: denumerator degrees of freedom for F-test
#' \item \code{p.manova}: adjusted P-value of the MANOVA
#' }
#' @note Quadratic discriminant analysis doesn't require homogeneous covariance matrices among groups, unlike linear (Robert I. Kabacoff, Quick-R, https://www.statmethods.net/advstats/discriminant.html).
#' @author Raphael Scherrer
#' @export

# Function to perform discriminant function analysis
dewlap_dfa <- function(specdata, vars, type = "linear", plotit = T, CV = F, method = "bonferroni") {
  
  library(MASS)
  
  # For each island...
  res <- sapply(levels(specdata$island), function(curr.island) {
    
    print(curr.island)
    
    # Subset of the data
    specdata <- droplevels(subset(specdata, island == curr.island))
    
    # Extract dependent variables
    Y <- as.matrix(specdata[, vars])
    
    # Perform discriminant analysis (linear or quadratic)
    # Note: quadratic analysis doesn't require homogeneous covariance matrices among groups unlike linear (Robert I. Kabacoff, Quick-R, https://www.statmethods.net/advstats/discriminant.html)
    if(type == "linear") {
      
      dfa.fit <- MASS::lda(habitat ~ Y, data = specdata, CV = CV)
      
    } else if(type == "quadratic") {
      
      dfa.fit <- MASS::qda(habitat ~ Y, data = specdata, CV = CV)
      
    }
    
    # Plot the discriminant functions (only if LDA)
    if(type == "linear" & plotit) plot(dfa.fit, main = curr.island)
    
    # Predictions
    pred <- predict(dfa.fit, as.data.frame(Y))
    
    # Test for differences among predicted groups
    MANOVA <- manova(Y ~ pred$class)
    MANOVA.res <- summary(MANOVA, test = "Wilks")
    print("MANOVA table")
    print(MANOVA.res)
    
    # Classification matrix (cross-tabulation)
    classMat <- table(specdata$habitat, pred$class)
    print("Classification matrix")
    print(classMat)
    
    # How many successful assignments?
    nSuccess <- sum(diag(classMat))
    
    # How many assignments in total?
    nTotal <- sum(classMat)
    
    # Probability of a correct reassignment by chance
    prob <- 1 / ncol(classMat)
    
    # Is the number of success higher than expected by chance?
    binom.res <- binom.test(nSuccess, nTotal, prob, alternative = "greater")
    
    # Output
    out <- with(binom.res, c(null.value, estimate, parameter, p.value))
    
    # Add MANOVA output
    out <- c(out, c(MANOVA.res$stats[1,]))
    
    return(out)
    
  })
  
  # Reorganize the result matrix
  res <- t(res)
  res <- as.data.frame(res)
  colnames(res) <- c("observed", "expected", "n", "p.binom", "df", "wilks", "approx.F", "num.df", "denom.df", "p.manova")
  
  # Adjust p-values
  res$padj.binom <- p.adjust(res$p.binom, method = method)
  res$padj.manova <- p.adjust(res$p.manova, method = method)
  
  res <- res[,c(1,2,3,4,11,5,6,7,8,9,10,12)]
  
  # Plot adjusted p-values
  if(plotit) {
    sapply(c("padj.binom", "padj.manova"), function(curr.p) {
      barplot(res[, curr.p], ylim = c(0,1), main = paste(curr.p, type))
      abline(h = 0.05, lty = 2)
    })
  }
  
  return(res)
  
}