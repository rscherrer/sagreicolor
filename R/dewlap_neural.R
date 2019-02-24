#' Habitat classification with neural networks
#'
#' This function trains neural networks to recognize differences between habitats. Each neural network is trained on a random sample of half of the data, and tested against the other half. The success of the classification is compared to a null expectation generated from a permuted dataset where no differences exist between habitats. The 5\% best performing machines are studied more in depth to identify what were the most important variables in discriminating between habitats.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param seed Seed for random number gnerators
#' @return A list of two data frames: one contains success and p-values for randomized and empirical data, the other contains the importance variables in the 5\% best classifiers.
#' @author Raphael Scherrer
#' @export

# Function to train neural networks to detect differences between habitats
dewlap_neural <- function(specdata, vars, nRepet = 1000, seed) {

  # Load dependencies
  library(DescTools)
  library(caret)
  library(rminer)

  if(!missing("seed")) set.seed(seed)

  nhabitats <- nlevels(specdata$habitat)

  # Initialization
  res <- matrix(0, nrow = 2 * nRepet, ncol = 6)
  labels <- rep(c("Randomized", "Empirical"), each = nRepet)
  confumats <- vector("list", nRepet)
  machines <- vector("list", nRepet)
  trainings <- vector("list", nRepet)

  # Loop through machines
  for(i in 1:(2 * nRepet)) {

    # Permute the data if within the first half of the loop
    if(i <= nRepet) {

      permid <- sample(nrow(specdata), replace = F)
      permhabitats <- specdata$habitat[permid]
      currdata <- data.frame(specdata[,vars], habitat = permhabitats)

    } else {

      currdata <- data.frame(specdata[,vars], habitat = specdata$habitat)

    }

    message(paste0("Training machine ", i, "/", 2 * nRepet, "..."))

    # Sample training and testing sets
    istraining <- caret::createDataPartition(currdata$habitat, p = 0.5, list = F)
    trainingset <- currdata[istraining,]
    testingset <- currdata[-istraining,]

    # Downsample the training set to ensure balance
    trainingset <- DescTools::Strata(trainingset, stratanames = "habitat", size = rep(min(table(trainingset$habitat)), length(unique(trainingset$habitat))))
    drop <- c("stratum","size","id")
    trainingset <- trainingset[,-which(colnames(trainingset) %in% drop)]

    # Record size of the training set
    trainsize <- nrow(trainingset)

    # Train the Vector Support Machine
    svm.model <- fit(habitat ~ ., data = trainingset, model = "svm", kernel = "rbfdot", task = "class")

    # How many support vectors?
    nSupportVectors <- svm.model@object@nSV

    # Parameters of the machine
    sigma <- svm.model@mpar$kpar$sigma
    cost <- svm.model@mpar$C

    # Test the machine
    svm.pred <- predict(svm.model, testingset[, -which(colnames(testingset) == "habitat")])

    # Confusion matrix
    confusion <- table(svm.pred, true = testingset$habitat)

    # Assess performance
    nSuccess <- sum(diag(confusion))
    nTotal <- sum(confusion)
    propSuccess <- nSuccess / nTotal

    # Test the performance against pure chance
    p.binom <- binom.test(nSuccess, nTotal, p = 1/nhabitats)$p.value

    # Write output
    res[i,1] <- propSuccess
    res[i,2] <- p.binom
    res[i,3] <- trainsize
    res[i,4] <- nSupportVectors
    res[i,5] <- sigma
    res[i,6] <- cost

    if(i > nRepet) {
      confumats[[i - nRepet]] <- confusion
      machines[[i - nRepet]] <- svm.model
      trainings[[i - nRepet]] <- trainingset
    }

  }

  res <- data.frame(labels, res)
  colnames(res) <- c("label", "propSuccess", "pBinom", "trainingSize", "nVectors", "sigma", "cost")

  message("Analyzing the best machines...")

  # Identify the best machines
  quant95 <- quantile(res$propSuccess[res$label == "Empirical"], probs = 0.95)
  bestid <-res$propSuccess[res$label == "Empirical"] >= quant95

  bestmachines <- machines[bestid]
  bestconfumats <- confumats[bestid]
  besttrainings <- trainings[bestid]

  # Get Feature Importance for top 5% machines
  bestFeatures <- mapply(Importance, bestmachines, besttrainings, MoreArgs = list(method="sensv"), SIMPLIFY = FALSE)
  importanceTable <- sapply(bestFeatures,"[[","imp")
  if(ncol(cbind(importanceTable)) > 1) importanceTable <- rowSums(importanceTable) else importanceTable <- c(importanceTable)
  names(importanceTable) <- colnames(trainings[[1]])
  importanceTable <- importanceTable[-1] # first value is habitat

  names(importanceTable) <- gsub("meanrefl", "Mean\nreflectance", names(importanceTable))
  names(importanceTable) <- gsub("cuton", "Cut-on\nwavelength", names(importanceTable))

  out <- list(res, importanceTable, bestconfumats, quant95)

  message("Done.")

  return(out)

}
