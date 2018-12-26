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
  library(pbapply)
  library(rminer)

  if(!missing("seed")) set.seed(seed)

  nhabitats <- nlevels(specdata$habitat)

  # Permute nRepet times
  ii <- lapply(seq_len(nRepet), function(it) {
    return(sample(nrow(specdata), replace = F))
  })

  message(paste("Training", nRepet, "networks on permuted data..."))

  # Train neural networks on permuted data
  permuted.res <- pblapply(ii, function(ii) {

    habitats <- specdata$habitat[ii]

    # Assign random habitat to every lizard
    d <- specdata[,vars]
    d <- data.frame(d, habitat = habitats)

    # Prepare training and testing datasets
    inTraining <- caret::createDataPartition(d$habitat, p=0.5, list=F)
    training <- d[inTraining,]
    testing <- d[-inTraining,]

    # Balance training set so there are same numbers of each class
    training <- DescTools::Strata(training, stratanames = "habitat", size = rep(min(table(training$habitat)),length(unique(training$habitat))))
    drop <- c("stratum","size","id")
    training <- training[,-which(colnames(training) %in% drop)]

    # Fit neural network
    svm.model <- fit(habitat~., data=training, model="svm", kernel = "rbfdot", task="class")
    svm.pred <- predict(svm.model, testing[,-which(colnames(testing)=="habitat")])

    # Confusion matrix
    tab <- table(svm.pred, true=testing$habitat)

    # Assess performance
    nSuccess <- sum(diag(tab))
    nTotal <- sum(tab)
    propSuccess <- nSuccess / nTotal

    p.binom <- binom.test(nSuccess, nTotal, p = 1/nhabitats)$p.value

    return(c(propSuccess, p.binom))

  })

  permuted.res <- do.call("rbind", permuted.res)

  message(paste("Training", nRepet, "networks on empirical data..."))

  d <- data.frame(specdata[,c(vars, "habitat")])

  empir <- pblapply(seq_len(nRepet), function(i) {

    # Create training and test sets
    inTraining <- createDataPartition(d$habitat, p=0.5, list=F)
    training <- d[inTraining,]
    testing <- d[-inTraining,]

    # Balance training set so there are same numbers of each class
    training <- DescTools::Strata(training, stratanames = "habitat", size = rep(min(table(training$habitat)),length(unique(training$habitat))))
    drop <- c("stratum","size","id")
    training <- training[,-which(colnames(training) %in% drop)]

    # Fit the machine
    svm.model <- fit(habitat~., data=training, model="svm", kernel = "rbfdot", task="class")
    svm.pred <- predict(svm.model, testing[,-which(colnames(testing)=="habitat")])

    # Confusion matrix
    tab <- table(svm.pred, true=testing$habitat)

    # Assess performance
    nSuccess <- sum(diag(tab))
    nTotal <- sum(tab)
    propSuccess <- nSuccess / nTotal

    p.binom <- binom.test(nSuccess, nTotal, p = 1/nhabitats)$p.value

    out <- list(training = training, confutab = tab, machine = svm.model, success = c(propSuccess, p.binom))

    # (Save training datasets for importance sampling)

    return(out)

  })

  empirical.res <- lapply(empir, function(curr.machine) curr.machine$success)
  empirical.res <- as.matrix(do.call("rbind", empirical.res))

  labels <- factor(c(rep("Randomizations", nrow(permuted.res)), rep("Empirical", nrow(empirical.res))))
  results <- cbind(as.data.frame(rbind(permuted.res, empirical.res)), labels)
  colnames(results) <- c("propSuccess","p.value", "label")

  message("Identifying key discriminating variables...")

  # Identify the best machines
  quant95 <- quantile(results$propSuccess[results$label == "Empirical"], probs = 0.95)
  idBestReps <- empirical.res[,1] >= quant95

  # Subset machines, confusion matrices, and training sets to top 5%
  machines <- lapply(empir, function(curr.machine) curr.machine$machine)
  bestMachines <- machines[which(idBestReps)]
  tabs <- lapply(empir, function(curr.machine) curr.machine$confutab)
  bestTabs <- tabs[which(idBestReps)]
  trainings <- lapply(empir, function(curr.machine) curr.machine$training)
  bestTrainings <- trainings[which(idBestReps)]

  # Get Feature Importance for top 5% machines
  bestFeatures <- pbmapply(Importance, bestMachines, bestTrainings, MoreArgs = list(method="sensv"), SIMPLIFY = FALSE)

  # Use Importance function, need to either save training data (prob faster) or run inside loop (longer)
  importanceTable <- sapply(bestFeatures,"[[","imp")
  if(ncol(cbind(importanceTable)) > 1) importanceTable <- rowSums(importanceTable) else importanceTable <- c(importanceTable)
  names(importanceTable) <- colnames(trainings[[1]])
  importanceTable <- importanceTable[-1] # first value is habitat

  names(importanceTable) <- gsub("meanrefl", "Mean\nreflectance", names(importanceTable))
  names(importanceTable) <- gsub("cuton", "Cut-on\nwavelength", names(importanceTable))

  # Output
  out <- list(results, importanceTable)
  names(out) <- c("Results", "Importance")

  message("Done.")

  return(out)

}
