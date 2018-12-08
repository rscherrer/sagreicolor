#' Habitat classification with neural networks
#'
#' This function trains neural networks to recognize differences between habitats. Each neural network is trained on a random sample of half of the data, and tested against the other half. The success of the classification is compared to a null expectation generated from a permuted dataset where no differences exist between habitats. The 5% best performing machines are studied more in depth to identify what were the most important variables in discriminating between habitats.
#'
#' @param specdata A data frame containing at least columns for the dependent variables, as well as a column "habitat".
#' @param vars A character or integer vector. The names, or indices, of the dependent variables in \code{specdata}.
#' @param nRepet The number of neural networks to train (same number for empirical and permuted datasets).
#' @param plot_success Whether to plot histograms of the success of the machines on the empirical data compared to the null distribution.
#' @param plot_importance Whether to plot the importance of each variable in discriminating among habitats, according to the top 5% machines.
#' @param seed Seed for random number gnerators
#' @return Just plots.
#' @note If \code{plot_importance = T} and reflectances at certain wavelengths are present in the variables (their name start with "wl" in specdata) then a first linear plot is drawn along the light spectrum, then a barplot is generated with the remaining variables.
#' @author Raphael Scherrer
#' @export

# Function to train neural networks to detect differences between habitats
dewlap_neural_global <- function(specdata, vars, nRepet = 1000, plot_success = T, seed, plot_importance = T) {

  # Load dependencies
  library(sagreicolor)
  library(DescTools)
  library(caret)
  library(pbapply)
  library(rminer)
  library(pbapply)
  library(extrafont)

  loadfonts()

  if(exists("seed")) set.seed(seed)

  nhabitats <- nlevels(specdata$habitat)

  # Permute nRepet times
  ii <- lapply(seq_len(nRepet), function(it) {
    return(sample(nrow(specdata), replace = F))
  })

  message("Training on permuted data...")

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

  message("Training on empirical data...")

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

  # Plot success and random expectation
  if(plot_success) {

    p1 <- ggplot(results, aes(x = propSuccess, fill=label))  + geom_histogram(position="identity", alpha=0.5, bins = 100) + theme_bw() + xlab("Proportion of success") + ylab("Count") + theme(legend.title = element_blank())

    p2 <- ggplot(results, aes(x = p.value, fill=label))  + geom_histogram(position="identity", alpha=0.5, bins = 100 ) + theme_bw() + xlab("Binomial test P-value") + ylab("Count") + theme(legend.title = element_blank())

    ggsave("plots/success_neural_network.pdf", p1, device = "pdf", width = 4, height = 2.5, family = "Garamond")

    ggsave("plots/pvalues_neural_network.pdf", p2, device = "pdf", width = 4, height = 2.5, family = "Garamond")

  }

  message("Evaluating the 5% best machines...")

  # Identify the best machines
  quant95 <- quantile(results$propSuccess[results$label == "Empirical"], probs = 0.95)
  idBestReps <- empirical.res[,1] > quant95

  # Subset machines, confusion matrices, and training sets to top 5%
  machines <- lapply(empir, function(curr.machine) curr.machine$machine)
  bestMachines <- machines[which(idBestReps)]
  tabs <- lapply(empir, function(curr.machine) curr.machine$confutab)
  bestTabs <- tabs[which(idBestReps)]
  trainings <- lapply(empir, function(curr.machine) curr.machine$training)
  bestTrainings <- trainings[which(idBestReps)]

  # Get Feature Importance for top 5% machines
  bestFeatures <- mapply(Importance, bestMachines, bestTrainings, MoreArgs = list(method="sensv"), SIMPLIFY = FALSE)

  # Use Importance function, need to either save training data (prob faster) or run inside loop (longer)
  importanceTable <- rowSums(sapply(bestFeatures,"[[","imp"))
  names(importanceTable) <- colnames(trainings[[1]])
  importanceTable <- importanceTable[-1] # first value is habitat

  if(plot_importance) {

    # First plot along the spectrum of wavelengths
    if(length(grep("wl", names(importanceTable))) != 0) {

      wl_id <- grep("wl", names(importanceTable))
      imp <- importanceTable[wl_id]
      wls <-  as.numeric(gsub("wl", "", names(importanceTable)))
      imp_along_spectrum <- cbind(imp, wls)
      plot(importance, ylab="Importance",xlab="Wavelength", type="l")

      importanceTable <- importanceTable[-wl_id] # remove wavelengths from the importance table

    }

    # Then plot the rest of the variables
    barplot(importanceTable)

  }


}
