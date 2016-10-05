library(hal)
library(testthat)

# TODO: write basic test of HAL functionality.
context("Basic test")

# define the SuperLearner library
SL.library <- c("SL.hal", "SL.glm", "SL.glmnet")

# number of covariates to use
d <- 10

#====================================================
# Compute SuperLearner for each of the data sets
#====================================================
#datasets = c("cpu","laheart","oecdpanel","pima","fev")
datasets = c("laheart")
outList <- vector(mode = "list", length = length(datasets))
count <- 0
for (dataset_name in datasets) {
  count <- count + 1

  # Read csvs from the extdata folder.
  file <- system.file("extdata", paste0(dataset_name, ".csv"), package = "hal")
  data = read.csv(file)

  # Remove the last row, which is all NA for some reason :/
  # TODO: figure out why this is.
  data = data[-nrow(data), ]

  # fit cross-validated super learner
  # each data set is arranged so that the outcome is in the first column
  # and the rest of the variables are in the remaining columns
  # set.seed(1568)
  # outList[[count]] <- SuperLearner::SuperLearner(
  #   Y = data[, 1],
  #   # Restrict to just the first 3 covariates for testing purposes.
  #   X = data[, 2:min(d+1, ncol(data))],
  #   #X = data[, 2:ncol(data)],
  #   #V=10,
  #   family = gaussian(),
  #   SL.library = SL.library
  # )
  
  # just call hal
  # note that this implementation doesn't time the prediction
  # because there's no newX specified. 
  outList[[count]] <- hal(
      Y = data[, 1],
      # Restrict to d covariates for testing purposes.
      X = data[, 2:min(d+1, ncol(data))],
      family= gaussian(),
      verbose = TRUE
  )
  
}