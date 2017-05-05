library(hal)
library(testthat)

# NOTE: this is very slow, so we don't want testthat() to run it automatically.
# Hence why the filename does not begin with "test_".

context("All datasets")

# Source other SL functions
source("tests/Simulation/otherSLFunctions.R")

# define the SuperLearner library
SL.library <- c("SL.hal", "SL.glm", "SL.glmnet")

#====================================================
# Compute CV.SuperLearner for each of the data sets
#====================================================
datasets = c("cpu","laheart","oecdpanel","pima","fev")
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
  set.seed(1568)
  outList[[count]] <- SuperLearner::CV.SuperLearner(
    Y = data[, 1],
    X = data[, 2:ncol(data)],
    V = 10,
    family = gaussian(),
    SL.library = SL.library
  )
}