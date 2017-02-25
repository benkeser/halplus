library(hal)
library(testthat)

# NOTE: this is very slow, so we don't want testthat() to run it automatically.
# Hence why the filename does not begin with "test_".

context("Dataset test")

# define the SuperLearner library
SL.library <- c("SL.hal", "SL.glm", "SL.glmnet")

# number of covariates to use
d <- 10

#====================================================
# Compute Hal for each of the data sets
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

  # just call hal
  # note that this implementation doesn't time the prediction
  # because there's no newX specified.
  outList[[count]] <- hal(
      Y = data[, 1],
      # Restrict to d covariates for testing purposes.
      X = data[, 2:min(d + 1, ncol(data))],
      family= gaussian(),
      verbose = TRUE
  )

}