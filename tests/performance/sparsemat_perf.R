library(hal)

# Available datasets in this package.
datasets = c("cpu", "laheart", "oecdpanel", "pima", "fev")

# Don't loop, just pick a dataset.
dataset_name = datasets[2]

# Read csvs from the extdata folder.
file <- system.file("extdata", paste0(dataset_name, ".csv"), package = "hal")
data = read.csv(file)

# Remove the last row, which is all NA for some reason :/
# TODO: figure out why this is.
data = data[-nrow(data), ]

##########################################
# Run HAL on a single dataset (no cross-validation).

# Compare SparseMat version to non sparseMat version.

result <- microbenchmark::microbenchmark(
  #### 1st version - sparseMat
  hal::hal(Y = data[, 1],
      # Restrict to just the first 5 covariates for testing purposes.
      X = data[, 2:min(1 + 5, ncol(data))],
      #X = data[, 2:ncol(data)],
      family = gaussian(), verbose = F),

  #### 2nd version - no sparseMat
  hal::hal(Y = data[, 1],
      # Restrict to just the first 5 covariates for testing purposes.
      X = data[, 2:min(1 + 5, ncol(data))],
      #X = data[, 2:ncol(data)],
      family = gaussian(), verbose = F, sparseMat = F),

  #### Other microbenchmark options.
  times = 10L
)

result
