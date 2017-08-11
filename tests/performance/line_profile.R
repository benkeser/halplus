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

library(lineprof)

##########################################
# Run HAL once on a single dataset (no cross-validation).
# SparseMat version.

# Line profiling code from:
# http://adv-r.had.co.nz/Profiling.html#measure-perf
# To measure execution time and memory usage.

# Takes 2 minutes to run.
# This system.time() will include extra time due to lineprof()
# plyr::alply on makeSparseMat.R line 95 seems to be the major memory and time user.
system.time({
  prof_result = lineprof::lineprof({
  result <- hal::hal(Y = data[, 1],
    # Restrict to just the first 7 covariates for testing purposes.
    #X = data[, 2:min(1 + 7, ncol(data))],
    X = data[, 2:ncol(data)],
    family = gaussian(), verbose = T)
  })
})

# Review timing by section.
# Sparse matrix: 50%
# Lasso: 30%
# Find duplicates: 20%
result$times
# Percentages:
result$times[, "elapsed"] / result$times["everything", "elapsed"]

# Review profiling result.
prof_result
str(prof_result)

# Show rows with the top 100 times.
if (F) {
  # CK: this stuff is not working - may not be possible due to structure of data.
  top = order(prof_result$time, decreasing = T)[1:100]
  names(prof_result)
  str(prof_result)
  prof = prof_result[top, ]
  prof
  prof_result$time[top]
  prof_result$ref[[top]]
  str(prof_result$ref)
}

if (F) {
  # Review in a cool interactive explorer.
  shine(prof_result)
}

##########################################
# Run HAL once on a single dataset (no cross-validation).
# non-sparse Mat version.

prof_result2 = lineprof::lineprof({
  result <- hal::hal(Y = data[, 1],
    # Restrict to just the first 7 covariates for testing purposes.
    X = data[, 2:min(1 + 7, ncol(data))],
    #X = data[, 2:ncol(data)],
    family = gaussian(), verbose = T, sparseMat = F)
})

# Review profiling result.
prof_result2

if (F) {
  # Review in a cool interactive explorer.
  shine(prof_result2)
}


##########################################
# Run HAL as part of SuperLearner.

# Skip this for now.
if (F) {

  # Define the SuperLearner library
  SL.library <- c("SL.hal", "SL.glm", "SL.glmnet")

  # fit super learner
  # each data set is arranged so that the outcome is in the first column
  # and the rest of the variables are in the remaining columns
  set.seed(1568)
  prof_result = lineprof::lineprof({
    result <- SuperLearner::SuperLearner(Y = data[, 1],
      # Restrict to just the first 3 covariates for testing purposes.
      X = data[, 2:min(4, ncol(data))],
      #X = data[, 2:ncol(data)],
      #V=10,
      family = gaussian(), SL.library = SL.library
    )})

  prof_result
}

