library(hal)
library(testthat)

context("Parallel glmnet")

# Number of covariates to use
d <- 8

# Sample size
n <- 2000

# Simulate some data, all continuous covariates.
set.seed(1)
x = data.frame(matrix(rnorm(n * d), ncol = d))
y = rnorm(n, rowSums(x))

library(doParallel)
library(foreach)

# Use doMC if possible, otherwise doParallel
if (require(doMC)) {
  doMC::registerDoMC()
  cl = NULL
} else {
  cl = parallel::makeCluster()
  registerDoParallel(cl)
}

# Confirm that we are operating in parallel.
foreach::getDoParWorkers()

################################################
# Benchmark parallel hal to normal hal.
# This will take a long time!

mb_result <- microbenchmark::microbenchmark(
  #### 1st version - normal glmnet.
  hal(Y = y, X = x, family = gaussian(), verbose = F, parallel = F),

  #### 2nd version - parallel glmnet.
  hal(Y = y, X = x, family = gaussian(), verbose = F, parallel = T),

  #### Other microbenchmark options.
  times = 10L
)

mb_result
