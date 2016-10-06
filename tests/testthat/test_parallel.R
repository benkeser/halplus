library(hal)
library(testthat)

context("Parallel glmnet")

# Number of covariates to use
d <- 3

# Sample size
n <- 100

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

# Fit hal with parallel glmnet.
hal_fit_par <- hal(Y = y, X = x, family = gaussian(),
  verbose = T, parallel = T, debug = T
)

# Review timing
hal_fit_par$times
