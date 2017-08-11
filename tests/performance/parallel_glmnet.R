library(hal)

# Number of covariates to use. This will be slow.
d <- 12
#d <- 8

# Sample size
n <- 500

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

# Fit hal with parallel glmnet. Takes ~8 minutes on a 4-core laptop.
hal_fit_par <- hal::hal(Y = y, X = x, family = gaussian(),
  verbose = T, parallel = T, debug = T)

# Review timing - takes 487 seconds (23 for glmnet)
hal_fit_par$times

# Compare to non-parallel version. Takes ~10 minutes.
hal_fit <- hal(Y = y, X = x, family = gaussian(),
                   verbose = T, parallel = F, debug = T)

# Review timing - takes 622 seconds (84 for glmnet).
hal_fit$times

