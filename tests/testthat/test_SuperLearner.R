library(hal)
library(testthat)
library(SuperLearner)

context("SuperLearner")

# Number of covariates to use
d <- 3

# Sample size
n <- 60

# Simulate some data, all continuous covariates.
set.seed(1)
x = data.frame(matrix(rnorm(n * d), ncol = d))
y = rnorm(n, rowSums(x))

sl = SuperLearner(y, x, SL.library = c("SL.mean", "SL.hal", "SL.glm"),
                  family = gaussian())
print(sl)