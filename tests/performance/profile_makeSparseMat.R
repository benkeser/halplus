library(hal)
library(testthat)

context("makeSparseMat")

# Number of covariates to use.
d <- 8

# Sample size
n <- 500

# Simulate some data, all continuous covariates.
set.seed(1)
x = data.frame(matrix(rnorm(n * d), ncol = d))

library(profvis)
system.time({
  prof_result = profvis::profvis({
    sparse_mat = hal::makeSparseMat(x)
  })
})
str(sparse_mat)

# Review profiling results (need a wide console window).
prof_result

# Review in shiny (run manually).
if (F) {
  shine(prof_result)
}

# Convert to a normal matrix.
normal_mat = as.matrix(sparse_mat)
str(normal_mat)

# Compare object sizes.
library(pryr)
object_size(sparse_mat)
object_size(normal_mat)

# We use about 1/3 of the memory due to the sparse matrix.
as.numeric(object_size(sparse_mat)) / as.numeric(object_size(normal_mat))
