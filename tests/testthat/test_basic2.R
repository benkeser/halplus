library(hal)
library(testthat)

# TODO: write basic test of HAL functionality.
context("Basic test")

# number of covariates to use
d <- 10

# sample size
n <- 2000

# simulate some data
# all continuous covariates
eval(parse(text=paste0(
    "x <- data.frame(", 
    paste0("x",1:d,"=rnorm(n)",collapse=","),
    ")"
)))

y <- eval(parse(text=paste0(
    "rnorm(n,",paste0("x",1:d,collapse="+"),")" 
)))

# fit hal
hal.fit <- hal(
    Y = y,
    # Restrict to d covariates for testing purposes.
    X = x,
    family= gaussian(),
    verbose = TRUE
)
