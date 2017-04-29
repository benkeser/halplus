# sins
figure1_data <- function(n) {
  x1 <- runif(n, -4, 4)
  y <- 2 * sin(pi / 2 * abs(x1)) + rnorm(n)

  return(data.frame(x1 = x1, y = y))
}

doMC::registerDoMC()
foreach::getDoParWorkers()

set.seed(1, "L'Ecuyer-CMRG")
data = figure1_data(500)
model = hal::hal(data$y, data[, "x1", drop = F],
                 newX = data[, "x1", drop = F],
                 nfolds = 20,
                 parallel = T,
                 useMin = F,
                 verbose = T)
#pred = predict(hal, data[, "x1", drop = F])

model$fit$object$lambda.min
model$fit$object$lambda.1se
model$fit$object$nzero[which.min(model$fit$object$cvm)]
# Try to extract which observations had non-zero coefficients from lasso.
betas = model$fit$object$glmnet.fit$beta[, which.min(model$fit$object$cvm)]
length(which(betas != 0))

library(ggplot2)
ggplot(data, aes(x = x1)) + theme_minimal() +
  #geom_rug(aes(x = data$x1, y = betas != 0)) +
  geom_point(aes(y = y), alpha = 0.15) +
  geom_step(aes(y = as.numeric(model$pred)))
