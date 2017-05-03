#' Highly Adaptive LASSO
#'
#' SuperLearner wrapper for the highly adaptive LASSO
#'
#' @param Y outcome
#' @param X data
#' @param newX New data to apply the model fit and generate predictions.
#' @param family Statistical family
#' @param verbose Set to T for more detailed output
#' @param obsWeights observation weights
#' @param sparseMat Use sparse matrix implementation or normal matrix implementation.
#' @param nfolds Number of CV folds for cv.glmnet
#' @param nlambda Number of lambda values to test in cv.glmnet.
#' @param useMin Glmnet option - use minimum risk lambda or 1se lambda (more
#'   penalization).
#' @param ... Any other arguments to pass-through to \code{hal}
#'
#' @export
SL.hal <- function(Y,
                   X,
                   newX,
                   family = gaussian(),
                   verbose = TRUE,
                   obsWeights = rep(1, length(Y)),
                   sparseMat = TRUE,
                   nfolds = ifelse(length(Y) <= 100, 20, 10),
                   nlambda = 100,
                   minVars = NULL,
                   maxDim = 20,
                   useMin = TRUE,
                   ...) {
  hal(Y = Y, X = X, newX = newX, family = family, verbose = verbose,
      obsWeights = obsWeights, sparseMat = sparseMat, nfolds = nfolds,
      nlambda = nlambda,  minVars = NULL, maxDim = 20, useMin = useMin, ...)
  out <- list(hal)
  class(out) <- "SL.hal"
  return(out)
}
