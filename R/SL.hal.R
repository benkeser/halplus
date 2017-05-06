#' Highly Adaptive Lasso
#'
#' SuperLearner wrapper for \code{hal}.
#'
#' @param Y A \code{numeric} of outcomes
#' @param X A \code{data.frame} of predictors
#' @param newX Optional \code{data.frame} on which to return predicted values
#' @param verbose A \code{boolean} indicating whether to print output on functions progress
#' @param obsWeights Optional \code{vector} of observation weights to be passed to \code{cv.glmnet}
#' @param nfolds Number of CV folds passed to \code{cv.glmnet}
#' @param nlambda Number of lambda values to search across in \code{cv.glmnet}
#' @param useMin Option passed to \code{cv.glmnet}, use minimum risk lambda or 1se lambda (more
#'   penalization)
#' @param family Needs to have a character object in \code{family$family} as required by \code{SuperLearner}
#' @param ... Any other arguments to pass-through to hal()
#'
#' @export
SL.hal <- function(Y,
                   X,
                   newX,
                   family = gaussian(),
                   verbose = TRUE,
                   obsWeights = rep(1, length(Y)),
                   nfolds = ifelse(length(Y) <= 100, 20, 10),
                   nlambda = 100,
                   useMin = TRUE,
                   ...) {
      halOut <- hal(Y = Y, X = X, newX = newX, verbose = verbose,
      obsWeights = obsWeights, nfolds = nfolds,
      nlambda = nlambda, useMin = useMin, ...)

      out <- list(object = halOut, pred = halOut$pred)
      class(out) <- "SL.hal"

      return(out)
}

#' predict.SL.hal
#' 
#' predict method for \code{SL.hal}
#' 
#' @param object A fitted object of class \code{hal}
#' @param newdata A matrix of new predictions to obtain predictions
#' @param bigDesign A boolean indicating whether to obtain predictions all at once
#' (which may be memory intractable) or to split up the task into smaller chunks
#' @param nChunks A numeric indicating how many chunks to split the prediction task into
#' (if \code{bigDesign = FALSE})
#' @param ... Other arguments passed to \code{predict}
#' 
#' @importFrom stats predict
#' @export 
predict.SL.hal <- function(object, newdata, bigDesign = FALSE, chunks = 5000, ...){
      pred <- stats::predict(object$object, newdata = newdata, bigDesign = bigDesign, 
                      chunks = chunks,...)
      return(pred)
}

