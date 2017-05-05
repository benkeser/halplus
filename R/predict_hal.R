#' predict.hal
#' 
#' A predict method for objects of class \code{hal}.
#' 
#' @param object A fitted object of class \code{hal}
#' @param newdata A matrix of new predictions to obtain predictions
#' @param bigDesign A boolean indicating whether to obtain predictions all at once
#' (which may be memory intractable) or to split up the task into smaller chunks
#' @param chunks A numeric indicating how many chunks to split the prediction task into
#' (if \code{bigDesign = FALSE})
#' @param s Option from \code{glmnet} indicating what value to use. Defaults to the value
#' that was specified in the original call to \code{hal} (which itself defaults to the value
#' which minimizes MSE).
#' @param ... Additional arguments (not currently used)
#' 
#' @export
predict.hal <-
  function(object,
           newdata,
           bigDesign = FALSE,
           verbose = TRUE,
           chunks = 1000,
           s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se),
           ...)
  {
    # all predictions at once
    if (bigDesign) {
      pred <- doPred(object = object,
                     newdata = newdata,
                     verbose = verbose)
    } else {
      nNew <- length(newdata[, 1])
      nChunks <- floor(nNew / chunks) + ifelse(nNew %% chunks == 0, 0, 1)
      pred <- rep(NA, length(nNew))
      for (i in 1:nChunks) {
        minC <- (i - 1) * chunks + 1
        maxC <- ifelse(i == nChunks, nNew, i * chunks)
        pred[minC:maxC] <-
          doPred(
            object = object,
            s = s,
            newdata = newdata[minC:maxC, ],
            verbose = verbose
          )
      }
    }
    return(as.numeric(pred))
  }
