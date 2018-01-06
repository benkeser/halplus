#' doPred
#' 
#' A helper function for the predict method for class \code{hal}.
#' @param object A fitted object of class \code{hal}
#' @param newdata A matrix of new predictions to obtain predictions
#' @param s Option from \code{glmnet} indicating what value to use. Defaults to the value
#' that was specified in the original call to \code{hal} (which itself defaults to the value
#' which minimizes MSE).
#' @param verbose A \code{boolean} indicating whether to print output on functions progress
#' @param ... Additional arguments (not currently used)
#' 
#' @importFrom Matrix sparseMatrix

doPred <- function(object, newdata, verbose = FALSE, s, offset) {
  if (is.vector(newdata))
    newdata <- matrix(newdata)

  if (verbose)
    cat("Making initial sparse matrix for predictions \n")

  #---------------------------------------------------
  # Make an initial sparse matrix based on newdata
  #---------------------------------------------------
  tmp <- makeSparseMat(X = object$X,
                       newX = newdata,
                       verbose = verbose)

  #---------------------------------------------------
  # Correct duplicate columns (if any)
  #---------------------------------------------------
  if (length(object$dupInds) > 0) {
    if (verbose)
      cat("Correcting duplicate columns in sparse matrix \n")
    # get vector of duplicate columns
    dupVec <- unlist(object$colDups, use.names = FALSE)
    # number of each duplicate
    nperDup <- unlist(lapply(object$colDups, length), use.names = FALSE)
    # number of duplicates to roll through
    K <- length(nperDup)
    # start and ending index
    startInd <- c(0, cumsum(nperDup)[1:(K - 1)])
    endInd <- cumsum(nperDup)
    # not duplicate colums
    notdupVec <- (1:ncol(tmp))[-dupVec]
    # put all the duplicated guys first
    tmp <- tmp[, c(dupVec, notdupVec)]

    uniqRowsList <- list()
    myp <- c(0, rep(NA, K))
    # look at the i associated with
    for (k in 1:K) {
      # this condition ensures that not all the values of a given set of duplicates
      # are equal to zero.
      if (tmp@p[startInd[k] + 1] != tmp@p[endInd[k] + 1]) {
        Fidx_base0 <- (tmp@p[startInd[k] + 1]):(tmp@p[endInd[k] + 1] - 1)
        nonzero_rows <-
          tmp@i[Fidx_base0 + 1] + 1 # actual row numbers of non-zero elements in column i=1
        # unique nonzero_rows
        theseRows <- sort(unique(nonzero_rows))
        uniqRowsList[[k]] <- theseRows
        # a new p vector for duplicated columns
        myp[k + 1] <- myp[k] + length(theseRows)
      } else{
        # the whole block for this set of duplicates is 0
        uniqRowsList[[k]] <- NULL
        myp[k + 1] <- myp[k]
      }
    }

    # look at this sparse matrix
    myi <- unlist(uniqRowsList)
    # check to see if it came out right
    # grbg1 <- sparseMatrix(i=myi, p=myp, x=1)

    # fix up p with nondup columns
    ## for this example every non-duplicated column in the new design
    ## matrix is 0, which is causing this to break. I think.
    if (tmp@p[endInd[K] + 1] != tmp@p[length(tmp@p)]) {
      fulli <-
        c(myi, tmp@i[(tmp@p[endInd[K] + 1] + 1):tmp@p[length(tmp@p)]] + 1)
      fullp <- c(myp,
                 tmp@p[((endInd[K] + 1) + 1):length(tmp@p)] -
                   tmp@p[(endInd[K] + 1)] + myp[K + 1])
    } else{
      fulli <- myi
      fullp <- myp
    }
    # sparseMatrix with duplicate columns removed 
    tmp <- Matrix::sparseMatrix(
      i = fulli,
      p = fullp,
      x = 1,
      dims = c(
        length(newdata[, 1]),
        length(notdupVec) + length(object$colDup)
      )
    )
  }

  # call predict.glmnet to get predictions on new sparseMat with duplicate 
  # columns removed. 
  if (is.null(offset)) {
    pred <- stats::predict(object$object$glmnet.fit, newx = tmp,
                           s = s, type = 'response')
  } else {
    pred <- stats::predict(object$object$glmnet.fit, newx = tmp,
                           s = s, type = 'response', newoffset = offset)
    }
  return(pred)
}
