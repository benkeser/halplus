#' hal
#'
#' The highly adaptive lasso fitting function. This function takes a matrix of predictor values
#' (which can be binary or continuous) and converts it into a set of indicator basis functions
#' that perfectly fit the data. The function then uses cross-validated lasso (via the \code{glmnet}
#' package) to select basis functions. The resulting fit is called the highly adaptive lasso.
#' The process of creating the indicator basis functions can be extremely time and memory intensive
#' as it involves creating n(2^d - 1) basis functions, where n is the number of observations
#' and d the number of covariates. The function also must subsequently search over basis functions
#' for those that are duplicated and store the results. Future implementations will attempt to
#' streamline this process to the largest extent possible; however, for the time being implementing
#' with values of n and d such that n(2^d - 1) > 1e7 is not recommended.
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
#' @param debug For benchmarking. Setting to \code{TRUE} will run garbage collection to
#' improve the accuracy of memory monitoring
#' @param parallel A boolean indicating whether to use a parallel backend, if possible
#' @param ... Not currently used
#'
#' @importFrom glmnet cv.glmnet
#' @importFrom bit bit
#' @importFrom stats gaussian predict
#' @importFrom utils combn
#' @importFrom data.table data.table set setkey
#' @importFrom plyr alply
#' @importFrom stringr str_c str_replace_na
#'
#' @export

hal <- function(Y,
                X,
                newX = NULL,
                verbose = FALSE,
                obsWeights = rep(1, length(Y)),
                nfolds = ifelse(length(Y) <= 100, 20, 10),
                nlambda = 100,
                useMin = TRUE,
                debug = TRUE,
                parallel = FALSE,
                ... # allow extra arguments with no death
) {


  #---------------------------------------------------------
  # Preliminary operations
  #---------------------------------------------------------
  d <- ncol(X)
  n <- length(X[, 1])

  if (is.vector(X))
    X <- matrix(X, ncol = 1)

  if (is.vector(newX))
    newX <- matrix(newX, ncol = 1)

  # Run garbage collection if we are in debug mode.
  if (debug) gc()

  # Initialize prediction object to null in case newX = NULL.
  pred <- NULL
  times <- NULL

  #------------------------------------------------------------
  # Make initial design matrix (including duplicated columns)
  #------------------------------------------------------------
  if (verbose) cat("Making sparse matrix \n")
  time_sparse_start <- proc.time()

  # makeSparseMat to create sparseMatrix design matrix
  X.init <- makeSparseMat(X = X, newX = X, verbose = verbose)

  time_sparse_end <- proc.time()
  time_sparse_matrix <- time_sparse_end - time_sparse_start

  # Run garbage collection if we are in debug mode.
  if (debug) gc()

  #------------------------------------------------------------
  # Removing duplicated columns
  # TODO: Should this code be wrapped up in a function or would
  # passing all those objects to another function waste memory?
  #------------------------------------------------------------
  if (verbose) cat("Finding duplicate columns \n")

  # Number of columns will become the new number of observations in the data.table
  nIndCols <- ncol(X.init)

  # Pre-allocate a data.table with one column, each row will store a single column from X.init
  datDT <-
    data.table(ID = 1:nIndCols,
               bit_to_int_to_str = rep.int("0", nIndCols))
  # Each column in X.init will be represented by a unique vector of integers.
  # Each indicator column in X.init will be converted to a row of integers or
  # a string of cat'ed integers in data.table. The number of integers needed to
  # represent a single column is determined automatically by package "bit" and
  # it depends on nrow(X.init)
  nbits <- nrow(X.init) # number of bits (0/1) used by each column in X.init
  bitvals <- bit::bit(length = nbits) # initial allocation (all 0/FALSE)
  nints_used <- length(unclass(bitvals)) # number of integers needed to represent each column

  # Track which results gave NA in one of the integers
  ID_withNA <- NULL

  # For loop over columns of X.init
  for (i in 1:nIndCols) {
    bitvals <- bit::bit(length = nbits) # initial allocation (all 0/FALSE)
    Fidx_base0 <-
      (X.init@p[i]):(X.init@p[i + 1] - 1) # zero-base indices of indices of non-zero rows for column i=1
    nonzero_rows <-
      X.init@i[Fidx_base0 + 1] + 1 # actual row numbers of non-zero elements in column i=1
    # print(i); print(nonzero_rows)
    # X.init@i[i:X.init@p[i]]+1 # row numbers of non-zero elements in first col
    bitvals[nonzero_rows] <- TRUE
    # str(bitwhich(bitvals))
    intval <-
      unclass(bitvals) # integer representation of the bit vector
    # stringval <- str_c(intval, collapse = "")
    if (any(is.na(intval)))
      ID_withNA <- c(ID_withNA, i)
    data.table::set(datDT, i, 2L,
                    value = stringr::str_c(stringr::str_replace_na(intval),
                                           collapse = ""))
  }
  # create a hash-key on the string representation of the column,
  # sorts it by bit_to_int_to_str using radix sort:
  data.table::setkey(datDT, bit_to_int_to_str)
  # add logical column indicating duplicates,
  # following the first non-duplicate element
  datDT[, duplicates := duplicated(datDT)]
  # just get the column IDs and duplicate indicators:
  datDT[, .(ID, duplicates)]

  dupInds <- datDT[, ID][which(datDT[, duplicates])]

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # OS: NEW FASTER APPROACH TO FIND DUPLICATE IDs
  # get the number of duplicates in each group if its 1 the column is
  # unique and we are note interested:
  datDT[, Ngrp := .N, by = bit_to_int_to_str]
  # collapse each duplicate group into a list of IDs, do that only
  # among strings that have duplicates
  collapsedDT <- datDT[Ngrp > 1, list(list(ID)), by = bit_to_int_to_str]
  colDups <- collapsedDT[["V1"]]
  # colDups[[2]]
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # OS: OLD APPROACH TO BE REMOVED AFTER VALIDATED
  # uniqDup <- unique(datDT[duplicates == TRUE, bit_to_int_to_str])
  # colDups.old <- alply(uniqDup, 1, function(l) {
  #   datDT[, ID][which(datDT[, bit_to_int_to_str] == l)]
  # })
  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  time_dup_end = proc.time()

  time_find_duplicates = time_dup_end - time_sparse_end

  # Run garbage collection if we are in debug mode.
  if (debug) gc()

  #------------------------------------------------------------
  # Fit lasso
  #------------------------------------------------------------

  if (verbose) cat("Fitting lasso \n")
  if (length(dupInds) > 0) {
    notDupInds <- (1:ncol(X.init))[-unlist(colDups, use.names = FALSE)]
    keepDupInds <-
      unlist(lapply(colDups, function(x) {
        x[[1]]
      }), use.names = FALSE)

    fitCV <-
      glmnet::cv.glmnet(
        x = X.init[, c(keepDupInds, notDupInds)],
        y = Y,
        weights = obsWeights,
        lambda = NULL,
        lambda.min.ratio = 0.001,
        type.measure = "deviance",
        nfolds = nfolds,
        family = "gaussian",
        alpha = 1,
        nlambda = nlambda,
        parallel = parallel
      )
  } else {
    # No duplication.
    fitCV <- glmnet::cv.glmnet(
      x = X.init,
      y = Y,
      weights = obsWeights,
      lambda = NULL,
      lambda.min.ratio = 0.001,
      type.measure = "deviance",
      nfolds = nfolds,
      family = "gaussian",
      alpha = 1,
      nlambda = nlambda,
      parallel = parallel
    )
  }
  time_lasso_end <- proc.time()
  time_lasso <- time_dup_end - time_lasso_end
  #------------------------------------------------------------
  # Initial output object (pred and times added below)
  #------------------------------------------------------------
  fit <- list(object = fitCV,
              useMin = useMin,
              X = X,
              dupInds = dupInds,
              colDups = colDups,
              pred = NULL,
              times = NULL
              )
  class(fit) <- "hal"

  if (identical(X, newX)) {
    if (length(dupInds) > 0) {
      pred <-
        predict(
          fitCV,
          newx = X.init[, c(keepDupInds, notDupInds)],
          s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se),
          type = "response"
        )
    } else{
      pred <-
        predict(
          fitCV,
          newx = X.init,
          s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se),
          type = "response"
        )
    }
  } else if (!is.null(newX)) {
    pred <- predict(fit,
                    newdata = newX,
                    bigDesign = FALSE,
                    chunks = 10000)
  }

  # wrap up the timing
  time_pred_end <- proc.time()
  time_pred <- time_pred_end - time_lasso_end
  time_everything <- time_pred_end - time_sparse_start
  times <- list(sparse_matrix = time_sparse_matrix,
            find_duplicates = time_find_duplicates,
            lasso = time_lasso,
            pred = time_pred,
            everything = time_everything)
  times <- t(simplify2array(times))

  # Run garbage collection if we are in debug mode.
  if (debug) gc()

  fit$pred <- pred
  fit$times <- times
  if (verbose) cat("Done with hal\n")
  return(fit)
}
