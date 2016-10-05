#' Highly Adaptive Lasso
#'
#' Model fitting function.
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
                family = gaussian(),
                verbose = F,
                obsWeights = rep(1, length(Y)),
                sparseMat = TRUE,
                nfolds = ifelse(length(Y) <= 100, 20, 10),
                nlambda = 100,
                useMin = TRUE,
                ... # allow extra arguments with no death
                ) {

  d <- ncol(X)

  # Initialize prediction object to null in case newX = NULL.
  pred = NULL
  times = NULL

  if (!sparseMat) {
    uniList <- plyr::alply(as.matrix(X), 2, function(x) {
      # myX <- matrix(x,ncol=length(unique(x)), nrow=length(x)) -
      #   matrix(unique(x), ncol=length(unique(x)), nrow=length(x), byrow=TRUE)
      myX <- matrix(x, ncol = length(x), nrow = length(x)) -
        matrix(x,
               ncol = length(x),
               nrow = length(x),
               byrow = TRUE)
      myX <- ifelse(myX < 0, 0, 1)
      myX
    })

    if (d >= 2) {
      highDList <- plyr::alply(matrix(2:d), 1, function(k) {
        thisList <- plyr::alply(combn(d, k), 2, function(x) {
          Reduce("*", uniList[x])
        })
        Reduce("cbind", thisList)
      })
      initX <-
        cbind(Reduce("cbind", uniList), Reduce("cbind", highDList))
      dup <- duplicated(t(initX))
      designX <- initX[, !dup]
    } else {
      initX <- Reduce("cbind", uniList)
      dup <- duplicated(t(initX))
      designX <- initX[, !dup]
    }

    fitCV <-
      glmnet::cv.glmnet(
        x = designX,
        y = Y,
        weights = obsWeights,
        lambda.min.ratio = 0.001,
        lambda = NULL,
        type.measure = "deviance",
        nfolds = nfolds,
        family = family$family,
        alpha = 1,
        nlambda = nlambda
      )

    fit <- list(object = fitCV,
                useMin = useMin,
                X = X,
                dup = dup,
                sparseMat = sparseMat
    )

    ## get predictions back
    if (!is.null(newX)) {
      mynewX <-
        matrix(newX[, 1], ncol = length(X[, 1]), nrow = length(newX[, 1])) -
        matrix(X[, 1],
               ncol = length(X[, 1]),
               nrow = length(newX[, 1]),
               byrow = TRUE)
      mynewX <- ifelse(mynewX < 0, 0, 1)

      makeNewDesignX <- TRUE
      if (all(dim(X) == dim(newX)))
        makeNewDesignX <- !all(X == newX)

      if (makeNewDesignX) {
        uniList <- plyr::alply(matrix(1:ncol(X)), 1, function(x) {
          myX <- matrix(newX[, x], ncol = length(X[, x]), nrow = length(newX[, x])) -
            matrix(
              X[, x],
              ncol = length(X[, x]),
              nrow = length(newX[, x]),
              byrow = TRUE
            )
          myX <- ifelse(myX < 0, 0, 1)
          myX
        })

        if (d >= 2) {
          highDList <- plyr::alply(matrix(2:d), 1, function(k) {
            thisList <- plyr::alply(combn(d, k), 2, function(x) {
              Reduce("*", uniList[x])
            })
            Reduce("cbind", thisList)
          })

          initX <-
            cbind(Reduce("cbind", uniList), Reduce("cbind", highDList))
          designNewX <- initX[, !dup]
        } else {
          initX <- Reduce("cbind", uniList)
          designNewX <- initX[, !dup]
        }
      } else {
        designNewX <- designX
      }

      pred <- predict(
        fitCV$glmnet.fit,
        newx = designNewX,
        s = ifelse(useMin, fitCV$lambda.min, fitCV$lambda.1se),
        type = "response"
      )
    }

  } else {

    # Using a Sparse Matrix.

    if (is.vector(X))
      X <- matrix(X, ncol = 1)

    if (is.vector(newX))
      newX <- matrix(newX, ncol = 1)

    n <- length(X[, 1])
    d <- ncol(X)

    if (verbose) cat("Making sparse matrix \n")

    time_sparse_start = proc.time()
    X.init <- makeSparseMat(X = X, newX = X, verbose = verbose)

    time_sparse_end = proc.time()
    time_sparse_matrix = time_sparse_end - time_sparse_start

    ## find duplicated columns
    if (verbose) cat("Finding duplicate columns \n")

    # Number of columns will become the new number of observations in the data.table
    nIndCols <- ncol(X.init)
    # Pre-allocate a data.table with one column, each row will store a single column from X.init
    datDT <-
      data.table(ID = 1:nIndCols,
                 bit_to_int_to_str = rep.int("0", nIndCols))
    # Each column in X.init will be represented by a unique vector of integers.
    # Each indicator column in X.init will be converted to a row of integers or a string of cat'ed integers in data.table
    # The number of integers needed to represent a single column is determined automatically by package "bit" and it depends on nrow(X.init)
    nbits <-
      nrow(X.init) # number of bits (0/1) used by each column in X.init
    bitvals <-
      bit(length = nbits) # initial allocation (all 0/FALSE)
    nints_used <-
      length(unclass(bitvals)) # number of integers needed to represent each column

    # Track which results gave NA in one of the integers
    ID_withNA <- NULL

    # For loop over columns of X.init
    for (i in 1:nIndCols) {
      bitvals <- bit(length = nbits) # initial allocation (all 0/FALSE)
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
      data.table::set(datDT, i, 2L, value = str_c(str_replace_na(intval), collapse = ""))
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

    # ----------------------------------------------------------------------
    # OS: NEW FASTER APPROACH TO FIND DUPLICATE IDs
    # ----------------------------------------------------------------------
    # get the number of duplicates in each group if its 1 the column is unique and we are note interested:
    datDT[, Ngrp := .N, by = bit_to_int_to_str]
    # collapse each duplicate group into a list of IDs, do that only among strings that have duplicates
    collapsedDT <- datDT[Ngrp > 1, list(list(ID)), by = bit_to_int_to_str]
    colDups <- collapsedDT[["V1"]]
    # colDups[[2]]

    ## ----------------------------------------------------------------------
    ## OS: OLD APPROACH TO BE REMOVED AFTER VALIDATED
    ## ----------------------------------------------------------------------
    # uniqDup <- unique(datDT[duplicates == TRUE, bit_to_int_to_str])
    # colDups.old <- alply(uniqDup, 1, function(l) {
    #   datDT[, ID][which(datDT[, bit_to_int_to_str] == l)]
    # })
    ## ----------------------------------------------------------------------

    time_dup_end = proc.time()

    time_find_duplicates = time_dup_end - time_sparse_end

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
          family = family$family,
          alpha = 1,
          nlambda = nlambda
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
        family = family$family,
        alpha = 1,
        nlambda = nlambda
      )
    }

    time_lasso_end = proc.time()

    time_lasso = time_lasso_end - time_dup_end

    fit <- list(object = fitCV,
                useMin = useMin,
                X = X,
                dupInds = dupInds,
                colDups = colDups,
                sparseMat = sparseMat
    )

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

    time_pred_end = proc.time()

    time_pred = time_pred_end - time_lasso_end

    time_everything = time_sparse_matrix + time_find_duplicates +
      time_lasso + time_pred

    times = list(sparse_matrix = time_sparse_matrix,
              find_duplicates = time_find_duplicates,
              lasso = time_lasso,
              pred = time_pred,
              everything = time_everything)

    # Convert from a list to a nice matrix.
    times = t(simplify2array(result$times))

    # Done with sparse Matix implementation.
  }

  class(fit) <- "SL.hal"

  out <- list(pred = pred, fit = fit, times = times)
  if (verbose) cat("Done with SL.hal\n")
  return(out)
}
