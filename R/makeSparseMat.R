#' makeSparseMat
#' 
#' Function to create a sparse design matrix of basis functions
#' based on a matrix of predictors.
#' 
#' @param X A \code{data.frame} of predictors
#' @param newX Optional \code{data.frame} on which to return predicted values
#' @param verbose A \code{boolean} indicating whether to print output on functions progress

#' @importFrom plyr llply alply
# TODO: don't always have a newX.

makeSparseMat <- function(X, newX = X, verbose = TRUE) {

  if (is.vector(X)) X <- matrix(X, ncol = 1)

  if (is.vector(newX)) newX <- matrix(newX, ncol = 1)

  d <- ncol(X)
  stopifnot(ncol(X) == ncol(newX))

  nX <- length(X[, 1])
  n <- length(newX[, 1])

  # numbers used to correct column indices later
  colStart <- 1
  colEnd <- d

  # start by creating a list of univariate indicators
  # length of the list is d and the entries are matrices
  # of row and column indices for a design matrix based
  # only on that covariate, i.e. columns in each list entry
  # run from 1:n, so we can use intersect later for higher
  # order terms.
  if (verbose) cat("Making ", d, " basis fns of dimension  1 \n")

  uni <- plyr::alply(matrix(1:d), 1, function(x) {
    j <- plyr::alply(matrix(newX[, x]), 1, function(y) {
      which(X[, x] <= y)
    })
    i <- rep(1:n, unlist(lapply(j, length), use.names = FALSE))
    cbind(unlist(i, use.names = FALSE), unlist(j, use.names = FALSE))
  })
  # number of 1's for each variable -- for variables with
  # length(unique(x)) == length(x) will be n*(n+1)/2, but if there
  # are ties, the length will be different
  nperuni <- lapply(uni, nrow)

  # slap them all together
  uni.ij <- Reduce("rbind", uni)

  # fix up the column indices
  uni.ij[, 2] <- uni.ij[, 2] +
    rep.int((colStart:colEnd) - 1, times = unlist(nperuni, use.names = FALSE)) *
    nX

  # i = row indices, j = column indices
  i <- uni.ij[, 1]
  j <- uni.ij[, 2]

  # loop over higher order terms
  if (d > 1) {
    for (k in 2:d) {
      # matrix of all d choose k combinations
      combos <- combn(d, k)

      if (verbose) cat("Making ", ncol(combos), " basis fns of dimension ", k, "\n")
      # adjust column indicators for column indices
      colStart <- colEnd + 1
      colEnd <- (colStart - 1) + ncol(combos)

      # list of length d choose k, each entry
      # containing n indices of columns corresponding to subjects
      j.list <- plyr::alply(combos, 2, function(a) {
        .getIntersect(uni[a])
      })

      # list of length d choose k, each entry containing
      # n indices of rows corresponding to subjects
      i.list <- plyr::llply(j.list, function(x) {
        rep(as.numeric(names(x)), unlist(lapply(x, length), use.names = FALSE))
      })

      # number of 1's for each combination
      nper <- lapply(i.list, length)

      # unlist column numbers
      j.list <- lapply(j.list, unlist, use.names = FALSE)

      # unlist rows and columns
      thisi <- unlist(i.list, use.names = FALSE)
      thisj <- unlist(j.list, use.names = FALSE)

      # fix up the column number
      thisj <- thisj +
        rep.int((colStart:colEnd) - 1, times = unlist(nper, use.names =
                                                        FALSE)) * nX

      # put it together
      i <- c(i, thisi)
      j <- c(j, thisj)
    }
  }

  # make the sparseMatrix
  grbg <-
    Matrix::sparseMatrix(
      i = i[order(i)],
      j = j[order(i)],
      x = 1,
      dims = c(n, nX * (2 ^ d - 1))
    )
  return(grbg)
}


#' myIntersect
#' 
#' Helper function for higher order interaction basis functions.
#' 
#' @param ... Arguments passed to intersect
#' 
.myIntersect <- function(...) {
  Reduce(intersect, list(...))
}

#' getIntersect 
#' 
#' Heper function for higher order interaction basis functions.
#' 
#' @param ... Arguments passed to \code{lapply}
.getIntersect <- function(...) {
  tmp <- lapply(..., function(b) {
    split(b[, 2], b[, 1])
  })
  tmpNames <- lapply(tmp, function(l) {
    as.numeric(names(l))
  })
  overlap <- Reduce(intersect, tmpNames)

  # indices of tmp that overlap
  newtmp <- lapply(tmp, function(b) {
    b[paste(overlap)]
  })

  # get intersection
  out <- eval(parse(text = paste0(
    paste0("mapply(.myIntersect,"),
    paste0("newtmp[[", 1:length(tmp), "]]", collapse = ","),
    ",SIMPLIFY=FALSE)"
  )))
  out
}
