#' Creates a sparseMatrix of the indicators
#'
#' Add more details.
#'
#' @param X X
#' @param newX newX
#' @param verbose If true output extra info during execution.
#' @importFrom plyr llply
#' @export
# TODO: don't always have a newX? What is the difference between X and newX?
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

  # Start by creating a list of univariate indicators.
  # The length of the list is d and the entries are matrices of row and column
  # indices for a design matrix based only on that covariate, i.e. columns in
  # each list entry run from 1:n, so we can use intersect later for higher order
  # terms.
  if (verbose) cat("Making", d, "basis functions of dimension 1\n")

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

  # functions used for higher order terms
  .myIntersect <- function(...) {
    Reduce(intersect, list(...))
  }

  # This is the major user of execution time and memory.
  # What does this do??
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

    # Get intersection
    # CK: parse() and eval() are probably slowing this down.
    out <- eval(parse(text = paste0(
      paste0("mapply(.myIntersect,"),
      paste0("newtmp[[", 1:length(tmp), "]]", collapse = ","),
      ",SIMPLIFY=FALSE)"
    )))
    out
  }

  # Loop over higher order terms.
  if (d > 1) {
    for (k in 2:d) {

      # Matrix of all d choose k combinations.
      combos <- utils::combn(d, k)

      if (verbose) cat("Making", ncol(combos), "basis functions of dimension", k, "\n")

      # Adjust column indicators for column indices.
      colStart <- colEnd + 1L
      colEnd <- (colStart - 1L) + ncol(combos)

      # list of length d choose k, each entry
      # containing n indices of columns corresponding to subjects
      # This is the primary cause of execution time and memory usage.
      j.list <- plyr::alply(combos, 2L, function(a) {
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

      # Put it together
      # CK: this is dynamic memory allocation - pre-allocating would be much better if possible.
      # Can we determine what the size will be in advance, or no?
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
