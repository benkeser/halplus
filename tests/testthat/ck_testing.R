# Original version.
v1 = function(X, newX) {
  n = nrow(newX)
  plyr::alply(matrix(1:d), 1, function(x) {
    j <- plyr::alply(matrix(newX[, x]), 1, function(y) {
      which(X[, x] <= y)
    })
    i <- rep(1:n, unlist(lapply(j, length), use.names = FALSE))
    cbind(unlist(i, use.names = FALSE), unlist(j, use.names = FALSE))
  })
}

# Revised version.
v2 = function(X, newX) {
  n = nrow(newX)
  # Loop over each initial column/variable.
  lapply(1:d, function(col_i) {
    # Now loop over each row (cell) of that column/variable.
    j <- lapply(newX[, col_i], function(row_value) {
      # Save the row indices of cells that are less than that value.
      which(X[, col_i] <= row_value)
    })
    # ???
    i <- rep(1:n, unlist(lapply(j, length), use.names = FALSE))
    # ???
    cbind(unlist(i, use.names = FALSE), unlist(j, use.names = FALSE))
  })
}

result1 = v1(X, newX)
result1a = v1(X, newX)
identical(result1, result1a)

result2 = v2(X, newX)
# Strip result1's attributes because they are unnecessary and prevent
# identical() from returning true.
attributes(result1) = NULL
identical(result1, result2)

class(result1)
class(result2)
length(result1)
length(result2)
str(result1)
str(result2)
