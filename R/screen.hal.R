
screen.hal <- function(Y, X, alpha = .05, min = 5, ...){
  .SL.require('hal')
  pvalues <- rep(NA, ncol(X))
  for (i in 1:ncol(X)){
    m <- lm(Y~ A+ X[,i])
    p <- try(summary(m)$coef[3,4], silent = TRUE)
    if (class(p) == "try-error") {
      pvalues[i] <- 1
    } else {
      pvalues[i] <- p
    }
  }
  keep <- pvalues <= alpha
  if(sum(keep) < min){
    keep[order(pvalues)[1:min]] <- TRUE
  }
  return(keep)
}
