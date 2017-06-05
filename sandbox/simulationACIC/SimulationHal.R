################################################################
#11 covariates
gendata1=function(n){
  U1 = runif(n,0,1)
  W1= -1*(U1<=.5)+(U1>.5)
  W2=rnorm(n)
  W3=rnorm(n,0,1)
  W4=rbinom(n, 1, 0.5)
  W5=rbinom(n,1,.7)
  W6=rbinom(n,1,.3)
  W7=rbinom(n,1,.45)
  W8=rgamma(n, 2, 1)
  W9=runif(n,-4,4)
  W10= -1*(U1<=.3) + (U1>.3)
  A=rbinom(n,1,{plogis(-.28*W1+5*W2 + W4+.08*W3 -1+W5*W6+.5*W7+W8^2+.2*W9*W10)})
  Y=rnorm(n,2*A+W1*W2+.4*W3-.33*W4*W5+.222*W6-W7*W8+.1*W9+.8*cos(W10),2)

  data.frame(A,W1,W2,W3,W4,W5,W6,W7,W8,W9,W10,Y)
}

if (!require(devtools)) install.packages(devtools)
if (!require(hal)) devtools::install_github("nhejazi/halplus")
if (!require(SuperLearner)) devtools::install_github("ecpolley/SuperLearner")


simdata1 <- gendata1(50)
Y1 <- simdata1$Y
X1 <- simdata1[-12]


ptm <- proc.time()
halresults1 <- hal(Y = Y1,
                   X = X1,
                   verbose = TRUE,
                   minVars = 5)
proc.time() - ptm
#With 250 obs and 11 cov takes 239.868 seconds

ptm <- proc.time()
SL_lib <- c(c("SL.hal", "screen.hal"), c("SL.glm"))
sl_halresults1 <- SuperLearner(Y = Y1, X = X1,
                               SL.library = SL_lib,
                               verbose = TRUE)
proc.time() - ptm

################################################################
#10 covariates
gendata4=function(n){
  U1 = runif(n,0,1)
  W1= -1*(U1<=.5)+(U1>.5)
  W2=rnorm(n)
  W3=rnorm(n,0,1)
  W4=rbinom(n, 1, 0.5)
  W5=rbinom(n,1,.7)
  W6=rbinom(n,1,.3)
  W7=rbinom(n,1,.45)
  W8=rgamma(n, 2, 1)
  W9=runif(n,-4,4)
  A=rbinom(n,1,{plogis(-.28*W1+5*W2 + W4+.08*W3 -1+W5*W6+.5*W7+W8^2+.2*W9)})
  Y=rnorm(n,2*A+W1*W2+.4*W3-.33*W4*W5+.222*W6-W7*W8+.1*cos(W9),2)

  data.frame(A,W1,W2,W3,W4,W5,W6,W7,W8,W9,Y)
}

simdata4<-gendata4(250)
Y4<-simdata4$Y
X4 <- simdata4[-11]

ptm <- proc.time()
SL_lib <- c("hal", "screen.hal")
sl_halresults <- SuperLearner(Y = Y1,
                              X = X1,
                              SL.library = SL_lib,
                              verbose = TRUE)
proc.time() - ptm


ptm <- proc.time()
halresults4<-hal(Y=Y4,X=X4)
proc.time() - ptm
#With 250 obs and 10 cov takes 117.976 seconds







##################################################################################################
#sim function to see how long it takes for 7 cov
gendata2=function(n){
  U1 = runif(n,0,1)
  W1= -1*(U1<=.5)+(U1>.5)
  W2=rnorm(n)
  W3=rnorm(n,0,1)
  W4=rbinom(n, 1, 0.5)
  W5=rbinom(n,1,.7)
  W6=rbinom(n,1,.3)
  A=rbinom(n,1,{plogis(-.28*W1+5*W2 + W4+.08*W3 -1+W5*W6)})
  Y=rnorm(n,2*A+W1*W2+.4*W3-.33*W4*W5+.222*W6,2)

  data.frame(A,W1,W2,W3,W4,W5,W6,Y)
}

simdata2<-gendata2(250)
Y2<-simdata2$Y
X2 <- simdata2[-8]



#With 7 cov it runs in only 39.254 seconds
ptm <- proc.time()
halresults2<-hal(Y=Y2,X=X2)
proc.time() - ptm


########################################################################################
#sim function to see how long it takes for 5 cov
gendata3=function(n){
  U1 = runif(n,0,1)
  W1= -1*(U1<=.5)+(U1>.5)
  W2=rnorm(n)
  W3=rnorm(n,0,1)
  W4=rbinom(n, 1, 0.5)
  A=rbinom(n,1,{plogis(-.28*W1+5*W2 + W4+.08*W3)})
  Y=rnorm(n,2*A+W1*W2+.4*W3-.33*W4,2)

  data.frame(A,W1,W2,W3,W4,Y)
}

simdata3<-gendata3(250)
Y3<-simdata3$Y
X3 <- simdata3[-6]



#With 5 cov it runs in only 14.618 seconds
ptm <- proc.time()
halresults3<-hal(Y=Y3,X=X3)
proc.time() - ptm



##########################################################################
#now trying out the screening function on the larger data

prescreen.hal <- function(Y, X, alpha = .05, min = 5, ...){
  pvalues <- rep(NA, ncol(X))
  for (i in 1:ncol(X)){
    m <- lm(Y~ X[,i])
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

prescreen.hal()
