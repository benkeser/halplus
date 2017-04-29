## NEW SIMULATION DATA SETS

# univariate 
makeData1 <- function(n){
  x1 <- runif(n,-4,4)
  y <- x1/20 - 0.42*x1^2 + rnorm(n)
  return(data.frame(x1=x1,y=y))
}

# jumps
makeData2 <- function(n){
  x1 <- runif(n,-4,4)
  y <- -2.7*as.numeric(x1< -3) + 2.5*as.numeric(x1 > -2) - 
    2*as.numeric(x1>0) + 4*as.numeric(x1>2) - 3*as.numeric(x1>3) + rnorm(n)
  return(data.frame(x1=x1,y=y))
}

# sins
makeData3 <- function(n){
  x1 <- runif(n,-4,4)
  y <- 2*sin(pi/2*abs(x1)) + 2*cos(pi/2*abs(x1)) + rnorm(n)
  
  return(data.frame(x1=x1,y=y))
}

# trivariate 
# smooth 
makeData4 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  y <- x1/15 - 0.28*x1^2 + 0.5*x2 + 0.25*x3*x2 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,y=y))
}

# jumps + interactions
makeData5 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  y <- -2*as.numeric(x1< -3)*x3 + 2.5*as.numeric(x1 > -2) - 
    2*as.numeric(x1>0) + 2.5*as.numeric(x1>2)*x3 - 2.5*as.numeric(x1>3) + 
    1*as.numeric(x2 > -1) - 
    4*as.numeric(x2>1)*x3 + 2*as.numeric(x2>3) + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,y=y))
}

# really hard
makeData6 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4) 
  x3 <- rbinom(n, 1, 0.5)
  y <- x3*(4*sin(pi/2*abs(x1))*(x2 < 0) + 4.1*cos(pi/2*abs(x1))*(x2 > 0)) + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,y=y))
}

### five-variate
# smooth
makeData7 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  
  y <- x1/10 - 0.3*x1^2 + 0.25*x2 + 0.5*x3*x2 - 0.5*x4 + 0.2*x5^2/5 - 0.1*x5 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,y=y))
}

# jumps + interactions
makeData8 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  
  y <- -1*as.numeric(x1< -3)*x3 + 0.5*as.numeric(x1 > -2) - 
    1*as.numeric(x1>0) + 2*as.numeric(x1>2)*x3 - 3*as.numeric(x1>3) + 
    1.5*as.numeric(x2 > -1) - 
    5*as.numeric(x2>1)*x3 + 2*as.numeric(x2>3) + as.numeric(x4 < 0)*2 - 
    as.numeric(x5 > 5)*1 - as.numeric(x4<0)*as.numeric(x1<0) + 2*x3 + 
    rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,y=y))
}

# really hard
makeData9 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4) 
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  
  y <- x3*(3.75*sin(pi/2*abs(x1))*(x2 < 0) + 4*cos(pi/2*abs(x1))*(x2 > 0)) + 
    sin(x4*pi)*x5/10 + cos(abs(x4-x5))*x3 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,y=y))
}

### seven variate
# smooth
makeData10 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  x6 <- rbinom(n, 1, 0.25)
  x7 <- rpois(n, 4)
  
  y <- 0.5*x1 - 0.24*x1^2 + 0.25*x2 + 0.25*x3*x2 - 0.25*x4 + 0.05*x5^2 - 0.25*x5 + 0.05*x6*x3*2 + 0.05*x7 + 0.02*x7*x1 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6, x7=x7,y=y))
}

# jumps + interactions
makeData11 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4)
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  x6 <- rbinom(n, 1, 0.25)
  x7 <- rpois(n, 4)
  
  y <- -1*as.numeric(x1< -3)*x3 + 0.5*as.numeric(x1 > -2) - 
    0.5*as.numeric(x1>0) + 2*as.numeric(x1>2)*x3 - 0.5*as.numeric(x1>3) + 
    1.5*as.numeric(x2 > -1) - 
    1*as.numeric(x2>1)*x3 + 1*as.numeric(x2>3)  + 
    as.numeric(x4 < 0)*2 - as.numeric(x5 > 5)*1 - 
    as.numeric(x4<0)*as.numeric(x1<0) + 1.75*x3 + 
    2*as.numeric(x7 > 1) - 0.5*x6 + as.numeric(x7 > x5)*2 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6, x7=x7,y=y))
}

# really hard
makeData12 <- function(n){
  x1 <- runif(n,-4,4)
  x2 <- runif(n,-4,4) 
  x3 <- rbinom(n, 1, 0.5)
  x4 <- rnorm(n)
  x5 <- rgamma(n, 2, 1)
  x6 <- rbinom(n, 1, 0.25)
  x7 <- rpois(n, 4)
  
  y <- x3*(2.75*sin(pi/2*abs(x1))*(x2<0) + 4*cos(pi/2*abs(x1))*(x2 > 0)) + 
    sin(x4*pi)*x5/10 + cos(abs(x4-x5))*x3 + 
    x7*sin(abs(x5)*x6)/5 + x5*cos(abs(x7)*x6)*x3/1.5 + rnorm(n)
  return(data.frame(x1=x1,x2=x2,x3=x3,x4=x4,x5=x5,x6=x6, x7=x7,y=y))
}

########################################################################
# post process functions
########################################################################
plotRslt <- function(riskList,p=1,legend=FALSE){
  par(mar=c(2.1,10.1,1.1,1.1))
  K <- length(riskList[[1]])
  plot(10,0,pch="",ylim=c(1,K),xlim=range(unlist(riskList)),
       bty="n",xlab="",ylab="",yaxt="n",xaxt="n",
       log="x",mgp=c(1.1,0,0))
  
  axis(side=1)
  
  lab <- apply(matrix(names(riskList[[1]])), 1, function(x){ 
    strsplit(x,split="_")[[1]][1]
  })

  ranks <- lapply(riskList, rank)
  rankAve <- Reduce("+",ranks)/3
  
  ordA <- order(rankAve)
  axis(side=2,at=K:1,las=2,labels=lab[ordA])
  
  library(RColorBrewer)
  mycol <- brewer.pal(5, "Blues")[3:5]
  for(i in 1:3){
    points(x=riskList[[i]][ordA], y=K:1, col=mycol[i],pch=p)
  }
  
  if(legend){
    legend(x="topright",inset=0.05, title="n", 
           pch=p,col=mycol,legend=c(100,500,1000))
  }
}


plotSummary <- function(riskList,nsim=27){
  par(mar=c(2.1,12.1,1.1,4.1))
  par(mgp=c(1,1,0))
  K <- length(riskList[[1]])
  plot(K,0,pch="",ylim=c(1,K),xlim=c(1,K),
       bty="n",xlab="Rank",ylab="",yaxt="n",xaxt="n")
  
  axis(side=1,at=1:K,labels=c(1,rep("",K-2),K))
  
  lab <- apply(matrix(names(riskList[[1]])), 1, function(x){ 
    strsplit(x,split="_")[[1]][1]
  })
  
  ranks <- lapply(riskList, rank)
  rankAve <- Reduce("+",ranks)/nsim
  
  ordA <- order(rankAve)
  axis(side=2,at=K:1,las=2,labels=lab[ordA])
  
  library(RColorBrewer)
  mycol <- brewer.pal(5, "Blues")[3:5]
  for(j in 1:(nsim/3)){
  for(i in ((j-1)*3+1):(3*j)){
    points(x=ranks[[i]][ordA]+runif(K,-0.2,0.2), y=K:1 + runif(K,-0.2,0.2), col=mycol[i-(3*(j-1))],pch=i-(3*(j-1)))
  }
  }
  
  legend(x="topright",inset=0.05, title="n", 
         fill=mycol,legend=c(100,500,1000))
}


plotR2 <- function(r2List,p=1,legend=FALSE,...){
  par(mar=c(3.1,10.1,1.1,1.1),mgp=c(2,0.5,0))
  K <- length(r2List[[1]])
  plot(10,0,pch="",ylim=c(1,K),...,
       #xlim=c(min(unlist(r2List)),0.8),
       #xlim=c(-0.4,0.8),
       bty="n",ylab="",xlab=expression(R^2),
       yaxt="n",xaxt="n")
  abline(v=0.8,lty=3)
  axis(side=1)
  
  lab <- apply(matrix(names(r2List[[1]])), 1, function(x){ 
    strsplit(x,split="_")[[1]][1]
  })
  
  rankAve <- Reduce("+",r2List)/length(r2List)
  
  ordA <- order(-rankAve)
  halLab <- which(lab[ordA]=="HAL")
  betterLab <- c(lab[ordA][1:length(lab) < halLab])
  worseLab <- c(lab[ordA][1:length(lab) > halLab])
  
  fullLab <- c(betterLab, expression(bold("HAL")),worseLab)
  axis(side=2,at=K:1,las=2,
       labels=fullLab)
  
  library(RColorBrewer)
  mycol <- brewer.pal(9, "Greys")[3:(2+length(r2List))]
  for(i in 1:length(r2List)){
    points(x=r2List[[i]][ordA], y=K:1, col=mycol[i],pch=p)
  }
  
  if(legend){
    legend(x="topleft", title="n", 
           pch=p,col=mycol,legend=c(500,1000,2000))
  }
}

plotSummaryR2 <- function(r2List,nsim=36,crossN=TRUE,
                          mypch=c(1,1,1),
                          jitter=FALSE,legend=FALSE,
                          crossSims=TRUE,
                          nPerSim=4,...){
  par(mar=c(3.1,10.1,1.1,1.1),mgp=c(2,0.5,0))
  K <- length(r2List[[1]])
  plot(K,0,pch="",ylim=c(1,K),
       bty="n",xlab=expression(R^2),ylab="",yaxt="n",xaxt="n",...)
  abline(v=0.8,lty=3)
  axis(side=1)
  #axis(side=1,at=1:K,labels=c(1,rep("",K-2),K))
  
  # lab <- apply(matrix(names(r2List[[1]])), 1, function(x){ 
  #   strsplit(x,split="_")[[1]][1]
  # })
  
  rankAve <- Reduce("+",r2List)/nsim
  lab <- apply(matrix(names(r2List[[1]])), 1, function(x){ 
    strsplit(x,split="_")[[1]][1]
  })
  ordA <- order(-rankAve)
  halLab <- which(lab[ordA]=="HAL")
  betterLab <- c(lab[ordA][1:length(lab) < halLab])
  worseLab <- c(lab[ordA][1:length(lab) > halLab])
  fullLab <- c(betterLab, expression(bold("HAL")),worseLab)
  
  axis(side=2,at=K:1,las=2,labels=fullLab)
  library(RColorBrewer)
  mycol <- brewer.pal(9, "Greys")[3:(2+nPerSim)]
  
  if(legend){
  legend(x="topleft",inset=0.05, title="n", 
         fill=mycol,legend=c(100,500,1000,2000))
  }
  for(j in 1:(nsim/nPerSim)){
    for(i in ((j-1)*nPerSim+1):(nPerSim*j)){
      points(x=unlist(r2List[[i]])[ordA], y=K:1 + ifelse(jitter,runif(K,-0.2,0.2),rep(0,K)), 
             col=ifelse(crossN, mycol[i-(nPerSim*(j-1))], mycol[3]),
             pch=mypch[j])
      
    }
  }
  
}

