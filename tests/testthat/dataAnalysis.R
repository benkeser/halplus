#==============================================
# Data Analyses with the Highly Adaptive Lasso
#==============================================

# load libraries and source HAL functions
library(SuperLearner)
library(caret)
library(np)
library(RCurl)

# source HAL functions
eval(parse(text=getURL(paste0("https://raw.githubusercontent.com/benkeser/",
                              "hal/master/halFunctions.R"))))
# source other SL functions
eval(parse(text=getURL(paste0("https://raw.githubusercontent.com/benkeser/",
                              "hal/master/Simulation/otherSLFunctions.R")))
     )


# define the SuperLearner library
SL.library <- c("SL.hal",
                "SL.caret.rf",
                "SL.caret.gamSpline",
                "SL.caret.gbm",
                "SL.caret.svmRadial",
                "SL.rpartPrune",
                "SL.polymars",
                "SL.npreg",
                "SL.glm",
                "SL.step.interaction")

#====================================================
# compute CV-SuperLearner for each of the data sets
#====================================================
outList <- vector(mode="list",length=5)
ct <- 0
for(datName in c("cpu","laheart","oecdpanel","pima","fev")){
  ct <- ct + 1
  # read data from github
  datUrl <- getURL(paste0("https://raw.githubusercontent.com/benkeser/",
                          "hal/master/Data%20Analysis/",datName,".csv"))
  dat <- read.csv(textConnection(datUrl),header=TRUE)

  # fit cross-validated super learner
  # each data set is arranged so that the outcome is in the first column
  # and the rest of the variables are in the remaining columns
  set.seed(1568)
  outList[[ct]] <- CV.SuperLearner(
    Y=dat[,1],
    X=dat[,2:ncol(dat)],
    V=10,
    family=gaussian(),
    SL.library=SL.library
  )
}


#==================================================================
# compute CV-MSE for each data set using summary.CV.SuperLearner
#==================================================================
out <- NULL
for(i in 1:5){
  thisSum <- summary(outList[[i]])
  mse <- thisSum$Table[,2]
  relMse <- thisSum$Table[,2]/thisSum$Table[11,2]
  out <- rbind(out, relMse[(1:12)[-11]])
}

#==================
# plot results
#==================
# labels for various learners
lab <- c("Super Learner","Discrete Super Learner",
         "HAL","Random Forest","GAM","GBM","Radial SVM",
         "Regression Tree","Polynomial MARS","Kernel Regression",
         "Stepwise GLM")
colnames(out) <- lab
ord <- order(colMeans(out))

# to bold the label for HAL
halLab <- which(lab[ord]=="HAL")
betterLab <- c(lab[ord][1:length(lab) < halLab])
worseLab <- c(lab[ord][1:length(lab) > halLab])
fullLab <- c(betterLab, expression(bold("HAL")),worseLab)
K <- length(lab)

# make the plot
par(mar=c(3.1,9.1,1.1,0.5),mgp=c(2.1,0.5,0))
plot(0,0,pch="",bty="n",xlab="Relative MSE",ylab="",
     xlim=range(out),ylim=c(1,K),yaxt="n",log="x")
axis(side=2,at=K:1,labels=fullLab,las=2)
abline(v=1,lty=3)
for(i in 1:K){
  points(x=out[,ord[i]],y=rep(K-i+1,5),col="gray50",pch=21)
  points(x=exp(mean(log(out[,ord[i]]))), y=K-i+1,col=1,pch=3)
}




