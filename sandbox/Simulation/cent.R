#! /usr/bin/env Rscript

# get environment variables
MYSCRATCH <- Sys.getenv('MYSCRATCH')
RESULTDIR <- Sys.getenv('RESULTDIR')
STEPSIZE <- as.numeric(Sys.getenv('STEPSIZE'))
TASKID <- as.numeric(Sys.getenv('SLURM_ARRAY_TASK_ID'))

# set defaults if nothing comes from environment variables
MYSCRATCH[is.na(MYSCRATCH)] <- '.'
RESULTDIR[is.na(RESULTDIR)] <- '.'
STEPSIZE[is.na(STEPSIZE)] <- 1
TASKID[is.na(TASKID)] <- 0

# get command lines arguments
args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 1){
  stop("Not enough arguments. Please use args 'listsize', 'prepare', 'run <itemsize>' or 'merge'")
}

nsim <- 12
ns <- c(500,1000,2000)
bigB <- 100
# 
# simulation parameters
parm <- expand.grid(sim=1:nsim,
                    n=ns,
                    seed=1:bigB)

# # initial results
load("~/svnnet/out/allOut_noSL.RData")
redo <- which(is.na(allOut[,4]))
parm <- parm[redo,]

# allf <- list.files("~/svnnet/out")
# parm <- NULL
# for(n in ns){
#   for(seed in 1:bigB){
#     for(sim in 1:nsim){
#       ind <- grepl(paste0("FAST_n=",n,"_seed=",seed,"_sim=",sim), allf)
#       if(!any(ind)) parm <- rbind(parm, c(sim,n,seed))
#     }
#   }
# }
# parm <- data.frame(parm); colnames(parm) <- c("sim","n","seed")
# save(parm, file="~/svnnet/remainSims.RData")

#load("~/svnnet/remainSims.RData")
# source in functions 
source("~/svnnet/myFunctions.R")
source("~/svnnet/halFunctions.R")
source("~/svnnet/makeData.R")

# source("C:/Users/David Benkeser/Dropbox/Berkeley/svnnet/myFunctions.R")

# get the list size #########
if (args[1] == 'listsize') {
  cat(nrow(parm))
}
# execute prepare job ##################
if (args[1] == 'prepare') {
  # for(n in ns){
  #   for(seed in 1:bigB){
  #     for(sim in 1:nsim){
  #       set.seed(seed)
  #       dat <- do.call(paste0("makeData",sim), args=list(n=n))
  #       save(dat, file=paste0("~/svnnet/scratch/inFile_n=",n,"_seed=",seed,"_sim=",sim,".RData"))
  #     }
  #   }
  #  print(paste0('done with n= ',n,' files'))
  # }
  print(paste0('initial datasets saved to: ~/svnnet/scratch/inFile ... .RData'))
}
# execute parallel job #################################################
if (args[1] == 'run') {
  if (length(args) < 2) {
    stop("Not enough arguments. 'run' needs a second argument 'id'")
  }
  id <- as.numeric(args[2])
  print(paste(Sys.time(), "arrid:" , id, "TASKID:",
              TASKID, "STEPSIZE:", STEPSIZE))
  for (i in (id+TASKID):(id+TASKID+STEPSIZE-1)) {
    print(paste(Sys.time(), "i:" , i))
    print(parm[i,])
    # load data
    load(paste0("~/svnnet/scratch/inFile_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData"))
    
    # set seed
    set.seed(parm[i,3])
    
    # load libraries
    library(np)
    options(np.messages=FALSE)
    library(methods)
    library(Matrix)
    library(data.table)
    library(plyr)
    library(SuperLearner)
    library(foreach)
    library(parallel)
    library(glmnet)
    # library(doParallel)
    algos <- c("SL.hal",
               "SL.caret.rf",
               "SL.caret.gbm",
               "SL.caret.svmRadial",
               "SL.rpartPrune",
               "SL.polymars",
               "SL.npreg")

    X <- data.frame(dat[,colnames(dat)!="y"])
    colnames(X) <- colnames(dat)[colnames(dat)!="y"]
    
    bigDat <- data.frame(do.call(paste0("makeData",parm[i,1]), args=list(n=1e4)))
    if(ncol(bigDat)==2){
      newX <- data.frame(x1=bigDat$x1)
    }else{
      newX <- bigDat[,-which(colnames(bigDat)=="y")]
    }
    risk <- NULL
    for(k in 1:length(algos)){
      fm <- do.call(algos[k], args=list(Y=dat$y, X=X, newX=newX,
                                        family=data.frame(family="gaussian",stringsAsFactors = FALSE),
                                        obsWeights=rep(1,nrow(X))))
      
      risk <- c(risk, 
                mean((bigDat$y - fm$pred)^2))
    }
    r2 <- 1 - risk/mean((bigDat$y - mean(bigDat$y))^2)
    
    out <- c(parm[i,1],parm[i,2],parm[i,3],risk,r2)
    
    if(any(is.na(out))){
      print("Some missing")
      save(fm, file=paste0("~/svnnet/out/fails/failNEW_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData"))
    }
    
    save(out,file=paste0("~/svnnet/out/outnoSL_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData.tmp"))
    file.rename(paste0("~/svnnet/out/outnoSL_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData.tmp"),
                paste0("~/svnnet/out/outnoSL_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData"))
    print(paste0("file saved as : ",  paste0("~/svnnet/out/out_n=",parm[i,2],"_seed=",parm[i,3],"_sim=",parm[i,1],".RData")))
  }
}

# merge job ###########################
if (args[1] == 'merge') {
  nsim <- 12
  ns <- c(500,1000,2000)
  bigB <- 100
  parm <- expand.grid(sim=1:nsim,
                      n=ns,
                      seed=1:bigB)  
  allOut <- NULL
  for(i in 1:nrow(parm)){
    thisF <- 
      tryCatch({
        get(load(paste0("~/svnnet/out/outnoSL_n=",parm$n[i],"_seed=",parm$seed[i],"_sim=",parm$sim[i],".RData")))
      },error=function(e){
        # cat(sim, " ", n, " ", s)
        c(parm$sim[i], parm$n[i], parm$seed[i], rep(NA, 14))
      })
    allOut <- rbind(allOut, thisF)
  }
  save(allOut, file=paste0('~/svnnet/out/allOut_noSL.RData'))
  print(paste0('saved results to: ~/svnnet/out/allOut_noSL.RData'))
}