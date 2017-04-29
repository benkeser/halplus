########################################################################
# other Super Learner functions
########################################################################
SL.caret.rf <- function(...,method="rf",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.gbm <- function(...,method="gbm",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.nnet <- function(...,method="nnet",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.svmRadial  <- function(...,method="svmRadial",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.logreg <- function(...,method="logreg",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.bartMachine <- function(...,method="bartMachine",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}

SL.caret.gamSpline <- function(...,method="gamSpline",tuneLength=3){
    SL.caret1(...,method=method, tuneLength=tuneLength)
}



SL.glmnet.int <-
    function (Y, X, newX, family, obsWeights, id, alpha = 1, nfolds = ifelse(length(Y)<=100, 20, 10),
              nlambda = 100, useMin = TRUE, ...)
    {
        SuperLearner:::.SL.require("glmnet")
        if (!is.matrix(X)) {
            X <- model.matrix(~-1 + .^2, X)
            newX <- model.matrix(~-1 + .^2, newX)
        }
        fitCV <- glmnet::cv.glmnet(x = X, y = Y, weights = obsWeights,
                                   lambda = NULL, type.measure = "deviance", nfolds = nfolds,
                                   family = family$family, alpha = alpha, nlambda = nlambda)
        pred <- predict(fitCV$glmnet.fit, newx = newX, s = ifelse(useMin,
                                                                  fitCV$lambda.min, fitCV$lambda.1se), type = "response")
        fit <- list(object = fitCV, useMin = useMin)
        class(fit) <- "SL.glmnet.int"
        out <- list(pred = pred, fit = fit)
        return(out)
    }

predict.SL.glmnet.int <- function (object, newdata, ...)
{
    if (!is.matrix(newdata)) {
        newdata <- model.matrix(~-1 + .^2, newdata)
    }
    pred <- predict(object$object$glmnet.fit, newx = newdata,
                    s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se),
                    type = "response")
    return(pred)
}

# kernel regression
SL.npreg <- function (Y, X, newX, family, obsWeights,
                      rangeThresh=1e-7, ...)
{
    if(abs(diff(range(Y))) <= rangeThresh){
        thisMod <- glm(Y ~ 1, data=X)
    }else{
        bw <- npregbw(as.formula(paste("Y ~", paste(names(X),collapse="+"))), data=X,
                      ftol=0.01,tol=0.01, remin=FALSE)
        # fit the kernel regression
        thisMod <- npreg(bw)
    }

    pred <- predict(thisMod, newdata=newX)
    fit <- list(object = thisMod)
    class(fit) <- "SL.npreg"
    out <- list(pred = pred, fit = fit)
    return(out)
}

predict.SL.npreg <- function(object, newdata, ...){
    pred <- predict(object=object$object, newdata=newdata)
    pred
}

SL.caret1 <- function (Y, X, newX, family, obsWeights, method = "rf", tuneLength = 3,
                       trControl = caret::trainControl(method = "cv", number = ifelse(length(Y)<=100, 20, 10), verboseIter = FALSE,
                                                       classProbs=ifelse(length(unique(Y))<=2,TRUE,FALSE)),
                       metric,...)
{
    if (length(unique(Y))>2){
        if(is.matrix(Y)) Y <- as.numeric(Y)
        metric <- "RMSE"
        if(method=="gbm" | method=="bartMachine"){
            suppressWarnings(
                # pass verbose==FALSE directly to train (verboseIter doesn't
                # suppress output)
                fit.train <- caret::train(x = X, y = Y, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl,verbose=FALSE)
            )
        }else if(method=="nnet"){
            suppressWarnings(
                fit.train <- caret::train(x = X, y = Y, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl,trace=FALSE)
            )
        }else{
            suppressWarnings(
                fit.train <- caret::train(x = X, y = Y, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl)
            )
        }
        pred <- predict(fit.train, newdata = newX, type = "raw")
    }
    if (length(unique(Y))<=2) {
        metric <- "Accuracy"
        Y.f <- as.factor(Y)
        levels(Y.f) <- c("A0", "A1")
        if(method=="gbm" | method=="bartMachine"){
            suppressWarnings(
                # pass verbose==FALSE directly to train (verboseIter doesn't
                # suppress output)
                fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl, verbose = FALSE)
            )
        }else if(method=="nnet"){
            suppressWarnings(
                # pass trace==FALSE directly to train (verboseIter doesn't
                # suppress output)
                fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl,trace=FALSE)
            )
        }else{
            suppressWarnings(
                fit.train <- caret::train(x = X, y = Y.f, weights = obsWeights,
                                          metric = metric, method = method,
                                          tuneLength = tuneLength,
                                          trControl = trControl)
            )
        }
        pred <- predict(fit.train, newdata = newX, type = "prob")[,2]
    }
    fit <- list(object = fit.train)
    out <- list(pred = pred, fit = fit)
    class(out$fit) <- c("SL.caret")
    cat("Done with SL.caret.",method,"\n")
    return(out)
}
