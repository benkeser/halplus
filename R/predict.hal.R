#' @importFrom plyr alply
#' @importFrom stats predict
#' @export
predict.hal <-
  function(object,
           newdata,
           bigDesign = FALSE,
           verbose = TRUE,
           chunks = 1000,
           s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se),
           offset = NULL,
           ...)
  {
    if (!object$sparseMat) {
      d <- ncol(object$X)
      # if you want to get predictions all at once (smaller newdata)
      if (bigDesign) {
        uniList <- plyr::alply(matrix(1:ncol(object$X)), 1, function(x) {
          myX <-
            matrix(newdata[, x],
                   ncol = length(object$X[, x]),
                   nrow = length(newdata[, x])) -
            matrix(
              object$X[, x],
              ncol = length(object$X[, x]),
              nrow = length(newdata[, x]),
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
          designNewX <- initX[, !object$dup]
        } else {
          initX <- Reduce("cbind", uniList)
          designNewX <- initX[, !object$dup]
        }
        # get predictions
        pred <- predict(
          object$object$glmnet.fit,
          newx = designNewX,
          s = s,
          type = "response",
          offset = offset
        )

      } else {
        # get row by row predictions, so you never have to store a big design matrix
        # for newdata
        pred <- apply(as.matrix(newdata), 1, function(i) {
          uniList <- plyr::alply(matrix(1:ncol(object$X)), 1, function(x) {
            myX <- matrix(i[x],
                          ncol = length(object$X[, x]),
                          nrow = length(i[x])) -
              matrix(
                object$X[, x],
                ncol = length(object$X[, x]),
                nrow = length(i[x]),
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
              cbind(Reduce("cbind", uniList),
                    Reduce("cbind", highDList))
            designNewX <- initX[!object$dup]
          } else{
            initX <- Reduce("cbind", uniList)
            designNewX <- initX[!object$dup]
          }
          # get predictions
          thispred <-
            predict(
              object$object$glmnet.fit,
              newx = matrix(designNewX, nrow = 1),
              s = s,
              type = "response",
              offset = offset
            )
          thispred
        })
      }
    } else {
      # all predictions at once
      if (bigDesign) {
        pred <- doPred(object = object,
                       newdata = newdata,
                       verbose = verbose)
      } else {
        nNew <- length(newdata[, 1])
        nChunks <- floor(nNew / chunks) + ifelse(nNew %% chunks == 0, 0, 1)
        pred <- rep(NA, length(nNew))
        for (i in 1:nChunks) {
          minC <- (i - 1) * chunks + 1
          maxC <- ifelse(i == nChunks, nNew, i * chunks)
          pred[minC:maxC] <-
            doPred(
              object = object,
              s = s,
              newdata = newdata[minC:maxC, ],
              verbose = verbose,
              offset = offset
            )
        }
      }
    }
    return(as.numeric(pred))
  }
