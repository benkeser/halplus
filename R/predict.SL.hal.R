#' Quick pass-through.
predict.SL.hal = function(object,
                          newdata,
                          bigDesign = FALSE,
                          verbose = TRUE,
                          chunks = 1000,
                          s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se),
                          ...) {
  predict(object, newdata, bigDesign, verbose, chunks, s, ...)
}