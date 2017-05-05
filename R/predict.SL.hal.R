#' Quick pass-through.
#' @export
predict.SL.hal = function(object,
                          newdata,
                          bigDesign = FALSE,
                          verbose = TRUE,
                          chunks = 1000,
                          s = ifelse(object$useMin, object$object$lambda.min, object$object$lambda.1se),
                          ...) {
  pred <- predict(object, newdata, bigDesign, verbose, chunks, s, ...)
}
