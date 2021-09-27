#' @method coef bolasso
#' @export
coef.bolasso <- function(object, select = c("lambda.min", "lambda.1se"), ...) {
  implement <- attributes(object)$implement
  c <- lapply(
    object,
    function(i) {
      do.call(
        stats::coef,
        translate_coef(implement, i, select[[1]], ...)
      )
    }
  )
  c <- do.call(cbind, c)
  colnames(c) <- paste0("boot", 1:ncol(c))
  Matrix::t(c)
}

#' @method predict bolasso
#' @export
predict.bolasso <- function(object, new.data, select = c("min", "1se"), ...) {
  varnames <- attributes(object)$varnames
  implement <- attributes(object)$implement
  form <- attributes(object)$call$form
  if (!is.null(form)) {
    new.data <- model_matrix(form = form, data = new.data, prediction = TRUE)$x
  } else {
    new.data <- model_matrix(x = new.data, y = 1)$x
  }
  validate_varnames(x = varnames, y = colnames(new.data))
  p <- lapply(
    object,
    function(i) {
      do.call(
        stats::predict,
        translate_predict(implement, i, new.data, select[[1]], ...)
      )
    }
  )
  p <- do.call(cbind, p)
  colnames(p) <- paste0("boot", 1:ncol(p))
  p
}
