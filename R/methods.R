#' @method coef bolasso
#' @export
coef.bolasso <- function(object, select = c("lambda.min", "lambda.1se", "min", "1se"), ...) {
  select <- match.arg(select)
  implement <- attributes(object)$implement
  coefs <- lapply(
    object,
    function(i) {
      do.call(
        stats::coef,
        translate_coef(implement, i, select, ...)
      )
    }
  )
  coefs <- do.call(cbind, coefs)
  colnames(coefs) <- paste0("boot", 1:ncol(coefs))
  Matrix::t(coefs)
}

#' @method predict bolasso
#' @export
predict.bolasso <- function(object, new.data, select = c("lambda.min", "lambda.1se", "min", "1se"), ...) {
  select <- match.arg(select)
  varnames <- attributes(object)$varnames
  implement <- attributes(object)$implement
  form <- eval(attributes(object)$call$formula)
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
        translate_predict(implement, i, new.data, select, ...)
      )
    }
  )
  p <- do.call(cbind, p)
  colnames(p) <- paste0("boot", 1:ncol(p))
  p
}

#' @method print bolasso
#' @export
print.bolasso <- function(x, ...) {
  nboot <- attributes(x)$nboot
  npred <- length(attributes(x)$varnames)
  nobs <- attributes(x)$dimensions[[1]]
  cat("-----",
      paste0(nboot, "-fold"),
      "bootstrapped model -----\n")
  cat(
    "Model matrix dimensions:\n",
    "  -", npred, "Predictors\n",
    "  -", nobs, "Observations\n\n"
  )
  return(invisible(x))
}
