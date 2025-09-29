bolasso_fast.fit <- function(x, y, n.boot, ...) {
  full_fit <- glmnet::cv.glmnet(x = x, y = y, ...)
  global_lambda <- full_fit$lambda.min
  global_lambda_seq <- full_fit$lambda[full_fit$lambda >= global_lambda]
  global_lambda_n <- length(global_lambda_seq)
  folds <- bootstraps(dat = x, n = n.boot)
  pb <- progressr::progressor(along = folds)
  models <- future.apply::future_lapply(
    folds,
    function(i) {
      lasso_args <- list(x = x[i, ], y = y[i], lambda = global_lambda_seq, ...)
      g <- do.call(
        what = glmnet::glmnet,
        args = lasso_args
      )
      pb()
      g
    },
    future.seed = TRUE,
    future.packages = c("Matrix", "glmnet", "progressr")
  )
  attr(models, "indices") <- folds
  return(models)
}

#' @method coef bolasso_fast
#' @export
coef.bolasso_fast <- function(object, ...) {
  global_lambda <- last(object[[1]]$lambda)
  coefs <- lapply(
    object,
    function(i) {
      model_coefs <- stats::coef(i, s = global_lambda, ...)
      return(model_coefs)
    }
  )
  coefs <- do.call(cbind, coefs)
  colnames(coefs) <- paste0("boot", 1:ncol(coefs))
  Matrix::t(coefs)
}

#' @method predict bolasso_fast
#' @export
predict.bolasso_fast <- function(object, new.data, ...) {
  global_lambda <- last(object[[1]]$lambda)
  varnames <- attributes(object)$varnames
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
      stats::predict(
        object = i,
        newx = new.data,
        s = global_lambda,
        ...
      )
    }
  )
  p <- do.call(cbind, p)
  colnames(p) <- paste0("boot", 1:ncol(p))
  p
}
