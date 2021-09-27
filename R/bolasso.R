new_bolasso <- function(x, implement, varnames) {
  stopifnot(
    is.list(x),
    all(
      vapply(
        x,
        function(i) inherits(i, "cv.gamlr") || inherits(i, "cv.glmnet"),
        logical(1)
      )
    ),
    implement %in% c("gamlr", "glmnet")
  )
  class(x) <- "bolasso"
  attr(x, "implement") <- implement
  attr(x, "call") <- sys.call(1L)
  attr(x, "varnames") <- varnames
  x
}

bolasso.fit <- function(x, y, n.boot, implement, ...) {
  folds <- bootstraps(dat = x, n = n.boot)
  pb <- progressr::progressor(along = folds)
  future.apply::future_lapply(
    folds,
    function(i) {
      lasso_args <- list(x = x[i, ], y = y[i], ...)
      g <- do.call(
        what = translate_lasso(implement),
        args = lasso_args
      )
      pb()
      g
    },
    future.seed = TRUE
  )
}

#' Bootsrap-enhanced Lasso
#'
#' This function implements model-consistent Lasso estimation through the
#' bootstrap. It supports parallel processing by way of the
#' \href{https://cran.r-project.org/web/packages/future/index.html}{future}
#' package, allowing the user to flexibly specify many parallelization methods.
#' This method was developed as a variable-selection algorithm, but this package
#' also supports making ensemble predictions on new data using the bagged Lasso
#' models.
#'
#' @param form An optional object of class \link{formula} (or one that can be
#'   coerced to that class): a symbolic description of the model to be fitted.
#'   Can be omitted when `x` and `y` are non-missing.
#' @param data An optional object of class \link{data.frame} that contains the
#'   modeling variables referenced in `form`. Can be omitted when `x` and `y`
#'   are non-missing.
#' @param n.boot An integer specifying the number of bootstrap replicates.
#' @param progress A boolean indicating whether to display progress across
#'   bootstrap folds.
#' @param implement A character; either 'glmnet' or 'gamlr', specifying which
#'   Lasso implementation to utilize. For specific modeling details, see
#'   `glmnet::cv.glmnet` or `gamlr::cv.gamlr`.
#' @param x An optional predictor matrix in lieu of `form` and `data`.
#' @param y An optional response vector in lieu of `form` and `data`.
#' @param ... Additional parameters to pass to either
#'   `glmnet::cv.glmnet` or `gamlr::cv.gamlr`.
#'
#' @seealso [glmnet::cv.glmnet] and [gamlr::cv.gamlr] for full details on the
#'   respective implementations and arguments that can be passed to `...`.
#'
#' @references
#' \insertRef{DBLP:journals/corr/abs-0804-1302}{bolasso}
#'
#' @examples
#' mtcars[, c(2, 10:11)] <- lapply(mtcars[, c(2, 10:11)], as.factor)
#' idx <- sample(nrow(mtcars), 22)
#' mtcars_train <- mtcars[idx, ]
#' mtcars_test <- mtcars[-idx, ]
#'
#' ## Formula Interface
#'
#' # Train model
#' set.seed(123)
#' bolasso_form <- bolasso(
#'   form = mpg ~ .,
#'   data = mtcars_train,
#'   n.boot = 20,
#'   nfolds = 5,
#'   implement = "glmnet"
#' )
#'
#' # Extract selected variables
#' selected_vars(bolasso_form, threshold = 0.9, select = "lambda.min")
#'
#' # Bagged ensemble prediction on test data
#' predict(bolasso_form,
#'         new.data = mtcars_test,
#'         select = "lambda.min")
#'
#' ## Alternal Matrix Interface
#'
#' # Train model
#' set.seed(123)
#' bolasso_mat <- bolasso(
#'   x = model.matrix(mpg ~ . - 1, mtcars_train),
#'   y = mtcars_train[, 1],
#'   data = mtcars_train,
#'   n.boot = 20,
#'   nfolds = 5,
#'   implement = "glmnet"
#' )
#'
#' # Extract selected variables
#' selected_vars(bolasso_mat, threshold = 0.9, select = "lambda.min")
#'
#' # Bagged ensemble prediction on test data
#' predict(bolasso_mat,
#'         new.data = model.matrix(mpg ~ . - 1, mtcars_test),
#'         select = "lambda.min")
#'
#' @return An object of class `bolasso`. This object is a list of length
#' `n.boot` of `cv.glmnet` or `cv.gamlr` objects.
#'
#' @export
bolasso <- function(form,
                    data,
                    n.boot = 100,
                    progress = TRUE,
                    implement = "glmnet",
                    x = NULL,
                    y = NULL,
                    ...) {
  data <- model_matrix(form = form, data = data, x = x, y = y)
  if (progress) {
    progressr::with_progress(
      models <- bolasso.fit(x = data$x,
                            y = data$y,
                            n.boot = n.boot,
                            implement = implement,
                            ...)
    )
  } else {
    models <- bolasso.fit(x = data$x,
                          y = data$y,
                          n.boot = n.boot,
                          implement = implement,
                          ...)
  }
  new_bolasso(models, implement = implement, varnames = colnames(data$x))
}
