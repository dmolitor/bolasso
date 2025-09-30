#' @import Matrix

new_bolasso <- function(
  x,
  indices,
  implement,
  varnames,
  nboot,
  dimensions,
  fun.call,
  fast = FALSE
) {
  stopifnot(
    is.list(x),
    all(
      vapply(
        x,
        function(i) {
          ifelse(
            fast,
            inherits(i, "glmnet"),
            inherits(i, "cv.gamlr") || inherits(i, "cv.glmnet")
          )
        },
        logical(1)
      )
    ),
    implement %in% c("gamlr", "glmnet")
  )
  class(x) <- if (fast) c("bolasso_fast", "bolasso") else "bolasso"
  attr(x, "indices") <- stats::setNames(indices, paste0("boot", 1:length(indices)))
  attr(x, "implement") <- implement
  attr(x, "call") <- fun.call
  attr(x, "varnames") <- varnames
  attr(x, "nboot") <- nboot
  attr(x, "dimensions") <- dimensions
  x
}

bolasso.fit <- function(x, y, n.boot, implement, ...) {
  folds <- bootstraps(dat = x, n = n.boot)
  pb <- progressr::progressor(along = folds)
  models <- future.apply::future_lapply(
    folds,
    function(i) {
      lasso_args <- list(x = x[i, , drop = FALSE], y = y[i], ...)
      g <- do.call(
        what = translate_lasso(implement),
        args = lasso_args
      )
      pb()
      g
    },
    future.seed = TRUE,
    future.packages = c("Matrix", implement, "progressr")
  )
  attr(models, "indices") <- folds
  return(models)
}


#' Bootsrap-enhanced Lasso
#'
#' This function implements model-consistent Lasso estimation through the
#' bootstrap. It supports parallel processing by way of the
#' \href{https://CRAN.R-project.org/package=future}{future}
#' package, allowing the user to flexibly specify many parallelization methods.
#' This method was developed as a variable-selection algorithm, but this package
#' also supports making ensemble predictions on new data using the bagged Lasso
#' models.
#'
#' @param formula An optional object of class \link{formula} (or one that can be
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
#' @param fast A boolean. Whether or not to fit a "fast" bootstrap procedure.
#'   If `fast == TRUE`, `bolasso` will fit [glmnet::cv.glmnet] on the entire
#'   dataset. It will then fit all bootstrapped models with the value of lambda
#'   (regularization parameter) that minimized cross-validation loss in the
#'   full model. If `fast == FALSE` (the default), `bolasso` will use
#'   cross-validation to find the optimal lambda for each bootstrap model.
#' @param ... Additional parameters to pass to either
#'   `glmnet::cv.glmnet` or `gamlr::cv.gamlr`.
#'
#' @seealso [glmnet::cv.glmnet] and [gamlr::cv.gamlr] for full details on the
#'   respective implementations and arguments that can be passed to `...`.
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
#'   nfolds = 5
#' )
#'
#' # Retrieve a tidy tibble of bootstrap coefficients for each covariate
#' tidy(bolasso_form)
#' 
#' # Extract selected variables
#' selected_variables(bolasso_form, threshold = 0.9, select = "lambda.min")
#'
#' # Bagged ensemble prediction on test data
#' predict(bolasso_form,
#'         new.data = mtcars_test,
#'         select = "lambda.min")
#'
#' ## Alternate Matrix Interface
#'
#' # Train model
#' set.seed(123)
#' bolasso_mat <- bolasso(
#'   x = model.matrix(mpg ~ . - 1, mtcars_train),
#'   y = mtcars_train[, 1],
#'   data = mtcars_train,
#'   n.boot = 20,
#'   nfolds = 5
#' )
#'
#' # Bagged ensemble prediction on test data
#' predict(bolasso_mat,
#'         new.data = model.matrix(mpg ~ . - 1, mtcars_test),
#'         select = "lambda.min")
#' 
#' # Extract the indices of the bootstrap replicates
#' bootstrap_samples(bolasso_mat)
#'
#' @return An object of class `bolasso`. This object is a list of length
#' `n.boot` of `cv.glmnet` or `cv.gamlr` objects.
#'
#' @export
bolasso <- function(
  formula,
  data,
  n.boot = 100,
  progress = TRUE,
  implement = c("glmnet", "gamlr"),
  x = NULL,
  y = NULL,
  fast = FALSE,
  ...
) {
  implement <- match.arg(implement)
  if (fast && implement == "gamlr") {
    message("Fast mode isn't compatible with `gamlr`; defaulting to `glmnet`")
    implement <- "glmnet"
  }
  data <- model_matrix(form = formula, data = data, x = x, y = y)
  if (progress) {
    if (fast) {
      progressr::with_progress(
        models <- bolasso_fast.fit(
          x = data$x,
          y = data$y,
          n.boot = n.boot,
          ...
        )
      )
    } else {
      progressr::with_progress(
        models <- bolasso.fit(
          x = data$x,
          y = data$y,
          n.boot = n.boot,
          implement = implement,
          ...
        )
      )
    }
  } else {
    if (fast) {
      models <- bolasso_fast.fit(
        x = data$x,
        y = data$y,
        n.boot = n.boot,
        ...
      )
    } else {
      models <- bolasso.fit(
        x = data$x,
        y = data$y,
        n.boot = n.boot,
        implement = implement,
        ...
      )
    }
  }
  bootstrap_indices <- attr(models, "indices")
  attr(models, "indices") <- NULL
  new_bolasso(
    models,
    indices = bootstrap_indices,
    implement = implement,
    varnames = colnames(data$x),
    nboot = n.boot,
    dimensions = dim(data$x),
    fun.call = match.call(),
    fast = fast
  )
}
