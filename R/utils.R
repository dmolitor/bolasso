#' @importFrom Rdpack reprompt

bootstraps <- function(dat, n) {
  stopifnot(is.matrix(dat) || inherits(dat, "sparseMatrix"),
            is.numeric(n) && n >= 1)
  lapply(1:n, function(i) sort(sample(nrow(dat), replace = TRUE)))
}

formula_lhs <- function(form) {
  form <- tryCatch(
    stats::as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) stop("Formula is missing LHS variable", call. = FALSE)
  deparse(form[[2]])
}

formula_rhs <- function(form) {
  form <- tryCatch(
    stats::as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) {
    deparse(form[[2]])
  } else {
    deparse(form[[3]])
  }
}

model_matrix <- function(form, data, x = NULL, y = NULL, prediction = FALSE) {
  validate_data(form = form, data = data, x = x, y = y)
  if (!is.null(x) && !is.null(y)) return(list(x = x, y = y))
  form_rhs <- stats::as.formula(paste("~", formula_rhs(form), "- 1"))
  form_lhs <- formula_lhs(form)
  x <- if (form_lhs %in% colnames(data)) {
    Matrix::sparse.model.matrix(
      form_rhs,
      data = data[, !colnames(data) == form_lhs]
    )
  } else {
    Matrix::sparse.model.matrix(
      form_rhs,
      data = data
    )
  }
  if (prediction) {
    y <- NULL
  } else {
    y <- data[[form_lhs]]
  }
  list(x = x, y = y)
}

#' Bolasso-selected Variables
#'
#' Identifies independent variables that are selected by the Bolasso algorithm at
#' least the fraction of the time specified by the user-defined threshold. The
#' typical value for this threshold is 0.9 and typically shouldn't be lower
#' than that.
#'
#' @param object An object of class \link{bolasso}.
#' @param threshold A numeric between 0 and 1, specifying the fraction of
#'   bootstrap replicates for which Lasso must select a variable for it to be
#'   considered a selected variable.
#' @param summarise A Boolean indicator where `FALSE` indicates returning the
#'   full set of coefficients at the selected variable/bootstrap replicate level
#'   and `TRUE` indicates taking the average of each variable's coefficient
#'   across bootstrap replicates. The default value is `TRUE` as it's more
#'   efficient and interpretable.
#' @param ... Additional arguments to pass to \code{\link{predict}} on
#'   objects with class \link{cv.glmnet} or \link{cv.gamlr}.
#'
#' @return A tibble with each selected variable and its respective coefficient
#'   for each bootstrap replicate.
#'
#' @seealso [glmnet::predict.glmnet()] and [gamlr:::predict.gamlr()] for details
#'   on additional arguments to pass to `...`.
#'
#' @export
selected_vars <- function(object, threshold = 0.9, summarise = TRUE, ...) {
  stopifnot(inherits(object, "bolasso"),
            is.numeric(threshold))
  model_coefs <- coef.bolasso(object = object, ...)
  model_coefs <- sparsity_threshold(model_coefs, threshold = threshold)
  model_coefs <- tidy_selected_vars(model_coefs)
  model_coefs <- lapply(model_coefs, tidy_intercept)
  if (summarise) {
    tibble::enframe(
      colMeans(model_coefs$x_summarised[, -1], na.rm = TRUE),
      name = "variable",
      value = "mean_coef"
    )
  } else {
    model_coefs$x
  }
}

sparsity_threshold <- function(dat, threshold) {
  dat[, (diff(dat@p) / dat@Dim[[1]]) >= threshold, drop = FALSE]
}
