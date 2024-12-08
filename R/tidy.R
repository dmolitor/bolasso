#' @importFrom generics tidy
#' @export
generics::tidy

tidy_intercept <- function(dat) {
  if (!any(colnames(dat) %in% c("intercept", "(Intercept)"))) return(dat)
  int_col <- colnames(dat) %in% c("intercept", "(Intercept)")
  stopifnot(sum(int_col) == 1)
  colnames(dat)[int_col] <- "Intercept"
  dat
}

tidy_selected_vars <- function(object) {
  object <- tibble::as_tibble(
    as.data.frame(
      as.matrix(object)
    ),
    rownames = "id"
  )
  return(object)
}

#' Tidy a bolasso object
#' 
#' @method tidy bolasso
#'
#' @param x A `bolasso` object.
#' @param select One of "min", "1se", "lambda.min", "lambda.1se".
#'   Both "min" and "lambda.min" are equivalent and are the lambda value
#'   that minimizes cv MSE. Similarly "1se" and "lambda.1se" are equivalent
#'   and refer to the lambda that achieves the most regularization and is
#'   within 1se of the minimal cv MSE.
#' @param ... Additional arguments to pass directly to `coef.bolasso`.
#' @return A tidy [tibble::tibble()] summarizing bootstrap-level
#'   coefficients for each covariate.
#'
#' @export
tidy.bolasso <- function(x, select = c("lambda.min", "lambda.1se", "min", "1se"), ...) {
  select <- match.arg(select)
  model_coefs <- stats::coef(x, select = select, ...)
  model_coefs <- tibble::as_tibble(
    as.data.frame(
      as.matrix(model_coefs)
    ),
    rownames = "id"
  )
  model_coefs <- tidy_intercept(model_coefs)
  return(model_coefs)
}
