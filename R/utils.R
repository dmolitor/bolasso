bootstraps <- function(dat, n) {
  stopifnot(is.matrix(dat) || inherits(dat, "sparseMatrix"),
            is.numeric(n) && n >= 1)
  lapply(1:n, function(i) sort(sample(nrow(dat), replace = TRUE)))
}

#' Extract indices used for each bootstrap replicate
#' 
#' This function extracts the indices used to bootstrap the data
#' for each replicate. This is helpful when doing some additional
#' analysis that requires knowledge of the exact bootstrap samples used.
#' 
#' @param model A model of class \link{bolasso}.
#' @return A named list of length equal to the number of bootstrap replicates
#'   that contains the corresponding indices used to subsample the data.
#' @export
bootstrap_samples <- function(model) {
  stopifnot(inherits(model, "bolasso"))
  attr(model, "indices")
}

formula_lhs <- function(form) {
  form <- tryCatch(
    stats::as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) stop("Formula is missing LHS variable", call. = FALSE)
  deparse(form[[2]], width.cutoff = 500L)
}

formula_rhs <- function(form) {
  form <- tryCatch(
    stats::as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) {
    deparse(form[[2]], width.cutoff = 500L)
  } else {
    deparse(form[[3]], width.cutoff = 500L)
  }
}

last <- function(x) {
  x[[length(x)]]
}

model_matrix <- function(form, data, x = NULL, y = NULL, prediction = FALSE) {
  validate_data(form = form, data = data, x = x, y = y)
  if (!is.null(x) && !is.null(y)) return(list(x = x, y = y))
  form_rhs <- stats::as.formula(paste("~", formula_rhs(form), "- 1"))
  form_lhs <- formula_lhs(form)
  x <- if (form_lhs %in% colnames(data)) {
    Matrix::sparse.model.matrix(
      form_rhs,
      data = data[, !colnames(data) == form_lhs, drop = FALSE]
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

qnt_threshold <- function(dat, threshold) {
  dat[, apply(
    dat,
    2,
    function(x) same_sign(stats::quantile(x, c((1-threshold)/2, (1+threshold)/2)))
  )]
}

same_sign <- function(x) {
  all(x < 0) || all(x > 0)
}

#' Bolasso-selected Variables
#'
#' Identifies covariates that are selected by the Bolasso algorithm at
#' the user-defined threshold. There are two variable selection criterion
#' to choose between; Variable Inclusion Probability ("vip") introduced in
#' the original Bolasso paper (Bach, 2008) and further developed by Bunea et
#' al. (2011), and the Quantile ("qnt") approach proposed by Abram et
#' al. (2016). The desired threshold value is 1 - alpha, where alpha is some
#' (typically small) significance level.
#' 
#' This function returns either a [tibble::tibble] of selected covariates
#' and their corresponding coefficients across all bootstrap replicates, or
#' a vector of selected covariate names.
#'
#' @param object An object of class \link{bolasso}.
#' @param threshold A numeric between 0 and 1, specifying the variable
#'   selection threshold to use.
#' @param method The variable selection method to use. The two valid options
#'   are `c("vip", "qnt")`. The default `"vip"` and is the method described in
#'   the original Bach (2008) and complementary Bunea et al. (2011) works. The
#'   `"qnt"` method is the method proposed by Abram et al. (2016).
#' @param var_names_only A boolean value. When `var_names_only = FALSE`
#'   (the default value) this function will return a [tibble::tibble] of
#'   selected covariates and their corresponding coefficients across all
#'   bootstrap replicates. When `var_names_only == TRUE`, it will return
#'   a vector containing all selected covariate names.
#' @param ... Additional arguments to pass to \code{\link{coef}} on
#'   objects with class \link{bolasso} or `bolass_fast`.
#'
#' @return A tibble with each selected variable and its respective coefficient
#'   for each bootstrap replicate OR a vector of the names of all selected
#'   variables.
#'
#' @seealso [glmnet::coef.glmnet()] and `gamlr:::coef.gamlr` for details
#'   on additional arguments to pass to `...`.
#'
#' @export
#' 
#' @aliases selected_vars
selected_variables <- function(
  object,
  threshold = 0.95,
  method = c("vip", "qnt"),
  var_names_only = FALSE,
  ...
) {
  stopifnot(inherits(object, "bolasso"),
            is.numeric(threshold))
  method <- match.arg(method, choices = c("vip", "qnt"))
  model_coefs <- stats::coef(object = object, ...)
  model_coefs <- switch(
    method,
    vip = vip_threshold(model_coefs, threshold = threshold),
    qnt = qnt_threshold(model_coefs, threshold = threshold)
  )
  model_coefs <- tidy_selected_vars(model_coefs)
  model_coefs <- tidy_intercept(model_coefs)
  model_coefs <- model_coefs[, setdiff(names(model_coefs), "Intercept"), drop = FALSE]
  if (var_names_only) return(setdiff(names(model_coefs), "id"))
  return(model_coefs)
}

#' @export
selected_vars <- selected_variables

vip_threshold <- function(dat, threshold) {
  dat[, (diff(dat@p) / dat@Dim[[1]]) >= threshold, drop = FALSE]
}

selected_vars_grid <- function(object, grid, ...) {
  stopifnot(inherits(object, "bolasso"))
  model_coefs <- stats::coef(object = object, ...)[, -1, drop = FALSE]
  all_cols <- colnames(model_coefs)
  grid_by_method <- lapply(
    c("vip", "qnt"),
    function(method) {
      method_by_threshold <- lapply(
        grid,
        function(threshold) {
          model_coefs <- switch (
            method,
            vip = vip_threshold(model_coefs, threshold = threshold),
            qnt = qnt_threshold(model_coefs, threshold = threshold)
          )
          tibble::tibble(
            "covariate" = all_cols,
            "selected" =  all_cols %in% colnames(model_coefs),
            "threshold" = threshold,
            "method" = toupper(method)
          )
        }
      )
      method_df <- do.call(rbind, method_by_threshold)
    }
  )
  grid_df <- do.call(rbind, grid_by_method)
  grid_df$predictor_id <- match(
    grid_df[["covariate"]],
    unique(grid_df[["covariate"]])
  )
  return(grid_df)
}

#' Calculate each covariate's smallest variable selection threshold
#' 
#' There are two methods of variable selection for covariates. The first
#' is the Variable Inclusion Probability (VIP) introduced by Bach (2008)
#' and generalized by Bunea et al (2011). The second is the Quantile
#' confidence interval (QNT) proposed by Abram et al (2016). For a given level
#' of significance alpha, each method selects covariates for the given
#' threshold = 1 - alpha. The higher the threshold (lower alpha), the more
#' stringent the variable selection criterion.
#' 
#' This function returns a tibble that, for each covariate, returns the
#' largest threshold (equivalently smallest alpha) at which it would be
#' selected for both the VIP and the QNT methods. Consequently the number
#' of rows in the returned tibble is 2*p where p is the number of covariates
#' included in the model.
#' 
#' @param object An object of class \link{bolasso} or `bolasso_fast`.
#' @param grid A vector of numbers between 0 and 1 (inclusive) specifying
#'   the grid of threshold values to calculate variable inclusion criterion 
#'   at. Defaults to `seq(0, 1, by = 0.01)`.
#' @param ... Additional parameters to pass to \code{\link{coef}} on objects of
#'   class \link{bolasso} and `bolasso_fast`.
#' 
#' @return A tibble with dimension (2*p)x5 where p is the number of covariates.
#' @export
selection_thresholds <- function(object, grid = seq(0, 1, by = 0.01),  ...) {
  selection_grid <- selected_vars_grid(object, grid = grid, ...)
  thresholds <- lapply(
    split(selection_grid, list(selection_grid$covariate, selection_grid$method)),
    function(group) {
      max_threshold <- suppressWarnings(max(group$threshold[group$selected]))
      if (is.infinite(max_threshold)) max_threshold <- min(grid)
      tibble::tibble(
        covariate = group$covariate[1],
        method = group$method[1],
        threshold = max_threshold,
        alpha = 1 - max_threshold,
        covariate_id = group$predictor_id[1]
      )
    }
  )
  thresholds <- do.call(rbind, thresholds) |> tibble::remove_rownames()
  return(thresholds)
}
