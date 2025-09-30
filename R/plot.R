#' Plot a `bolasso` object
#' 
#' The method plots coefficient distributions for the covariates included
#' in the `bolasso` model. If there are more than 30 covariates included in
#' the full model, this will plot the 30 covariates with the largest
#' absolute mean coefficient. The user can also plot coefficient distributions
#' for a specified subset of covariates.
#' 
#' @method plot bolasso
#' 
#' @param x An object of class \link{bolasso} or `bolasso_fast`.
#' @param covariates A subset of the covariates to plot. This should be a
#'   vector of covariate names either as strings or bare. E.g.
#'   `covariates = c("var_1", "var_2")` or `covariates = c(var_1, var_2)`.
#'   This argument is optional and is `NULL` by default. In this case it will
#'   plot up to 30 covariates with the largest absolute mean coefficients.
#' @param ... Additional arguments to pass directly to \code{\link{coef}} for
#'   objects of class \link{bolasso} or `bolasso_fast`.
#' @importFrom stats coef reshape
#' @export
plot.bolasso <- function(x, covariates = NULL, ...) {
  id <- covariate <- NULL ## This is so stupid R CMD Check doesn't flip out
  coefs <- tidy(x, ...)
  coefs <- coefs[, setdiff(colnames(coefs), "Intercept"), drop = FALSE]
  if (!is.null(substitute(covariates))) {
    covariates <- substitute(covariates)
    coefs <- subset(coefs, select = c(id, eval(covariates)))
  }
  covar_cols <- setdiff(colnames(coefs), "id")
  if (length(covar_cols) > 30 && is.null(covariates)) {
    covar_col_means <- colMeans(coefs[, covar_cols, drop = FALSE])
    top_30_covars <- covar_col_means[sort(order(abs(covar_col_means), decreasing = TRUE)[1:30])]
    covar_cols <- names(top_30_covars)
    coefs <- coefs[, c("id", covar_cols), drop = FALSE]
  }
  coefs_long <- coefs |>
    transform(id = as.integer(gsub("boot", "", id))) |>
    reshape(
      idvar = "id",
      varying = setdiff(colnames(coefs), "id"),
      v.names = "coef",
      timevar = "covariate",
      times = setdiff(colnames(coefs), "id"),
      direction = "long"
    ) |>
    transform(
      id = paste0("boot", id),
      covariate_id = as.integer(factor(covariate))
    ) |>
    tibble::as_tibble()
  ggplot2::ggplot(coefs_long, ggplot2::aes(x = factor(covariate), y = coef)) +
    ggplot2::geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.5) +
    ggplot2::labs(y = "Coefficient distribution", x = "Covariate") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = "gray90"))
}

#' Plot selected variables from a `bolasso` object.
#' 
#' The method plots coefficient distributions for the selected covariates
#' in the `bolasso` model. If there are more than 30 selected covariates,
#' this will plot the 30 selected covariates with the largest
#' absolute mean coefficient. The user can also plot coefficient distributions
#' for a specified subset of selected covariates.
#' 
#' @param x An object of class \link{bolasso} or `bolasso_fast`.
#' @param threshold A numeric between 0 and 1, specifying the variable
#'   selection threshold to use.
#' @param method The variable selection method to use. The two valid options
#'   are `c("vip", "qnt")`. The default `"vip"` and is the method described in
#'   the original Bach (2008) and complementary Bunea et al. (2011) works. The
#'   `"qnt"` method is the method proposed by Abram et al. (2016).
#' @param covariates A subset of the selected covariates to plot. This should be a
#'   vector of covariate names either as strings or bare. E.g.
#'   `covariates = c("var_1", "var_2")` or `covariates = c(var_1, var_2)`.
#'   This argument is optional and is `NULL` by default. In this case it will
#'   plot up to 30 covariates with the largest absolute mean coefficients.
#' @param ... Additional arguments to pass to \code{\link{coef}} on
#'   objects with class `bolasso` or `bolass_fast`.
#' @export
plot_selected_variables <- function(
  x,
  covariates = NULL,
  threshold = 0.95,
  method = c("vip", "qnt"),
  ...
) {
  id <- coef <- covariate <- NULL ## This is so stupid R CMD Check doesn't flip out
  method <- match.arg(method)
  coefs <- selected_vars(x, threshold = threshold, method = method, ...)
  if (length(names(coefs)) < 2 && names(coefs) == "id") {
    warning("Nothing to plot! There were no selected variables at the threshold of ", threshold, ".")
    return(invisible(NULL))
  }
  if (!is.null(substitute(covariates))) {
    covariates <- substitute(covariates)
    coefs <- subset(coefs, select = c(id, eval(covariates)))
  }
  covar_cols <- setdiff(colnames(coefs), "id")
  if (length(covar_cols) > 30 && is.null(substitute(covariates))) {
    covar_col_means <- colMeans(coefs[, covar_cols, drop = FALSE])
    top_30_covars <- covar_col_means[sort(order(abs(covar_col_means), decreasing = TRUE)[1:30])]
    covar_cols <- names(top_30_covars)
    coefs <- coefs[, c("id", covar_cols), drop = FALSE]
  }
  coefs_long <- coefs |>
    transform(id = as.integer(gsub("boot", "", id))) |>
    reshape(
      idvar = "id",
      varying = setdiff(colnames(coefs), "id"),
      v.names = "coef",
      timevar = "covariate",
      times = setdiff(colnames(coefs), "id"),
      direction = "long"
    ) |>
    transform(id = paste0("boot", id), covariate_id = as.integer(factor(covariate))) |>
    tibble::as_tibble()
  ggplot2::ggplot(coefs_long, ggplot2::aes(x = factor(covariate), y = coef)) +
    ggplot2::geom_boxplot(size = 0.25, notchwidth = 0.25, outlier.size = 0.5) +
    ggplot2::labs(y = "Coefficient distribution", x = "Covariate") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.ticks.x = ggplot2::element_line(color = "gray90"))
}

#' Plot each covariate's smallest variable selection threshold
#' 
#' Plot the results of the \link{selection_thresholds} function.
#' 
#' @param object An object of class \link{bolasso} or `bolasso_fast`.
#'   This argument is optional if you directly pass in the data via the
#'   `data` argument. E.g. `data = selection_thresholds(object)`.
#' @param data A dataframe containing the selection thresholds. E.g.
#'   obtained via `selection_thresholds(object)`. This argument is optional
#'   if you directly pass a `bolasso` or `bolasso_fast` object via the `object`
#'   argument.
#' @param ... Additional arguments to pass directly to
#'   \link{selection_thresholds}.
#' 
#' @seealso [selection_thresholds()]
#' 
#' @return A `ggplot` object
#' @export
plot_selection_thresholds <- function(object = NULL, data = NULL, ...) {
  covariate <- covariate_id <- threshold <- NULL
  if (is.null(object) && is.null(data)) {
    stop("Either `object` or `data` must be provided")
  }
  if (is.null(data)) {
    selection_grid <- selection_thresholds(object, ...)
  } else {
    selection_grid <- data
  }
  t <- 30
  n_covar <- length(unique(selection_grid$covariate))
  selection_grid |>
    ggplot2::ggplot(ggplot2::aes(
      x = if (n_covar > t) covariate_id else covariate,
      y = threshold
    )) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 1,
      color = "gray40",
      fill = "gray40",
      alpha = 0.7
    ) +
    ggplot2::coord_cartesian(
      ylim = c(min(selection_grid$threshold), 1),
      expand = FALSE
    ) +
    ggplot2::facet_wrap(~ method, nrow = 2) +
    ggplot2::labs(
      x = if (n_covar > t) "Covariate ID" else "Covariate",
      y = expression("Threshold (1 - " * alpha * ")")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = if (n_covar > t) 0 else 90,
        vjust = 0.5
      ),
      axis.ticks.x = ggplot2::element_line(color = "gray70"),
      strip.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}
