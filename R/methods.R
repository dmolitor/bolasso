#' @method coef bolasso
#' @export
coef.bolasso <- function(object, select = c("lambda.min", "lambda.1se"), ...) {
  implement <- attributes(object)$implement
  coefs <- future.apply::future_lapply(
    object,
    function(i) {
      do.call(
        stats::coef,
        translate_coef(implement, i, select[[1]], ...)
      )
    },
    future.seed = TRUE,
    future.packages = c("Matrix", implement)
  )
  coefs <- do.call(cbind, coefs)
  colnames(coefs) <- paste0("boot", 1:ncol(coefs))
  Matrix::t(coefs)
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
  p <- future.apply::future_lapply(
    object,
    function(i) {
      do.call(
        stats::predict,
        translate_predict(implement, i, new.data, select[[1]], ...)
      )
    },
    future.seed = TRUE,
    future.packages = c("Matrix", implement)
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
  selected90 <- selected_vars(x)
  selected100 <- selected_vars(x, threshold = 1)
  cat("-------------",
      paste0(nboot, "-fold"),
      "bootstrapped Lasso -------------\n\n")
  cat(
    "Model matrix dimensions:\n",
    "  -", npred, "Predictors\n",
    "  -", nobs, "Observations\n\n"
  )
  cat(
    "Selected variables:\n",
    "  -",
    paste0(nrow(selected90[selected90$variable != "Intercept", ]), "/", npred),
    "predictors selected with 90% threshold\n",
    "  -",
    paste0(nrow(selected100[selected100$variable != "Intercept", ]), "/", npred),
    "predictors selected with 100% threshold\n"
  )
  return(invisible(x))
}

#' @method plot bolasso
#' @export
plot.bolasso <- function(x, threshold = 0.9, ...) {
  s <- selected_vars(object = x, threshold = threshold)
  s <- s[order(s$mean_coef), ]
  s <- s[s$variable != "Intercept", ]
  if (nrow(s) == 0) {
    message("No selected variables to plot")
    return(invisible(x))
  }
  s$variable <- factor(s$variable, levels = s$variable)
  s$mean_coef <- round(s$mean_coef, 3)
  s$color <- ifelse(s$mean_coef < 0, "negative", "positive")
  coef_range <- range(s$mean_coef)
  coef_diff <- coef_range[[2]] - coef_range[[1]]
  if (nrow(s) >= 10) {
    s <- rbind(s[1:5, ], s[(nrow(s) - 4):nrow(s), ])
  }
  ggplot2::ggplot(data = s,
                  ggplot2::aes(x = get("variable"),
                               y = get("mean_coef"),
                               fill = get("color"))) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::geom_text(ggplot2::aes(label = paste0("(", get("mean_coef"), ")"),
                                    x = get("variable"),
                                    y = get("mean_coef") +
                                      0.03 *
                                      sign(get("mean_coef")) *
                                      coef_diff),
                       size = 3,
                       inherit.aes = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Predictor",
                  y = "Mean Coefficient",
                  title = "Top/Bottom 5 Selected Predictors") +
    ggplot2::theme(axis.title = ggplot2::element_text(face = "bold"),
                   axis.text.x = ggplot2::element_text(angle = 90),
                   plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold"),
                   legend.position = "none")
}
