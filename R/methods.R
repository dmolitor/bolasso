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

#' @method print bolasso
#' @export
print.bolasso <- function(object, ...) {
  nboot <- attributes(object)$nboot
  npred <- length(attributes(object)$varnames)
  selected90 <- selected_vars(object)
  selected100 <- selected_vars(object, threshold = 1)
  cat("-------------",
      paste0(nboot, "-fold"),
      "bootstrapped Lasso -------------\n\n")
  cat(
    "Model matrix dimensions:\n",
    "  -", npred, "Predictors\n",
    "  -", object[[1]]$glmnet.fit$nobs, "Observations\n\n"
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
  return(invisible(object))
}

#' @method plot bolasso
#' @export
plot.bolasso <- function(object, threshold = 0.9, ...) {
  s <- selected_vars(object = object, threshold = threshold, ...)
  s <- s[order(s$mean_coef), ]
  s$variable <- factor(s$variable)
  s$mean_coef <- round(s$mean_coef, 3)
  s$color <- ifelse(s$mean_coef < 0, "negative", "positive")
  if (nrow(s) >= 10) {
    s <- rbind(s[1:5, ], s[(nrow(s) - 5):nrow(s), ])
  }
  ggplot2::ggplot(data = s,
                  aes(x = variable,
                      y = mean_coef,
                      fill = color)) +
    ggplot2::geom_bar(stat = "identity", width = 0.5) +
    ggplot2::theme_minimal() +
    ggplot2::labs(x = "Predictor",
                  y = "Mean Coefficient",
                  title = "Top/Bottom 5 Selected Predictors") +
    ggplot2::theme(axis.title = element_text(face = "bold"),
                   axis.text.x = element_text(angle = 90),
                   plot.title = element_text(hjust = 0.5, face = "bold"),
                   legend.position = "none")
}
