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

#' Bootrap-enhanced LASSO
#'
#' This function implements model-consistent LASSO estimation through the
#' bootstrap. It applies bootstrapping with
#' \href{https://cran.r-project.org/web/packages/future.apply/index.html}{future.apply}
#' for distributed processing across cores or computing clusters.
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
