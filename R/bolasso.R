# Code --------------------------------------------------------------------

bootstraps <- function(dat, n) {
  stopifnot(is.matrix(dat),
            is.numeric(n) && n >= 1)
  lapply(1:n, function(i) sort(sample(nrow(dat), replace = TRUE)))
}

new_bolasso <- function(x, implement) {
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
  x
}

bolasso.fit <- function(x, y, n.boot, implement, ...) {
  folds <- bootstraps(dat = x, n = n.boot)
  # Initialize progress bar
  pb <- progressr::progressor(along = folds)
  future.apply::future_lapply(
    folds,
    function(i) {
      lasso_args <- list(x = x[i, ], y = y[i], ...)
      g <- do.call(
        what = translate_lasso(implement), 
        args = lasso_args
      )
      # Report progress
      pb()
      g
    },
    future.seed = TRUE
  )
}

bolasso <- function(x, 
                    y, 
                    n.boot = 100, 
                    progress = TRUE, 
                    implement = "glmnet", 
                    ...) {
  if (progress) {
    progressr::with_progress(
      models <- bolasso.fit(x = x,
                            y = y, 
                            n.boot = n.boot, 
                            implement = implement, 
                            ...)
    )
  } else {
    models <- bolasso.fit(x = x,
                          y = y, 
                          n.boot = n.boot, 
                          implement = implement, 
                          ...)
  }
  new_bolasso(models, implement = implement)
}

coef.bolasso <- function(object, select = c("lambda.min", "lambda.1se"), ...) {
  implement <- attributes(object)$implement
  c <- lapply(
    object, 
    function(i) {
      do.call(
        coef, 
        translate_coef(implement, i, select[[1]], ...)
      )
    }
  )
  c <- do.call(cbind, c)
  colnames(c) <- paste0("boot", 1:ncol(c))
  Matrix::t(c)
}

predict.bolasso <- function(object, new.data, select = c("min", "1se"), ...) {
  implement <- attributes(object)$implement
  p <- lapply(
    object, 
    function(i) {
      do.call(
        predict, 
        translate_predict(implement, i, new.data, select[[1]], ...)
      )
    }
  )
  p <- do.call(cbind, p)
  colnames(p) <- paste0("boot", 1:ncol(p))
  p
}

translate_coef <- function(implement, object, s, ...) {
  if (!implement %in% c("gamlr", "glmnet")) {
    stop("Supported options are 'glmnet' or 'gamlr'", call. = FALSE)
  }
  if (implement == "glmnet") {
    list("object" = object, "s" = translate_lambda(implement, s), ...)
  } else {
    list("object" = object, "select" = translate_lambda(implement, s), ...)
  }
}

translate_lasso <- function(implement) {
  if (!implement %in% c("gamlr", "glmnet")) {
    stop("Supported options are 'glmnet' or 'gamlr'", call. = FALSE)
  }
  if (implement == "gamlr") {
    gamlr::cv.gamlr
  } else {
    glmnet::cv.glmnet
  }
}

translate_lambda <- function(implement, val) {
  if (!implement %in% c("gamlr", "glmnet")) {
    stop("Supported options are 'glmnet' or 'gamlr'", call. = FALSE)
  }
  if (!val %in% c("min", "1se", "lambda.min", "lambda.1se")) {
    stop("Invalid lambda selection value", call. = FALSE)
  }
  if (implement == "glmnet") {
    switch(val,
           "lambda.min" = ,
           "min" = "lambda.min",
           "lambda.1se" = ,
           "1se" = "lambda.1se")
  } else {
    switch(val,
           "lambda.min" = ,
           "min" = "min",
           "lambda.1se" = ,
           "1se" = "1se")
  }
}

translate_predict <- function(implement, object, new.dat, s, ...) {
  if (!implement %in% c("gamlr", "glmnet")) {
    stop("Supported options are 'glmnet' or 'gamlr'", call. = FALSE)
  }
  if (implement == "glmnet") {
    list("object" = object, 
         "newx" = new.dat, 
         "s" = translate_lambda(implement, s),
         ...)
  } else {
    list("object" = object, 
         "newdata" = new.dat, 
         "select" = translate_lambda(implement, s),
         ...)
  }
}

# Examples ----------------------------------------------------------------

library(dplyr)

iris_dat <- iris %>%
  mutate(is_setosa = as.integer(Species == "setosa")) %>%
  select(-Species) %>%
  as.matrix()

idx <- sample(1:nrow(iris_dat), 100)
train_x <- iris_dat[idx, 1:4]
train_y <- iris_dat[idx, "is_setosa"]
test_x <- iris_dat[-idx, 1:4]
test_y <- iris_dat[-idx, "is_setosa"]

# Train a bolasso model
train_bolasso_glmnet <- bolasso(x = train_x,
                                y = train_y, 
                                family = "binomial",
                                implement = "glmnet")
train_bolasso_gamlr <- bolasso(x = train_x,
                               y = train_y, 
                               family = "binomial",
                               implement = "gamlr")

model_coefs_glmnet <- coef(train_bolasso_glmnet, select = "min")
model_coefs_gamlr <- coef(train_bolasso_gamlr, select = "min")

model_preds_glmnet <- predict(train_bolasso_glmnet, 
                              test_x, 
                              select = "min", 
                              type = "response")
model_preds_gamlr <- predict(train_bolasso_gamlr, 
                             test_x, 
                             select = "min", 
                             type = "response")

ensemble_pred_glmnet <- rowMeans(model_preds_glmnet)
ensemble_pred_gamlr <- rowMeans(model_preds_gamlr)

evaluate <- bind_cols("glmnet" = ensemble_pred_glmnet,
                      "gamlr" = ensemble_pred_gamlr,
                      "truth" = test_y)
