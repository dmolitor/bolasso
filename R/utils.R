# Code --------------------------------------------------------------------

bootstraps <- function(dat, n) {
  stopifnot(is.matrix(dat) || inherits(dat, "sparseMatrix"),
            is.numeric(n) && n >= 1)
  lapply(1:n, function(i) sort(sample(nrow(dat), replace = TRUE)))
}

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

formula_lhs <- function(form) {
  form <- tryCatch(
    as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) stop("Formula is missing LHS variable", call. = FALSE)
  deparse(form[[2]])
}

formula_rhs <- function(form) {
  form <- tryCatch(
    as.formula(form),
    error = function(e) stop("Invalid formula", call. = FALSE)
  )
  if (length(form) < 3) {
    deparse(form[[2]])
  } else {
    deparse(form[[3]])
  }
}

model_matrix <- function(form, data, x = NULL, y = NULL, prediction = FALSE) {

  # form = form; data = new.data; prediction = TRUE

  validate_data(form = form, data = data, x = x, y = y)
  if (!is.null(x) && !is.null(y)) return(list(x = x, y = y))
  form_rhs <- as.formula(paste("~", formula_rhs(form), "- 1"))
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
    y <- data[, form_lhs]
  }
  list(x = x, y = y)
}

predict.bolasso <- function(object, new.data, select = c("min", "1se"), ...) {
  varnames <- attributes(object)$varnames
  implement <- attributes(object)$implement
  form <- attributes(object)$call$form
  if (!is.null(form)) {
    new.data <- model_matrix(form = form, data = new.data, prediction = TRUE)$x
  } else {
    new.data <- model_matrix(1 ~ . - 1, data = new.data, prediction = TRUE)$x
  }
  validate_varnames(x = varnames, y = colnames(new.data))
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

selected_vars <- function(object, threshold = 0.9, ...) {
  stopifnot(inherits(object, "bolasso"),
            is.numeric(threshold))
  model_coefs <- coef.bolasso(object = object, ...)
  model_coefs <- sparsity_threshold(model_coefs, threshold = threshold)
  model_coefs <- tidy_selected_vars(model_coefs)
  tidy_intercept(model_coefs)
}

sparsity_threshold <- function(dat, threshold) {
  dat[, (diff(dat@p) / dat@Dim[[1]]) > threshold, drop = FALSE]
}

tidy_intercept <- function(dat) {
  if (!any(colnames(dat) %in% c("intercept", "(Intercept)"))) return(dat)
  int_col <- colnames(dat) %in% c("intercept", "(Intercept)")
  stopifnot(sum(!int_col) != 1)
  colnames(dat)[int_col] <- "Intercept"
  dat
}

tidy_selected_vars <- function(object) {
  dplyr::as_tibble(
    as.data.frame(
      as.matrix(object)
    ),
    rownames = "id"
  )
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

validate_varnames <- function(x, y) {
  if (length(setdiff(x, y)) != 0) {
    stop(
      "Modeling data and prediction data have differing names\n",
      "\r  - Columns in modeling data not in prediction data: ",
      paste(setdiff(x, y), collapse = " | "),
      "\n\r  - Columns in prediction data not in modeling data: ",
      paste(setdiff(y, x), collapse = " | "),
      "\n",
      call. = FALSE
    )
  }
}

validate_data <- function(form, data, x = NULL, y = NULL) {
  if (!is.null(x) && !is.null(y)) {
    if (!(is.matrix(x) || inherits(x, "sparseMatrix"))) {
      stop("`x` must be of class `matrix` or `sparseMatrix`", call. = FALSE)
    }
    return(invisible(NULL))
  }
  if ((missing(form) || is.null(form)) || (missing(data) || is.null(data))) {
    stop("Both a formula and data must be supplied", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("Argument `data` must be an object with class `data.frame`",
         call. = FALSE)
  }
}

# Examples ----------------------------------------------------------------

library(dplyr)

iris_dat <- iris %>%
  `[`(sample(nrow(iris)), ) %>%
  mutate(is_setosa = as.integer(Species == "setosa")) %>%
  select(-Species)
  # mutate(challenge = factor(c(rep(1:4, 25), rep(1:5, 10))))

train_x <- iris_dat[1:100, 1:4]
train_y <- iris_dat[1:100, "is_setosa"]
test_x <- iris_dat[-(1:100), 1:4]
test_y <- iris_dat[-(1:100), "is_setosa"]

# Train a bolasso model
set.seed(321)
train_bolasso_glmnet <- bolasso(x = as.matrix(train_x),
                                y = train_y,
                                family = "binomial",
                                implement = "glmnet")
set.seed(123)
train_bolasso_gamlr <- bolasso(x = as.matrix(train_x),
                               y = train_y,
                               family = "binomial",
                               implement = "gamlr")

model_coefs_glmnet <- selected_vars(train_bolasso_glmnet, select = "min")
model_coefs_gamlr <- selected_vars(train_bolasso_gamlr, select = "min")

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

# Test formula interface
set.seed(123)
train_bolasso_gamlr_form <- bolasso(
  form = is_setosa ~ .,
  data = iris_dat[1:100, ],
  family = "binomial",
  n.boot = 100,
  progress = TRUE,
  implement = "gamlr"
)
set.seed(321)
train_bolasso_glmnet_form <- bolasso(
  form = is_setosa ~ .,
  data = iris_dat[1:100, ],
  family = "binomial",
  n.boot = 100,
  progress = TRUE,
  implement = "glmnet"
)

model_preds_glmnet_form <- predict(train_bolasso_glmnet_form,
                                   test_x,
                                   select = "min",
                                   type = "response")
model_preds_gamlr_form <- predict(train_bolasso_gamlr_form,
                                  test_x,
                                  select = "min",
                                  type = "response")

ensemble_pred_glmnet_form <- rowMeans(model_preds_glmnet_form)
ensemble_pred_gamlr_form <- rowMeans(model_preds_gamlr_form)

evaluate <- bind_cols("glmnet_mat" = ensemble_pred_glmnet,
                      "glmnet_form" = ensemble_pred_glmnet_form,
                      "gamlr_mat" = ensemble_pred_gamlr,
                      "gamlr_form" = ensemble_pred_gamlr_form,
                      "truth" = test_y)
