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
