test_that("bolasso glmnet functionality works sequentially", {
  # glmnet formula
  set.seed(123)
  model_form_glmnet <- bolasso(
    formula = mpg ~ .,
    data = mtcars,
    n.boot = 5,
    implement = "glmnet"
  )
  # glmnet matrix interface
  x <- model.matrix(mpg ~ . - 1, mtcars)
  y <- mtcars[["mpg"]]
  set.seed(123)
  model_xy_glmnet <- bolasso(
    x = x,
    y = y,
    n.boot = 5,
    implement = "glmnet"
  )

  expect_equal(coef(model_form_glmnet), coef(model_xy_glmnet))
  expect_equal(predict(model_form_glmnet, mtcars), predict(model_xy_glmnet, x))
  expect_equal(selected_vars(model_form_glmnet), selected_vars(model_xy_glmnet))
  expect_s3_class(plot(model_form_glmnet), "ggplot")
  expect_s3_class(plot(model_xy_glmnet), "ggplot")
})

test_that("bolasso gamlr functionality works sequentially", {
  # gamlr formula
  set.seed(123)
  model_form_gamlr <- bolasso(
    formula = mpg ~ .,
    data = mtcars,
    n.boot = 5,
    implement = "gamlr"
  )
  # glmnet matrix interface
  x <- model.matrix(mpg ~ . - 1, mtcars)
  y <- mtcars[["mpg"]]
  set.seed(123)
  model_xy_gamlr <- bolasso(
    x = x,
    y = y,
    n.boot = 5,
    implement = "gamlr"
  )

  expect_equal(coef(model_form_gamlr), coef(model_xy_gamlr))
  expect_equal(predict(model_form_gamlr, mtcars), predict(model_xy_gamlr, x))
  expect_equal(selected_vars(model_form_gamlr), selected_vars(model_xy_gamlr))
  expect_s3_class(plot(model_form_gamlr), "ggplot")
  expect_s3_class(plot(model_xy_gamlr), "ggplot")
})
