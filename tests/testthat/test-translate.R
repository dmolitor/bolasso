test_that("translate_coef translates correctly", {
  expect_equal(
    names(translate_coef("gamlr", list(), s = "min")),
    c("object", "select")
  )
  expect_equal(
    names(translate_coef("gamlr", list(), s = "min", "evil" = "laugh")),
    c("object", "select", "evil")
  )
  expect_equal(
    names(translate_coef("glmnet", list(), s = "lambda.min")),
    c("object", "s")
  )
  expect_equal(
    names(translate_coef("glmnet", list(), s = "lambda.min", "evil" = "laugh")),
    c("object", "s", "evil")
  )
})

test_that("translate_lasso translates correctly", {
  expect_equal(translate_lasso("glmnet"), glmnet::cv.glmnet)
  expect_equal(translate_lasso("gamlr"), gamlr::cv.gamlr)
  expect_error(translate_lasso("unknown"))
})

test_that("translate_lambda translates correctly", {
  expect_equal(translate_lambda("gamlr", "lambda.min"), "min")
  expect_equal(translate_lambda("gamlr", "lambda.1se"), "1se")
  expect_equal(translate_lambda("glmnet", "min"), "lambda.min")
  expect_equal(translate_lambda("glmnet", "1se"), "lambda.1se")
  expect_error(translate_lambda("gamlr", "evillaugh"))
})

test_that("translate_predict translates correctly", {
  expect_equal(
    names(translate_predict("gamlr", list(), list(), s = "min")),
    c("object", "newdata", "select")
  )
  expect_equal(
    names(translate_predict("gamlr", list(), list(), s = "min", "evil" = "laugh")),
    c("object", "newdata", "select", "evil")
  )
  expect_equal(
    names(translate_predict("glmnet", list(), list(), s = "min")),
    c("object", "newx", "s")
  )
  expect_equal(
    names(translate_predict("glmnet", list(), list(), s = "min", "evil" = "laugh")),
    c("object", "newx", "s", "evil")
  )
})
