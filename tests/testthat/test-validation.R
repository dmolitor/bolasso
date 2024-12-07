test_that("validate_varnames works correctly", {
  expect_error(
    validate_varnames(c("hello", "there"), c("hello", "tHeRe")),
    regexp = "Modeling data and prediction data have differing names"
  )
  expect_null(validate_varnames(c("hello", "there"), c("hello", "there")))
})

test_that("validate_data works correctly", {
  expect_error(
    validate_data(mpg ~ ., x = mtcars),
    "Both a formula and data must be supplied"
  )
  expect_error(
    validate_data(mpg ~ ., as.matrix(mtcars)),
    "Argument `data` must be an object with class `data.frame`"
  )
  expect_error(
    validate_data(x = mtcars[, -1], y = mtcars[, 1]),
    "`x` must be of class `matrix` or `sparseMatrix`"
  )
  expect_null(
    validate_data(mpg ~ ., data = mtcars)
  )
})
