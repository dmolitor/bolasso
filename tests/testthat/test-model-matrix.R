test_that("model_matrix handles alternate interface correctly", {
  x <- Matrix::sparse.model.matrix(mpg ~ . - 1, mtcars)
  y <- mtcars[["mpg"]]
  x_dense <- as.matrix(x)

  expect_type(model_matrix(x = x, y = y), "list")
  expect_type(model_matrix(x = as.matrix(x), y = y), "list")
  expect_length(model_matrix(x = x, y = y), 2)
})

test_that("model_matrix handles formula interface correctly", {
  expect_type(model_matrix(mpg ~ ., mtcars), "list")
  expect_length(model_matrix(mpg ~ ., mtcars), 2)
  expect_null(model_matrix(mpg ~ ., mtcars, prediction = TRUE)[["y"]])
  expect_equal(
    model_matrix(mpg ~ ., mtcars),
    model_matrix(
      x = Matrix::sparse.model.matrix(mpg ~ . - 1, mtcars),
      y = mtcars[["mpg"]]
    )
  )
})

test_that("model_matrix asserts correct data types", {
  expect_error(
    model_matrix(x = mtcars[, -1], y = mtcars[, 1]),
    regexp = "`x` must be of class `matrix` or `sparseMatrix`"
  )
  expect_error(
    model_matrix(mpg ~ ., as.matrix(mtcars)),
    "Argument `data` must be an object with class `data.frame`"
  )
})
