test_that("bootstraps works", {
  x <- model.matrix(mpg ~ . - 1, mtcars)
  boots <- bootstraps(x, 5)

  expect_type(boots, "list")
  expect_length(boots, 5)
  expect_equal(
    vapply(boots, \(x) all(x %in% 1:nrow(mtcars)), logical(1)),
    rep(TRUE, 5)
  )
  expect_error(bootstraps(mtcars, 5))
})

test_that("formula_lhs works", {
  expect_equal(formula_lhs(y ~ .), "y")
  expect_equal(formula_lhs("y ~ ."), "y")
  expect_error(formula_lhs(~ x))
})

test_that("formula_rhs works", {
  expect_equal(formula_rhs(y ~ .), ".")
  expect_equal(formula_rhs("y ~ ."), ".")
  expect_error(formula_rhs("a ~"))
})

test_that("sparsity_threshold works", {
  x <- cbind(Matrix::rsparsematrix(11, 3, .5), c(rep(0, 10), 1), c(1, rep(0, 10)))

  expect_equal(sparsity_threshold(x, 0.1), x[, -(4:5)])
  expect_error(sparsity_threshold(as.matrix(x), 0.1))
})
