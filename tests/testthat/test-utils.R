test_that("bootstraps works", {
  x <- model.matrix(mpg ~ . - 1, mtcars)
  boots <- bootstraps(x, 5)

  expect_type(boots, "list")
  expect_length(boots, 5)
  expect_equal(
    vapply(boots, function (x) all(x %in% 1:nrow(mtcars)), logical(1)),
    rep(TRUE, 5)
  )
  expect_error(bootstraps(mtcars, 5))
})

test_that("formula_lhs works", {
  expect_equal(formula_lhs(y ~ .), "y")
  expect_equal(formula_lhs("y ~ ."), "y")
  expect_error(formula_lhs(~ x), regexp = "Formula is missing LHS variable")
})

test_that("formula_rhs works", {
  expect_equal(formula_rhs(y ~ .), ".")
  expect_equal(formula_rhs("y ~ ."), ".")
  expect_error(formula_rhs("a ~"), regexp = "Invalid formula")
})

test_that("last works", {
  expect_null(last(NULL))
  expect_null(last(c()))
  expect_equal(last(NA), NA)
  expect_equal(last(1:1000), 1000)
})

test_that("thresholding functions work", {
  x <- cbind(Matrix::rsparsematrix(11, 3, .5), c(rep(0, 10), 1), c(1, rep(0, 10)))
  x_qnt <- abs(x)

  expect_equal(vip_threshold(x, 0.1), x[, -(4:5)])
  expect_error(vip_threshold(as.matrix(x), 0.1))
  expect_equal(
    qnt_threshold(x_qnt, 0.1),
    x_qnt[, apply(x_qnt, 2, \(x) !0 %in% quantile(x, c(0.45, 0.55)))]
  )
})

test_that("same_sign works", {
  expect_true(same_sign(rep(1, 1e3)))
  expect_true(same_sign(rep(-1, 1e3)))
  expect_false(same_sign(rep(c(-1, 1), 1e3)))
})
