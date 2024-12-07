test_that("tidy_intercept tidies correctly", {
  test1 <- tibble::tibble("a" = 1:10)
  test2 <- tibble::tibble("intercept" = 1:10, "a" = 11:20)
  test3 <- tibble::tibble("(Intercept)" = 1:10, "a" = 11:20)

  expect_equal(tidy_intercept(test1), test1)
  expect_equal(tidy_intercept(test2), tidy_intercept(test3))
})

test_that("tidy tidies correctly", {
  model <- bolasso(mpg ~ ., mtcars, 10, FALSE, fast = TRUE)
  tidied <- tidy(model)

  expect_equal(dim(tidied), c(10, ncol(mtcars) + 1))
  expect_equal(names(tidied), c("id", "Intercept", colnames(model.matrix(mpg ~ . -1, mtcars))))
})
