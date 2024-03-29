test_that("tidy_intercept tidies correctly", {
  test1 <- tibble::tibble("a" = 1:10)
  test2 <- tibble::tibble("intercept" = 1:10, "a" = 11:20)
  test3 <- tibble::tibble("(Intercept)" = 1:10, "a" = 11:20)

  expect_equal(tidy_intercept(test1), test1)
  expect_equal(tidy_intercept(test2), tidy_intercept(test3))
})
