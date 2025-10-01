test_that("plotting selected variables works", {
  df <- data.frame(y = rbinom(1000, 1, 0.5), X = rnorm(1000), Z = rnorm(1000, 2))
  model <- bolasso(y ~ ., data = df, family = "binomial", fast = TRUE)
  
  expect_warning(plot_selected_variables(model, threshold = 1), "Nothing to plot!")
  expect_equal(suppressWarnings(plot_selected_variables(model, threshold = 1)), NULL)

  expect_s3_class(plot_selected_variables(model, covariates = c("X", "Z"), threshold = 0), "ggplot")
  expect_s3_class(plot_selected_variables(model, covariates = c(X, Z), threshold = 0), "ggplot")
})

test_that("plotting bolasso object works", {
  df <- data.frame(y = rbinom(1000, 1, 0.5), X = rnorm(1000), Z = rnorm(1000, 2))
  model <- bolasso(y ~ ., data = df, family = "binomial", fast = TRUE)
  
  expect_s3_class(plot(model, covariates = c("X", "Z")), "ggplot")
  expect_s3_class(plot(model, covariates = c(X, Z)), "ggplot")
  expect_s3_class(plot(model, covariates = c()), "ggplot")
})