#' @importFrom tibble as_tibble

tidy_intercept <- function(dat) {
  if (!any(colnames(dat) %in% c("intercept", "(Intercept)"))) return(dat)
  int_col <- colnames(dat) %in% c("intercept", "(Intercept)")
  stopifnot(sum(int_col) == 1)
  colnames(dat)[int_col] <- "Intercept"
  dat
}

tidy_selected_vars <- function(object) {
  as_tibble(
    as.data.frame(
      as.matrix(object)
    ),
    rownames = "id"
  )
}
