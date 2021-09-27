validate_varnames <- function(x, y) {
  if (length(setdiff(x, y)) != 0) {
    stop(
      "Modeling data and prediction data have differing names\n",
      "\r  - Columns in modeling data not in prediction data: ",
      paste(setdiff(x, y), collapse = " | "),
      "\n\r  - Columns in prediction data not in modeling data: ",
      paste(setdiff(y, x), collapse = " | "),
      "\n",
      call. = FALSE
    )
  }
}

validate_data <- function(form, data, x = NULL, y = NULL) {
  if (!is.null(x) && !is.null(y)) {
    if (!(is.matrix(x) || inherits(x, "sparseMatrix"))) {
      stop("`x` must be of class `matrix` or `sparseMatrix`", call. = FALSE)
    }
    return(invisible(NULL))
  }
  if ((missing(form) || is.null(form)) || (missing(data) || is.null(data))) {
    stop("Both a formula and data must be supplied", call. = FALSE)
  }
  if (!is.data.frame(data)) {
    stop("Argument `data` must be an object with class `data.frame`",
         call. = FALSE)
  }
}
