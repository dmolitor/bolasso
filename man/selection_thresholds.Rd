% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{selection_thresholds}
\alias{selection_thresholds}
\title{Calculate each covariate's smallest variable selection threshold}
\usage{
selection_thresholds(object, grid = seq(0, 1, by = 0.01), ...)
}
\arguments{
\item{object}{An object of class \link{bolasso} or \code{bolasso_fast}.}

\item{grid}{A vector of numbers between 0 and 1 (inclusive) specifying
the grid of threshold values to calculate variable inclusion criterion
at. Defaults to \code{seq(0, 1, by = 0.01)}.}

\item{...}{Additional parameters to pass to \code{\link{coef}} on objects of
class \link{bolasso} and \code{bolasso_fast}.}
}
\value{
A tibble with dimension (2*p)x5 where p is the number of covariates.
}
\description{
There are two methods of variable selection for covariates. The first
is the Variable Inclusion Probability (VIP) introduced by Bach (2008)
and generalized by Bunea et al (2011). The second is the Quantile
confidence interval (QNT) proposed by Abram et al (2016). For a given level
of significance alpha, each method selects covariates for the given
threshold = 1 - alpha. The higher the threshold (lower alpha), the more
stringent the variable selection criterion.
}
\details{
This function returns a tibble that, for each covariate, returns the
largest threshold (equivalently smallest alpha) at which it would be
selected for both the VIP and the QNT methods. Consequently the number
of rows in the returned tibble is 2*p where p is the number of covariates
included in the model.
}
