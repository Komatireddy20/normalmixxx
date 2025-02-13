#' Cumulative Distribution Function (CDF) for Normal Mixture
#'
#' Computes the CDF of a normal mixture distribution with given parameters.
#' @param q Numeric value at which to evaluate the CDF.
#' @return The CDF value at `q`.
#' @export
pmix <- function(q) {
  p1 <- pnorm(q, mean = 1, sd = 2)
  p2 <- pnorm(q, mean = 5, sd = 1/2)
  return(2/3 * p1 + 1/3 * p2)
}

#' Probability Density Function (PDF) for Normal Mixture
#'
#' Computes the PDF of a normal mixture distribution with given parameters.
#' @param x Numeric value at which to evaluate the PDF.
#' @return The PDF value at `x`.
#' @export
dmix <- function(x) {
  d1 <- dnorm(x, mean = 1, sd = 2)
  d2 <- dnorm(x, mean = 5, sd = 1/2)
  return(2/3 * d1 + 1/3 * d2)
}

#' Random Sampling from Normal Mixture
#'
#' Generates random samples from a normal mixture distribution.
#' @param n Number of samples to generate.
#' @return A vector of `n` random samples.
#' @export
rmix <- function(n) {
  u <- runif(n)
  x <- ifelse(u < 2/3, rnorm(n, mean = 1, sd = 2), rnorm(n, mean = 5, sd = 1/2))
  return(x)
}

#' Quantile Function (Inverse CDF) for Normal Mixture
#'
#' Computes the quantile function for a normal mixture distribution.
#' @param p Probability value (between 0 and 1).
#' @return The quantile value corresponding to probability `p`.
#' @export
qmix <- function(p) {
  qf <- function(x) pmix(x) - p
  return(uniroot(qf, interval = c(-10, 10), extendInt = "yes")$root)
}
