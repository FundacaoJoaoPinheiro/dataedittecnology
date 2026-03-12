#' @title Detect outlier
#' @description
#' Detect outlier as well as dataMaid::identifyOutliers()
#' 'mc' is the 'medcouple', a robust concept and estimator of skewness, and a and b are appropriate constants (-4 and 3).
#' The medcouple is defined as a scaled median difference of the left and right half of distribution,
#' and hence not based on the third moment as the classical skewness.
#'
#' @param variable quantitative variable
#' @param a constant
#' @param b constant
#' @importFrom robustbase mc
#' @import robustbase
#' @return Vector with TRUE or FALSE

outlier_function <- function(variable, a=-4,b=3){
  q1 <- stats::quantile(variable, probs = 0.25, na.rm = T, names = F)
  q3 <- stats::quantile(variable, probs = 0.75, na.rm = T, names = F)
  iqr <- stats::IQR(variable, na.rm = T)
  mc <- robustbase::mc(variable, na.rm = T)
  inf <- q1 - 1.5*exp(a*mc)*iqr
  sup <- q3 + 1.5*exp(b*mc)*iqr

  check <- ifelse(variable < inf | variable > sup, T, F)

  return(check)
}
