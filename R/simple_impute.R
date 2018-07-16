#' simple missing-data imputation
#'
#'
#' @description
#' Imputes missing-data using mean value for continuous variables and mode value for categoricals.
#'
#'
#' @param  data an arbitrary dataset.
#'
#'
#'
#' @author  Vahid Balazadeh
#'
#'
#' @return a normalized dataframe with no missing data of continuous and (or) categorical measurements.
#' @export simple.impute
#'
#'

simple.impute <- function(data) {

  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }

  na2mean <- function(x) if (is.numeric(x) == T) replace(x, is.na(x), mean(x, na.rm = TRUE)) else replace(x, is.na(x), getmode(x))


  data.frame(lapply(data, na2mean))
}
