#' Test for association between two paired variables.
#'
#' @description
#' Tests for association between two variables:
#' \describe{
#' \item{Categorical-Categorical}{Using pearson's chi-squared test (chisq.test function from stats package).}
#' \item{Continuous-Continuous}{Using correlation test (cor.test function from stats package).}
#' \item{Categorical-Continuous}{Using analysis of variance model (aov function from stats package).}
#' }
#'
#' @param data A dataframe. It is strongly recommended that the dataframe has no missing data and is preprocessed.
#' @param var1 First variable
#' @param var2 Second variable
#'
#' @details This provides a wrapper to \code{chisq.test}, \code{cor.test}, \code{aov}, \code{p.adjust} from \code{stats} package to test association between two variables
#'
#'
#' @author Elyas Heidari
#'
#' @return P.value of test between the two variables.
#'
#' @export
#'
#' @importFrom stats cor.test chisq.test aov


test.pair <- function(data, var1, var2) {

  is.cat <- function(var) {
    return(!length(unique(var[!is.na(var)])) > levels)
  }

  var1 <- data[, var1]
  var2 <- data[, var2]
  var1.is.cat <- is.cat(var1)
  var2.is.cat <- is.cat(var2)
  if (!var1.is.cat & !var2.is.cat) {
    ct <- stats::cor.test(var1, var2)
    ct <- ct$p.value
  }

  if (var1.is.cat & var2.is.cat) {
    ct <- stats::chisq.test(var1, var2)
    ct <- ct$p.value
  }
  if (var1.is.cat & !var2.is.cat) {
    ct <- stats::aov(var2 ~ var1, data = data.frame(var1 = var1 , var2 = var2))
    ct <- summary(ct)[[1]][["Pr(>F)"]][1]
  }
  if (!var1.is.cat & var2.is.cat) {
    ct <- stats::aov(var1 ~ var2, data = data.frame(var1 = var1 , var2 = var2))
    ct <- summary(ct)[[1]][["Pr(>F)"]][1]
  }
  return(ct)
}
