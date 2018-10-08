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
#' @param var1_name First variable's name.
#' @param var2_name Second variable's name.
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#'
#' @details This provides a wrapper to \code{chisq.test}, \code{cor.test}, \code{aov} from \code{stats} package to test association between two variables
#'
#'
#' @author Elyas Heidari
#'
#' @return P.value of test between the two variables.
#'
#' @export
#'
#' @examples
#' ## Preprocess the data
#' data("NHANES")
#' data <- data_preproc(NHANES, levels = 15)
#' ## Find test p.values for:
#' ## One continuous and one categorical variable
#' cont_cat_test <- test_pair(data, var1_name = "LBXTC", var2_name = "RIAGENDR")
#
#' ## Two continuous variables
#' cont_cont_test <- test_pair(data, var1_name = "LBXTC", var2_name = "LBXVIE")
#'
#' ## Two categorical variables
#' cat_cat_test <- test_pair(data, var1_name = "DIQ010", var2_name = "SMD410")
#'
#' @importFrom stats cor.test chisq.test aov


test_pair <- function(data, var1_name, var2_name, levels = NULL) {
  is.cat <- function(var) {
    if (is.null(levels))
      return(is.factor(var))
    else
      return(!length(unique(var[!is.na(var)])) > levels)
  }

  var1.is.cat <- is.cat(var1 <- data[, var1_name])
  var2.is.cat <- is.cat(var2 <- data[, var2_name])

  sample_size = min(500, nrow(data))
  try_num = as.integer(nrow(data) / sample_size) + 1

  cal_p.value <- function(var1, var2) {
    if (!var1.is.cat & !var2.is.cat) {
      ct <- stats::cor.test(var1, var2)
      ct <- ct$p.value
    }

    if (var1.is.cat & var2.is.cat) {
      ct <- stats::chisq.test(var1, var2)
      ct <- ct$p.value
    }
    if (var1.is.cat & !var2.is.cat) {
      ct <-
        stats::aov(var2 ~ var1, data = data.frame(var1 = var1 , var2 = var2))
      ct <- summary(ct)[[1]][["Pr(>F)"]][1]
    }
    if (!var1.is.cat & var2.is.cat) {
      ct <-
        stats::aov(var1 ~ var2, data = data.frame(var1 = var1 , var2 = var2))
      ct <- summary(ct)[[1]][["Pr(>F)"]][1]
    }
    return(ct)
  }
  return(mean(sapply(c(1:try_num), function(x) {
      smp <- sample(nrow(data), sample_size)
      if ((var1.is.cat && length(unique(data[smp, var1_name])) == 1) || (var2.is.cat && length(unique(data[smp, var2_name])) == 1)){
       smp <- c(1:nrow(data))
      }
    cal_p.value(data[smp, var1_name], data[smp, var2_name])
  })))
}
