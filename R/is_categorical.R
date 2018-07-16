#' Specify categorical and continuous variables in a dataset.
#'
#'
#'
#' @param  x An arbitrary dataframe
#' @param  is.cat A list containing boolean elements related to dataset variables (For each categorical variable it is TRUE)
#'
#'
#'
#' @author Elyas Heidari
#'
#'
#' @return A dataset with specified categorical and continuous variables.
#'
#' @export is.categorical
#'

is.categorical <- function(x, is.cat) {
  x <- data.frame(x)
  x <- apply(x, 2, as.numeric)
  binding <- apply(x[, is.cat] , 2, as.factor)
  x <- data.frame(x)
  binding <- data.frame(binding)
  x<- cbind(x[, !is.cat] , binding)
  x
}
