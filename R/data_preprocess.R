#' data preprocess
#'
#'
#' @description
#' Specify categorical and continuous variables and imputes dataset missing-values.
#'
#'
#' @param  data an arbitrary dataset.
#' @param  is.cat A list containing boolean elements related to dataset variables. (For each categorical variable it is TRUE) (Default = NA)
#' @param  levels an integer to indicate the maximum levels of categorical variables. The default is 10. (It will be used when "is.cat" list in NA)
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#'
#' @return a normalized dataframe with no missing data of continuous and (or) categorical measurements.
#' @export data.preproc
#'
#'
#'

data.preproc <- function(data,
                         is.cat = NULL,
                         levels = 10) {
  is.cat.f <- function(var) {
    !length(unique(var)) > levels
  }

  is.categorical <- function(x, is.cat) {
    x <- data.frame(x)
    x <- apply(x, 2, as.numeric)
    if (sum(is.cat) != 0) {
      if (sum(is.cat) > 1)
        binding <- apply(data.frame(x[, is.cat]) , 2, as.factor)
      else {
        binding <- data.frame(as.factor(t[, which(is.cat == T)]))
        colnames(binding) <- colnames(x)[which(is.cat == T)]
      }

      x <- data.frame(x)
      binding <- data.frame(binding)
      x <- cbind(x[, !is.cat] , binding)
    }
    x
  }

  Mode <- function(x) {
    ux <- unique(x)
    ux <- ux[which(!is.na(ux))]
    ux[which.max(tabulate(match(x, ux)))]
  }

  # a function to impute NAs in a categorial vector with the mode of the vector
  impute.factor <- function(x) {
    x <- as.factor(as.character(x))
    x[is.na(x)] = Mode(x)
    x
  }

  # a function to impute NAs in a continuous vector with the mean of the vector
  impute.continuous <- function(x) {
    x <- as.numeric(x)
    x[is.na(x)] = mean(x, na.rm = T)
    x
  }


  if (is.null(is.cat))
    data <- is.categorical(data, sapply(data, is.cat.f))
  else
    data <- is.categorical(data, is.cat)

  data.frame(lapply(data, function(x)
    if (is.numeric(x) == T)
      impute.continuous(x)
    else if (is.factor(x) == T)
      impute.factor(x)))
}
