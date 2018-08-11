#' preprocess the data
#'
#'
#' @description
#' Specify categorical and continuous variables and impute the missing values.
#'
#'
#' @param  data An arbitrary dataset (For example data.frame or matrix).
#' @param  is.cat A boolean list specifies which variables are categorical. (default = NULL)
#' @param  levels An integer number indicates the maximum levels of categorical variables. It is used when \code{is.cat} in NULL. (default = 5)
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @examples
#' ## Using levels
#' data("Nhanes")
#' data_preproc(Nhanes, levels = 15)
#'
#' ## Using is.cat
#' require(datasets)
#' data("mtcars")
#' l <- logical(11)
#' l[c(8, 9)] <- TRUE
#' data_preproc(mtcars, is.cat = l)
#'
#' @return A normalized data.frame object with specified continuous and (or) categorical variables and no missing values.
#' @export
#'

data_preproc <- function(data,
                         is.cat = NULL,
                         levels = 5) {
  is.cat.function <- function(var) {
    return(!length(unique(var[!is.na(var)])) > levels)
  }

  cont.cat.spec <- function(x, is.cat) {
    x <- data.frame(x)
    ls <- c(1:ncol(x))
    t <- sapply(ls, function(i)
      (!(is.numeric(x[, i]) | is.cat[i])))

    if (sum(t) != 0) {
      ls <- ls[t]
      x <- x[-ls]
      is.cat <- is.cat[-ls]
    }

    x[, is.cat] <- data.frame(sapply(x[, is.cat], as.factor))
    x <- data.frame(x)
    x <- sapply(x, as.numeric)
    x <- data.frame(x)
    if (sum(is.cat) != 0) {
      if (sum(is.cat) > 1)
        binding <- apply(data.frame(x[, is.cat]) , 2, as.factor)
      else {
        binding <- data.frame(as.factor(t[, which(is.cat == T)]))
        colnames(binding) <- colnames(x)[which(is.cat == T)]
      }

      x <- data.frame(x)
      binding <- data.frame(binding)
      x <- cbind(x[,!is.cat] , binding)
    }
    x
  }

  Mode <- function(x) {
    ux <- unique(x)
    ux <- ux[which(!is.na(ux))]
    ux[which.max(tabulate(match(x, ux)))]
  }

  impute.factor <- function(x) {
    x <- as.factor(as.character(x))
    x[is.na(x)] = Mode(x)
    x
  }

  impute.continuous <- function(x) {
    x <- as.numeric(x)
    x[is.na(x)] = mean(x, na.rm = T)
    x
  }


  data <- data.frame(data)

  # Specify categorical and continuous variables
  if (is.null(is.cat))
    data <- cont.cat.spec(data, sapply(data, is.cat.function))
  else
    data <- cont.cat.spec(data, is.cat)

  # Impute the dataset
  data.frame(lapply(data, function(x)
    if (is.numeric(x) == T)
      impute.continuous(x)
    else if (is.factor(x) == T)
      impute.factor(x)))
}
