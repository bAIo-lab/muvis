#' preprocess the data
#'
#'
#' @description
#' Specify categorical and continuous variables and impute the missing values.
#'
#'
#' @param  data A data.frame object or a matrix.
#' @param  is.cat A boolean vector specifies which variables are categorical. (default = NULL)
#' @param  levels An integer number indicates the maximum levels of categorical variables. It is used when \code{is.cat} in NULL. (default = 5)
#' @param  detect.outliers Logical indicating if data outliers should be detected. If TRUE outliers will be treated as NA. Defaults to FALSE.
#' @param  alpha A number between (0, 1). Rows where the ratio of the NA values in them is more than alpha will be deleted.
#' @param  ... Any additional arguments.
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @section Additional arguments:
#' \describe{
#' \item{beta}{The level of statistical significance with which to accept or reject outliers. The argument of \code{\link[AnomalyDetection]{AnomalyDetectionVec}} function. Defaults to 0.5.}
#' }
#' @examples
#' ## Using levels
#' data("NHANES")
#' df <- data_preproc(NHANES, levels = 15)
#'
#' ## Using is.cat
#' require(datasets)
#' data("mtcars")
#' l <- logical(11)
#' l[c(8, 9)] <- TRUE
#' df <- data_preproc(mtcars, is.cat = l)
#'
#' ## Detect outliers
#' df <- data_preproc(NHANES, levels = 15, detect.outliers = TRUE, alpha = 0.4, beta = 1)
#'
#' @return A normalized data.frame object with specified continuous and (or) categorical variables and no missing values.
#' @export
#'
#' @importFrom AnomalyDetection AnomalyDetectionVec
#'

data_preproc <- function(data,
                         is.cat = NULL,
                         levels = 5,
                         detect.outliers = FALSE,
                         alpha = 0.5,
                         ...) {
  args <- list(...)
  beta = args$beta
  if (is.null(beta))
    beta = 0.5
  df_anomaly_detector <- function(df) {
    df <- data.frame(df)
    column_anom_detect <- function(x) {
      index <- c(1:length(x))
      col_df <- data.frame(index, x)

      na_row <- apply(col_df, 1, function(x)
        is.na(x[2]))
      na_df <- col_df[na_row,]
      col_df <- col_df[!na_row,]
      col_df <- col_df[order(col_df[, 2]),]
      anm <- AnomalyDetection::AnomalyDetectionVec(
        x = col_df[, 2],
        period = nrow(col_df) / 5,
        plot = F,
        direction = "both",
        alpha = beta
      )
      col_df[anm$anoms$index, 2] <- NA

      col_df <- rbind(col_df, na_df)

      return(col_df[order(col_df[, 1]), 2])
    }

    return(data.frame(sapply(df, column_anom_detect)))

  }

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

  # Set outlier to NA
  if (detect.outliers) {
    cont_vars <- sapply(data, function(x)! is.factor(x))
    data[, cont_vars] <- df_anomaly_detector(data[, cont_vars])
  }

  # Delete rows with more NA ratio more than alpha
  data <-
    data[apply(data, 1, function(x)
      (sum(is.na(x)) / length(x)) <= alpha), ]

  # Impute the dataset
  data.frame(lapply(data, function(x)
    if (is.numeric(x) == T)
      impute.continuous(x)
    else if (is.factor(x) == T)
      impute.factor(x)))
}
