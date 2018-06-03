#' @description It corrects NAN and normalizes data.
#' @import softImpute
#' @export
#'

data_preprocess <- function(data,
                            impute = T,
                            scale = T,
                            is.cont = NULL) {
  data <- data.matrix(data)
  ## to do size of is.cont compatible with dim(data)[2]
  if (length(is.cont) != 0) {
    cont <- data[, which(is.cont)]
    desc <- data[, !which(is.cont)]
  } else{
    level <- apply(data, 2 , function(x)
      length(unique(x)))
    cont <- data[, which(level > 6)]
    desc <- data[, which(level <= 6)]
  }

  if (impute) {
    softImpute(cont, rank = 100, lambda = 30) -> cont
    # softImpute(desc) -> desc
  }
  if (scale) {
    cont <- biScale(cont)
  }
  cbind(cont, desc)
}
