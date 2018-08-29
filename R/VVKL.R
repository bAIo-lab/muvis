#' Calculate Violating Variable-wise Kullback-Leibler divergence
#'
#'
#' @description
#' Calculates variable-wise Kullback-Leibler divergence between the two groups of samples which violate the linear relationship between two continuous variables.
#'
#' @param  data A numeric dataframe including no missing value
#' @param  var1 A vector of continuous values indicating the first variable. (the order of values should be the same as the order of rows in data)
#' @param  var2 A vector of continuous values indicating the second variable. (the order of values should be the same as the order of rows in data)
#' @param  permute An integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#'
#' @author  Elyas Heidari
#'
#' @return  If permute = 0 returns a dataframe including sorted Kullback-Liebler (KL) divergence. If permute > 0 returns a dataframe including p.values and sorted KL divergence.
#'
#' @export
#'
#' @examples
#' data("NHANES")
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' data$SEQN <- NULL
#' # Set permute to calculate p.values
#' kl <- VVKL(data, var1 = data$LBXTC, var2 = data$LBXVIE, permute = 100, levels = NULL)
#'
#' ## Using raw data
#' kl <- VVKL(NHANES, var1 = data$LBXTC, var2 = data$LBXVIE, permute = 0, levels = 15)
#' @importFrom purrr   map
#' @importFrom entropy KL.plugin
#' @importFrom permute shuffle
#' @importFrom dplyr %>%
#' @importFrom stats residuals
#' @importFrom utils head tail


VVKL <- function(data, var1, var2, permute = 0, levels = NULL) {

  if (!is.null(levels))
    data <- data_preproc(data, levels = levels)

  is.cat <- function(var) {
    return(is.factor(var))
  }

  if(is.null(levels))
    lvl <- max(sapply(data, nlevels))
  else
    lvl <- levels

  kl.calc <- function(data, group1, group2) {
    1:dim(data)[2] %>% purrr::map(function(x)
      freq(data[, x], group1, group2))  %>% purrr::map(function(x)
        abs(entropy::KL.plugin(x$group1, x$group2)) + abs(entropy::KL.plugin(x$group2, x$group1))) -> to.ret
    return(unlist(to.ret))
  }
  freq <- function(vec, group1, group2) {
    if (!is.cat(vec)){
      vec <-
        cut(vec,
            breaks = seq((min(vec) - .0000001), (max(vec) + .0000001), (max(vec) - min(vec) + .0000002) /
                           lvl),
            labels = 1:lvl)
    }
    to.ret <- list(group1 = c(), group2 = c())
    levels(factor(vec)) %>% purrr::map(function(x)
      list(group1 = max(1, sum(vec[group1] == x)), group2 = max(1, sum(vec[group2] == x)))) %>% purrr::map(function(x)
        to.ret <<-
          list(
            group1 = c(to.ret$group1, x$group1),
            group2 = c(to.ret$group2, x$group2)
          )) -> na
    return(to.ret)
  }

  p.val <- function(x, vec) {
    return(which(sort(vec, decreasing = T) < x)[1] / length(vec))
  }


  lm <- lm(var2~var1)
  sm <- summary(lm)
  res <- residuals(lm)
  names(res) <- 1:length(res)
  frac = .05
  down <- head(order(res),frac*dim(data)[1])
  up <- tail(order(res),frac*dim(data)[1])
  data <- data.frame(data)
  up.down <- c(up, down)
  kl <-
    kl.calc(data, up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)])
  if(permute > 0){
    kl.df <- data.frame()
    1:permute %>% purrr::map(function(x)
      shuffle(up.down)) %>% purrr::map(function(x)
        list(up = x[1:length(up)], down = x[(length(up) + 1):length(x)])) %>% purrr::map(function(f)
          kl.calc(data, f[[1]], f[[2]])) %>% purrr::map(function(x)
            kl.df <<- rbind(kl.df, x)) -> na

    1:dim(kl.df)[2] %>% purrr::map(function(i)
      p.val(kl[i], kl.df[, i])) -> kls
    return(sort(data.frame(KL = kl, row.names = colnames(data), p.value = unlist(kls)), decreasing = T))
  }
  return(sort(data.frame(KL = kl, row.names = colnames(data)), decreasing = T))
}



