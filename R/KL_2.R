#' calculating Kullback-Leibler divergence for each categorical variable between two different groups of samples
#'
#'
#' @description
#' Calculates variable-wise Kullback-Leibler divergence between the two groups of samples which violate the linear relationship between two continuous variables.
#'
#' @param  data A numeric dataframe including no missing value
#' @param  var1 A vector of continuous values indicating the first variable. (the order of values should be the same as the order of rows in data)
#' @param  var2 A vector of continuous values indicating the second variable. (the order of values should be the same as the order of rows in data)
#' @param  permute An integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  frac A double value between 0 and 1 which indicates the fraction of outliers in the fit model. That is, the threshold to recognize a data point as an outlier of the fit line.
#' @param  levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable.
#'
#' @author  Elyas Heidari
#'
#' @return  If permute = 0 returns a dataframe including sorted Kullback-Liebler (KL) divergence. If permute > 0 returns a dataframe including p.values and sorted KL divergence.
#'
#' @export violating.vars
#'
#' @importFrom purrr map
#' @importFrom entropy KL.plugin
#' @importFrom permute shuffle
#' @importFrom dplyr %>%
#' @importFrom stats residuals
#' @importFrom utils head tail


violating.vars <- function(data, var1, var2, permute = 0, frac = 0.05, levels = 5) {

  is.cat <- function(var) {
    return(!length(unique(var[!is.na(var)])) > levels)
  }

  kl.calc <- function(data, group1, group2) {
    1:dim(data)[2] %>% map(function(x)
      freq(data[, x], group1, group2))  %>% map(function(x)
        abs(KL.plugin(x$group1, x$group2)) + abs(KL.plugin(x$group2, x$group1))) -> to.ret
    return(unlist(to.ret))
  }
  freq <- function(vec, group1, group2) {
    if (!is.cat(vec))
      vec <-
        cut(vec,
            breaks = seq((min(vec) - .0000001), (max(vec) + .0000001), (max(vec) - min(vec) + .0000002) /
                           levels),
            labels = 1:levels)
    to.ret <- list(group1 = c(), group2 = c())
    levels(factor(vec)) %>% map(function(x)
      list(group1 = max(1, sum(vec[group1] == x)), group2 = max(1, sum(vec[group2] == x)))) %>% map(function(x)
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


  lm <- lm(var1~var2)
  sm <- summary(lm)
  res <- residuals(lm)
  names(res) <- 1:length(res)
  down <- head(order(res),frac*dim(data)[1])
  up <- tail(order(res),frac*dim(data)[1])
  data <- data.frame(data)
  up.down <- c(up, down)
  kl <-
    kl.calc(data, up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)])
  if(permute > 0){
    kl.df <- data.frame()
    1:permute %>% map(function(x)
      shuffle(up.down)) %>% map(function(x)
        list(up = x[1:length(up)], down = x[(length(up) + 1):length(x)])) %>% map(function(f)
          kl.calc(data, f[[1]], f[[2]])) %>% map(function(x)
            kl.df <<- rbind(kl.df, x)) -> na

    1:dim(kl.df)[2] %>% map(function(i)
      p.val(kl[i], kl.df[, i])) -> kls
    return(sort(data.frame(KL = kl, row.names = colnames(data), p.value = unlist(kls)), decreasing = T))
  }
  return(sort(data.frame(KL = kl, row.names = colnames(data)), decreasing = T))
}



