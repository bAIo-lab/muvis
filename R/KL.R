#' calculate variable-wise Kullback-Leibler divergence
#'
#'
#'
#' @description
#' Calculates variable-wise Kullback-Leibler divergence between two groups of samples.
#'
#' @param  data a numerical dataframe with now missing value
#' @param  g1 a vector of integers. Demonstrates the row indices of group 1.
#' @param  g2 a vector of integers. Demonstrates the row indices of group 2.
#' @param  permute an integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  levels an integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish categorical variable.
#'
#' @details
#' The function helps users to find out the variables with the most divergence between two groups with different states of one specific variable. For instance, within a dataset of health measurements, we are interested in finding the most important variables in occurring cardiovascular disease.
#' The function is able to carry out permutation test to calculate the p_value for each variable.
#'
#'
#' @author Elyas Heidari
#'
#'
#' @return if permute = 0 returns a dataframe including Kullback-Liebler (KL) divergence. if permute > 0 returns a dataframe including KL divergence and p.values.
#'
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom permute shuffle
#' @importFrom dplyr pull
#' @importFrom entropy KL.plugin
#' @importFrom magrittr %>%


div <- function(data,
                g1,
                g2,
                permute = 0,
                levels = 5) {
  is.cat <- function(var) {
    !length(unique(var[!is.na(var)])) > levels
  }
  kl.calc <- function(data, g1, g2) {
    1:dim(data)[2] %>% map(function(x)
      freq(data[, x], g1, g2))  %>% map(function(x)
        abs(KL.plugin(x$g1, x$g2)) + abs(KL.plugin(x$g2, x$g1))) -> to.ret
    unlist(to.ret)
  }
  freq <- function(vec, g1, g2) {
    if (!is.cat(vec))
      vec <-
        cut(vec,
            breaks = seq((min(vec) - .0000001), (max(vec) + .0000001), (max(vec) - min(vec) + .0000002) /
                           levels),
            labels = 1:levels)
    to.ret <- list(g1 = c(), g2 = c())
    levels(factor(vec)) %>% map(function(x)
      list(g1 = max(1, sum(vec[g1] == x)), g2 = max(1, sum(vec[g2] == x)))) %>% map(function(x)
        to.ret <<-
          list(
            g1 = c(to.ret$g1, x$g1),
            g2 = c(to.ret$g2, x$g2)
          )) -> na
    to.ret
  }
  p.val <- function(x, vec) {
    which(sort(vec, decreasing = T) < x)[1] / length(vec)
  }
  data <- data.frame(data)
  g1.g2 <- c(g1, g2)
  kl <-
    kl.calc(data, g1.g2[1:length(g1)], g1.g2[(length(g1) + 1):length(g1.g2)])
  if (permute > 0) {
    kl.df <- data.frame()
    1:permute %>% map(function(x)
      shuffle(g1.g2)) %>% map(function(x)
        list(g1 = x[1:length(g1)], g2 = x[(length(g1) + 1):length(x)])) %>% map(function(f)
          kl.calc(data, f[[1]], f[[2]])) %>% map(function(x)
            kl.df <<- rbind(kl.df, x)) -> na

    1:dim(kl.df)[2] %>% map(function(i)
      p.val(kl[i], kl.df[, i])) -> kls
    return(data.frame(
      KL = kl,
      row.names = colnames(data),
      p.value = unlist(kls)
    ))
  }
  return(data.frame(KL = kl, row.names = colnames(data)))
}
