#' Calculate Variable-wise Kullback-Leibler divergence
#'
#'
#'
#' @description
#' Calculates variable-wise Kullback-Leibler divergence between the two groups of samples.
#'
#' @param  data A numerical dataframe with no missing value.
#' @param  group1 A vector of integers. Demonstrates the row indices of group 1.
#' @param  group2 A vector of integers. Demonstrates the row indices of group 2.
#' @param  permute An integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#'
#' @details
#' The function helps users to find out the variables with the most divergence between two groups with different states of one specific variable. For instance, within a dataset of health measurements, we are interested in finding the most important variables in occurring cardiovascular disease.
#' The function is able to carry out the permutation test to calculate the p_value for each variable.
#'
#'
#' @author Elyas Heidari
#' @examples
#' data("NHANES")
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' data$SEQN <- NULL
#' # Construct two groups of samples
#' g1 <- which(data$PAD590 == 1)
#' g2 <- which(data$PAD590 == 6)
#' # Set permute to calculate p.values
#' kl <- VKL(data, group1 = g1, var2 = group2 = g2, permute = 100, levels = NULL)
#'
#' ## Using raw data
#' kl <- VKL(NHANES, group1 = g1, group2 = g2, permute = 0, levels = 15)
#'
#' @return if permute = 0 returns a dataframe including sorted Kullback-Liebler (KL) divergence. if permute > 0 returns a dataframe including p.values and sorted KL divergence.
#'
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom permute shuffle
#' @importFrom dplyr pull
#' @importFrom entropy KL.plugin
#' @importFrom dplyr %>%



VKL <- function(data,
                group1,
                group2,
                permute = 0,
                levels = NULL) {
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
    if (!is.cat(vec))
      vec <-
        cut(vec,
            breaks = seq((min(vec) - .0000001), (max(vec) + .0000001), (max(vec) - min(vec) + .0000002) /
                           lvl),
            labels = 1:lvl)
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


  data <- data.frame(data)
  group1.group2 <- c(group1, group2)
  kl <-
    kl.calc(data, group1.group2[1:length(group1)], group1.group2[(length(group1) + 1):length(group1.group2)])
  if (permute > 0) {
    kl.df <- data.frame()
    1:permute %>% purrr::map(function(x)
      permute::shuffle(group1.group2)) %>% purrr::map(function(x)
        list(group1 = x[1:length(group1)], group2 = x[(length(group1) + 1):length(x)])) %>% purrr::map(function(f)
          kl.calc(data, f[[1]], f[[2]])) %>% purrr::map(function(x)
            kl.df <<- rbind(kl.df, x)) -> na

    1:dim(kl.df)[2] %>% purrr::map(function(i)
      p.val(kl[i], kl.df[, i])) -> kls
    return(sort(data.frame(
      KL = kl,
      row.names = colnames(data),
      p.value = unlist(kls)
    ), decreasing = T))
  }

  df <- data.frame(KL = kl, row.names = colnames(data))
  return(data.frame(KL = df[order(-df$KL), ], row.names = rownames(df)))
}
