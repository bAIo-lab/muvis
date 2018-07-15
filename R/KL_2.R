#' calculating Kullback-Leibler divergence for each categorical variable between two different groups of samples.
#'
#'
#' @description
#' Calculates variable-wise Kullback-Leibler divergence between two groups of samples which violate the linear relationship between two continuous variables.
#'
#' @param  data a dataframe of categorical variables.
#' @param  var1 a vector of continuous values indicating the first variable. (the order of values should be the same as the order of rows in data)
#' @param  var2 a vector of continuous values indicating the second variable. (the order of values should be the same as the order of rows in data)
#' @param  permute an integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  frac a double value between 0 and 1 which indicates the fraction of outliers in the fit model. That is, the threshold to recognize a datapoint as an outlier of the fit line.
#'
#'
#' @author  Elyas Heidari
#'
#' @return  if permute = 0 returns a dataframe including Kullback-Liebler (KL) divergence. if permute > 0 returns a dataframe including KL divergence and p.values.
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom entropy KL.plugin
#' @importFrom permute shuffle
#' @importFrom magrittr %>%
#' @importFrom stats residuals
#' @importFrom utils head tail


div2 <- function(data,
                 var1,
                 var2,
                 permute = 0,
                 frac = 0.05) {
  kl.calc <- function(g1, g2, data) {
    1:dim(data)[2] %>% purrr::map(function(x)
      freq(data[, x], g1, g2)) %>% purrr::map(function(x)
        abs(entropy::KL.plugin(x$g1, x$g2)) + abs(entropy::KL.plugin(x$g2, x$g1))) -> to.ret
    unlist(to.ret)
  }
  freq <- function(vec, g1, g2) {
    to.ret <- list(g1 = c(), g2 = c())
    levels(factor(vec)) %>% purrr::map(function(x)
      list(g1 = max(1, sum(vec[g1] == x)), g2 = max(1, sum(vec[g2] == x)))) %>% purrr::map(function(x)
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
  lm <- lm(var1 ~ var2)
  sm <- summary(lm)
  res <- stats::residuals(lm)
  names(res) <- 1:length(res)
  down <- utils::head(order(res), frac * dim(data)[1])
  up <- utils::tail(order(res), frac * dim(data)[1])
  data <- data[, colSums(data.matrix(data), na.rm = T) > 5000]
  data <- data.frame(data)
  up.down <- c(up, down)
  kl <-
    kl.calc(up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)], data)
  if (permute > 0) {
    kl.df <- data.frame()
    1:100 %>% map(function(x)
      permute::shuffle(up.down)) %>% purrr::map(function(x)
        list(up = x[1:length(up)], down = x[(length(up) + 1):length(x)])) %>% purrr::map(function(f)
          kl.calc(f[[1]], f[[2]], data)) %>% purrr::map(function(x)
            kl.df <<- rbind(kl.df, x)) -> na

    1:dim(kl.df)[2] %>% purrr::map(function(i)
      p.val(kl[i], kl.df[, i])) -> kls
    return(data.frame(
      KL = kl,
      row.names = colnames(data),
      p.value = unlist(kls)
    ))
  }
  return(data.frame(KL = kl, row.names = colnames(data)))
}
