#' calculating Kullback-Leibler divergence for each categorical variable between two different groups of samples.
#'
#'
#'
#' @description
#' A function to calculate Kullback-Leibler divergence for each categorical variable between two groups of samples which have different values of a binary-valued variable.
#'
#' @param  data a dataframe of categorical variables.
#' @param  var a character string or an integer indicating the column of the variable of interest. Note that the column should be of two levels (binary-valued).
#' @param permute an integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
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
#' @examples
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom permute shuffle
#' @importFrom dplyr pull
#' @importFrom entropy KL.plugin


div <- function(data, var, permute = 0) {
  kl.calc <- function(up, down, data) {
    up <- data[up, 1:dim(data)[2]]
    down <- data[down, 1:dim(data)[2]]
    up.freq <- do.call(cbind, apply(up , 2, leveler))
    down.freq <- do.call(cbind, apply(down , 2, leveler))
    1:dim(up)[2] %>% purrr::map(function(x)
      abs(entropy::KL.plugin(up.freq[,x], down.freq[,x])) + abs(entropy::KL.plugin(down.freq[,x], up.freq[,x]))) -> kl.up.down
    unlist(kl.up.down)
  }
  leveler <- function(vec) {
    tb <- table(factor(vec))
    pmax(tb, 1)
  }
  p.val <- function(x, vec) {
    print(x)
    print(vec)
    which(sort(vec, decreasing = T) < x)[1] / length(vec)
  }
  var.vec <- dplyr::pull(data[,var])
  down <- which(var.vec == unique(var.vec)[1])
  up <- which(var.vec == unique(var.vec)[2])

  data <- data[, colSums(data.matrix(data), na.rm = T) > 5000]
  data <- data.frame(data)
  up.down <- c(up, down)

  kl <-
    kl.calc(up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)], data)
  if(permute > 0){
  kl.df <- data.frame()
  1:100 %>% purrr::map(function(x)
    permute::shuffle(up.down)) %>% purrr::map(function(x)
      list(up = x[1:length(up)], down = x[(length(up) + 1):length(x)])) %>% purrr::map(function(f)
        kl.calc(f[[1]], f[[2]], data)) %>% purrr::map(function(x)
          kl.df <<- rbind(kl.df, x)) -> na

  1:dim(kl.df)[2] %>% purrr::map(function(i)
    p.val(kl[i], kl.df[, i])) -> kls
  print(kls)
  return(data.frame(KL = kl, row.names = colnames(data), p.value = unlist(kls)))
  }
  return(data.frame(KL = kl, row.names = colnames(data)))
}




