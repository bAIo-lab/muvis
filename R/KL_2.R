#' calculating Kullback-Leibler divergence for each categorical variable between two different groups of samples.
#'
#'
#' @description
#' A function to calculate Kullback-Leibler divergence for each categorical variable between two groups of samples which do not follow the linear association between two continuous variables.
#'
#' @param  data a dataframe of categorical variables.
#' @param  var1 a vector of continuous values indicating the first variable.
#' @param  var2 a vector of continuous values indicating the second variable.
#' @param  permute an integer indicating the number of permutations for permutation test. If 0 (the default) no permutation test will be carried out.
#' @param  frac a double value between 0 and 1 which indicates the fraction of outliers in the fit model. That is, the threshold to recognize a datapoint as an outlier of the fit line.
#'
#'
#' @author  Elyas Heidari
#'
#' @return  if permute = 0 returns a dataframe including Kullback-Liebler (KL) divergence. if permute > 0 returns a dataframe including KL divergence and p.values.
#'
#' @examples
#' TODO
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom entropy KL.plugin
#' @importFrom permute shuffle

div2 <- function(data, var1, var2, permute = 0, frac = 0.05) {
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
  lm <- lm(var1~var2)
  sm <- summary(lm)
  res <- residuals(lm)
  names(res) <- 1:length(res)
  down <- head(order(res),frac*dim(data)[1])
  up <- tail(order(res),frac*dim(data)[1])
  data <- data[, colSums(data.matrix(data), na.rm = T) > 5000]
  data <- data.frame(data)
  up.down <- c(up, down)
  kl <-
    kl.calc(up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)], data)
  if(permute > 0){
    kl.df <- data.frame()
    1:100 %>% map(function(x)
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
