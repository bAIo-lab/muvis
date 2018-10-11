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
#' @param  plot A logical indicating if the scatter plot of two variables is returned. It highlights the two outlier groups with different colors.
#' @param  var1.name The name of the first variable to be shown on the plot.
#' @param  var2.name The name of the second variable to be shown on the plot.
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
#' ## Using raw data and plot
#' kl <- VVKL(NHANES, var1 = data$LBXTC, var2 = data$LBXVIE, permute = 0, levels = 15,
#' plot = TRUE, var1.name = 'LBXTC', var2.name = 'LBXVIE')
#' @importFrom purrr   map
#' @importFrom entropy KL.plugin
#' @importFrom permute shuffle
#' @importFrom dplyr %>%
#' @importFrom stats residuals
#' @importFrom utils head tail
#' @import     ggplot2
#' @importFrom ggthemes theme_foundation
#' @importFrom scales manual_pal
#' @importFrom ggExtra ggMarginal
#' @importFrom grid unit
#' @importFrom ggplotify as.ggplot


VVKL <-
  function(data,
           var1,
           var2,
           permute = 0,
           levels = NULL,
           plot = F,
           var1.name = "var1",
           var2.name = "var2") {

    color_set <- c("#440154FF",
                   "#3B528BFF",
                   "#FDE725FF",
                   "#21908CFF",
                   "#5DC863FF"
    )
    if (plot) {
      g <-
        ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2)) +
        ggplot2::geom_jitter(color = color_set[2]) +
        ggplot2::geom_smooth(method = "lm",
                             se = TRUE,
                             color = 'black') +
        ggplot2::ggtitle(paste("Scatter plot for", var1.name, "and", var2.name, collapse = " ")) +
        ggplot2::labs(x = var1.name,
                      y = var2.name,
                      color = var1.name) +
        theme_minimal()
    }

    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)

    is.cat <- function(var) {
      return(is.factor(var))
    }

    if (is.null(levels))
      lvl <- max(sapply(data, nlevels))
    else
      lvl <- levels

    # levels of continuous variables density is hardcoded
    lvl = 20
    kl.calc <- function(data, group1, group2) {
      1:dim(data)[2] %>% purrr::map(function(x)
        kl.calc.vec(data[, x], group1, group2)) -> to.ret
      return(unlist(to.ret))
    }
    kl.calc.vec <- function(vec, group1, group2) {
      freqs <- list(group1 = c(), group2 = c())
      if (!is.cat(vec)) {
        rangee <-
          c(min(vec[group1], vec[group2]), max(vec[group1], vec[group2]))
        freqs$group1 <-
          entropy::discretize(vec[group1], lvl, r = rangee)
        freqs$group2 <-
          entropy::discretize(vec[group2], lvl, r = rangee)
        freqs$group1 <-
          replace(x = freqs$group1,
                  list = which(freqs$group1 == 0),
                  1)
        freqs$group2 <-
          replace(x = freqs$group2,
                  list = which(freqs$group2 == 0),
                  1)
      } else{
        levels(factor(vec)) %>% purrr::map(function(x)
          list(group1 = max(1, sum(vec[group1] == x)), group2 = max(1, sum(vec[group2] == x)))) %>% purrr::map(function(x)
            freqs <<-
              list(
                group1 = c(freqs$group1, x$group1),
                group2 = c(freqs$group2, x$group2)
              )) -> na
      }
      kl1 <- entropy::KL.plugin(freqs$group1, freqs$group2)
      kl2 <- entropy::KL.plugin(freqs$group2, freqs$group1)
      if (kl1 == Inf)
        return (abs(kl2 / 2))
      else if (kl2 == Inf)
        return (abs(kl1 / 2))
      return((abs(
        entropy::KL.plugin(freqs$group1, freqs$group2)
      ) + abs(
        entropy::KL.plugin(freqs$group2, freqs$group1)
      )) / 2)
    }
    p.val <- function(x, vec) {
      return(which(sort(vec, decreasing = T) < x)[1] / length(vec))
    }



    lm <- lm(var2 ~ var1)
    sm <- summary(lm)
    res <- residuals(lm)
    names(res) <- 1:length(res)

    # frac is hardcoded
    frac = .05
    down <- head(order(res), frac * dim(data)[1])
    up <- tail(order(res), frac * dim(data)[1])

    if (plot) {
      g <-
        g + ggplot2::geom_point(data = data[up, ],
                                ggplot2::aes(x = var1[up], y = var2[up]),
                                color = 'lightcoral') +
        ggplot2::geom_point(data = data[down, ],
                            ggplot2::aes(x = var1[down], y = var2[down]),
                            color = 'springgreen3')

      g <- ggplotify::as.ggplot(ggExtra::ggMarginal(g,
                               type = "histogram",
                               fill = "transparent",
                               color = 'black'))
    }

    data <- data.frame(data)
    up.down <- c(up, down)
    kl <-
      kl.calc(data, up.down[1:length(up)], up.down[(length(up) + 1):length(up.down)])
    if (permute > 0) {
      kl.df <- data.frame()
      1:permute %>% purrr::map(function(x)
        permute::shuffle(up.down)) %>% purrr::map(function(x)
          list(up = x[1:length(up)], down = x[(length(up) + 1):length(x)])) %>% purrr::map(function(f)
            kl.calc(data, f[[1]], f[[2]])) %>% purrr::map(function(x)
              kl.df <<- rbind(kl.df, x)) -> na

      1:dim(kl.df)[2] %>% purrr::map(function(i)
        p.val(kl[i], kl.df[, i])) -> kls

      df <- data.frame(
        KL = kl,
        row.names = colnames(data),
        variable = colnames(data),
        p.value = unlist(kls)
      )
      kl <- df[order(-df$KL), ]

    } else {
      df <- data.frame(KL = kl,
                       variable = colnames(data),
                       row.names = colnames(data))
      kl <- df[order(-df$KL), ]
    }

    if (plot) {
      return(list(kl = kl, plot = g))
    } else {
      return(kl)
    }
  }
