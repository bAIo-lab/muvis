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


VVKL <-
  function(data,
           var1,
           var2,
           permute = 0,
           levels = NULL,
           plot = F,
           var1.name = "var1",
           var2.name = "var2") {
    theme_Publication <- function(base_size = 14) {
      (
        ggthemes::theme_foundation(base_size = base_size)
        + ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            size = ggplot2::rel(1.2),
            hjust = 0.5
          ),
          text = ggplot2::element_text(),
          panel.background = ggplot2::element_rect(colour = NA),
          plot.background = ggplot2::element_rect(colour = NA),
          panel.border = ggplot2::element_rect(colour = NA),
          axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
          axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
          axis.title.x = ggplot2::element_text(vjust = -0.2),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.ticks = ggplot2::element_line(),
          panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
          panel.grid.minor = ggplot2::element_blank(),
          legend.key = ggplot2::element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size = grid::unit(0.2, "cm"),
          legend.spacing = grid::unit(0, "cm"),
          legend.title = ggplot2::element_text(face = "italic"),
          plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
          strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
          strip.text = ggplot2::element_text(face = "bold")
        )
      )

    }

    scale_fill_Publication <- function(...) {
      ggplot2::discrete_scale("fill", "Publication", scales::manual_pal(
        values = c(
          "#386cb0",
          "#fdb462",
          "#7fc97f",
          "#ef3b2c",
          "#662506",
          "#a6cee3",
          "#fb9a99",
          "#984ea3",
          "#ffff33"
        )
      ), ...)

    }

    scale_colour_Publication <- function(...) {
      discrete_scale("colour",
                     "Publication",
                     scales::manual_pal(
                       values = c(
                         "#386cb0",
                         "#fdb462",
                         "#7fc97f",
                         "#ef3b2c",
                         "#662506",
                         "#a6cee3",
                         "#fb9a99",
                         "#984ea3",
                         "#ffff33"
                       )
                     ),
                     ...)
    }

    if (plot) {
      g <-
        ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2)) +
        ggplot2::geom_jitter(color = "steelblue") +
        ggplot2::geom_smooth(method = "lm",
                             se = TRUE,
                             color = "black") +
        ggplot2::ggtitle(paste("Scatter plot for", var1.name, "and", var2.name, collapse = " ")) +
        ggplot2::labs(x = var1.name,
                      y = var2.name,
                      color = var1.name) +
        theme_Publication() + scale_fill_Publication()
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

    kl.calc <- function(data, group1, group2) {
      1:dim(data)[2] %>% purrr::map(function(x)
        freq(data[, x], group1, group2))  %>% purrr::map(function(x)
          abs(entropy::KL.plugin(x$group1, x$group2)) + abs(entropy::KL.plugin(x$group2, x$group1))) -> to.ret
      return(unlist(to.ret))
    }
    freq <- function(vec, group1, group2) {
      if (!is.cat(vec)) {
        vec <-
          cut(vec,
              breaks = seq((min(vec) - .0000001),
                           (max(vec) + .0000001),
                           (max(vec) - min(vec) + .0000002) /
                             lvl
              ),
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


    lm <- lm(var2 ~ var1)
    sm <- summary(lm)
    res <- residuals(lm)
    names(res) <- 1:length(res)
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

      g <- ggExtra::ggMarginal(g,
                               type = "histogram",
                               fill = "transparent",
                               color = "black")
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

      kl <- sort(data.frame(
        KL = kl,
        row.names = colnames(data),
        p.value = unlist(kls)
      ),
      decreasing = T)

    } else {
      kl <- sort(data.frame(KL = kl, row.names = colnames(data)), decreasing = T)
    }

    if (plot) {
      return(list(kl = kl, plot = g))
    } else {
      return(kl)
    }
  }
