#' Plot variables.
#'
#'
#' @description
#' Plots one or a pair of variables (non) interactively using ggplot2 and highcharter packages.
#'
#' @param data A dataframe. It is strongly recommended that the dataframe has no missing data and is preprocessed.
#' @param vars A vector of length one or two including the name (or index) of a (row) column(s) of data.
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' @param interactive Logical indicating if the output should be interactive. Defaults to FALSE.
#'
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @return There may be 5 scenarios for vars:
#' \item{One categorical variable}{Plots the barplot of the variable.}
#' \item{One continuous variable}{Plots the density (histogram) plot of the variable.}
#' \item{A categorical and a continuous variable}{Plots a Boxplot (or violin plot in non-interactive mode) of the continuous variable for different levels of the categorical variable.}
#' \item{Two continuous variables}{Plots a scatter plot of two variables.}
#' \item{Two categorical variables}{Plots a relative histogram (or heatmap in non-interactive mode) showing distribution of one variable for each level of the other.}
#' (Plots interactively If interactive = TRUE).
#'
#' @export
#'
#' @examples
#' ## Preprocess the data
#' data("NHANES")
#' data <- data_preproc(NHANES, levels = 15)
#'
#' ## Plot (non)interactive for:
#' ## One categorical variable
#' pt1 <- plot_assoc(data, vars = "PAD600")
#' pt2 <- plot_assoc(data, vars = "SMD410", interactive = TRUE)
#'
#' ## One continuous variable
#' pt3 <- plot_assoc(data, vars = "LBXTC")
#' pt4 <- plot_assoc(data, vars = "BMXBMI", interactive = TRUE)
#'
#' ## One continuous and one categorical variable
#' pt5 <- plot_assoc(data, vars = c("LBXTC", "RIAGENDR"))
#' pt6 <- plot_assoc(data, vars = c("BMXBMI", "PAD600"), interactive = TRUE)
#
#' ##  Two continuous variables
#' pt7 <- plot_assoc(data, vars = c("LBXTC", "BMXBMI"))
#' pt8 <- plot_assoc(data, vars = c("LBXVIE", "LBXVIC"), interactive = TRUE)
#'
#' ## Two categorical variables
#' pt9 <- plot_assoc(data, vars = c("SMD410", "PAD600"))
#' pt10 <- plot_assoc(data, vars = c("PAD600", "SMD410"), interactive = TRUE)
#'
#' ## With raw data
#' pt11 <- plot_assoc(NHANES, vars = "RIDAGEYR", levels = 15)
#'
#' @import     ggplot2
#' @importFrom ggExtra ggMarginal
#' @importFrom dplyr pull group_by summarise summarize filter
#' @importFrom highcharter highchart hc_xAxis hc_yAxis hc_add_series hc_chart hc_add_theme hc_theme_google hcpie hcboxplot hc_add_series_scatter hc_title hcaes
#' @importFrom limma strsplit2
#' @importFrom stats density
#' @importFrom dplyr %>%
#' @importFrom ggplotify as.ggplot
#' @importFrom limma strsplit2
#' @importFrom leaflet colorNumeric
#' @importFrom ggbeeswarm geom_quasirandom
#' @importFrom superheat superheat

plot_assoc <- function(data,
                       vars,
                       levels = NULL,
                       interactive = FALSE) {
  color_set <- c("#440154FF",
                 "#3B528BFF",
                 "#FDE725FF",
                 "#21908CFF",
                 "#5DC863FF"
                 )

  theme_publication <- theme_minimal

  is.cat <- function(var) {
    if (is.null(levels))
      return(is.factor(var))
    else
      return(!length(unique(var[!is.na(var)])) > levels)
  }

  # Assuming cat_var is a categorical but it is not factor in general
  plot_cat <- function(data, cat_var) {
    var_vect <- as.factor(data[, cat_var])
    pal <-
      leaflet::colorNumeric(color_set, domain = c(1:nlevels(var_vect)))
    g <-
      ggplot2::ggplot(data = data, ggplot2::aes(var_vect, fill = var_vect)) +
      ggplot2::geom_bar(alpha = 0.7) +
      ggplot2::labs(title = paste("Barplot for", cat_var, sep = " "),
                    x = cat_var) +
      scale_fill_manual(name = cat_var, values = sapply(c(1:nlevels(var_vect)), pal)) +
      theme_publication()
    return(g)
  }
  # Assuming cont_var is a continuous
  plot_cont <- function(data, cont_var) {
    ..density.. <- NULL
    var_vect <- data[, cont_var]
    g <-
      ggplot2::ggplot(data = data, ggplot2::aes(var_vect)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ..density..),
        binwidth  = diff(range(var_vect, na.rm = T)) / 20,
        fill = color_set[4],
        col = color_set[1],
        alpha = 0.7
      ) + ggplot2::geom_density(col = color_set[2]) +
      ggplot2::labs(title = paste("Histogram for", cont_var, sep = " "),
                    x = cont_var) +
      theme_publication()
    return(g)
  }

  plot_cont_cat <- function(data, cont_var, cat_var) {
    cat_vect <- as.factor(data[, cat_var])
    cont_vect <- data[, cont_var]
    pal <-leaflet::colorNumeric(color_set, domain = c(1:nlevels(cat_vect)))
    g <-
      ggplot2::ggplot(data,
                      ggplot2::aes(x = cat_vect,
                                   y = cont_vect
                                   )) +
      ggplot2::geom_boxplot(aes(col = 'Boxplot')) + ggbeeswarm::geom_quasirandom(aes(col=cat_vect)) +
      ggplot2::ggtitle(paste("Boxplot for", cont_var, "and", cat_var, collapse = " ")) +
      ggplot2::labs(x = cat_var,
                    y = cont_var,
                    color = cat_var) +
      scale_color_manual(values = as.vector(c(sapply(c(1:nlevels(cat_vect)), pal), 'grey'))) +
      theme_publication()
  }

  plot_cont_cont <- function(data, var1, var2) {
    vec1 <- data[, var1]
    vec2 <- data[, var2]
    g <-
      ggplot2::ggplot(data, ggplot2::aes(x = vec1, y = vec2)) +
      ggplot2::geom_jitter(color = color_set[2]) +
      ggplot2::geom_smooth(method = "lm",
                           se = TRUE,
                           color = 'black') +
      ggplot2::ggtitle(paste("Scatter plot for", var1, "and", var2, collapse = " ")) +
      ggplot2::labs(x = var1,
                    y = var2,
                    color = var1) +
      theme_publication()


    to.ret <-
      ggplotify::as.ggplot(
        ggExtra::ggMarginal(
          g,
          type = "histogram",
          fill = "transparent",
          color = 'black'
        )
      )
  }

  plot_cat_cat <- function(data, var1, var2) {
    t <- table(data[, var1], data[, var2])
    m <- mean(t)
    rownames(t) <-
      sapply(c(1:nlevels(data[, var1])), function(x)
        paste(var1, '_', x, sep = ""))
    colnames(t) <-
      sapply(c(1:nlevels(data[, var2])), function(x)
        paste(var2, '_', x, sep = ""))
    x <- limma::strsplit2(rownames(t), "_")[1, 1]
    y <- limma::strsplit2(colnames(t), "_")[1, 1]
    rownames(t) <- strsplit2(rownames(t), "_")[, 2]
    colnames(t) <- strsplit2(colnames(t), "_")[, 2]

    t.col <- t < m
    # set all values that satisfy the condition to "white"
    t.col <- gsub("TRUE", "white", t.col)
    # set all values that do not satisfy the condition to "black"
    t.col <- gsub("FALSE", "black", t.col)
    t.col <- matrix(t.col, nrow = dim(t)[1])

    g <- superheat::superheat(
      t,
      grid.hline.col = 'white',
      grid.vline.col = 'white',
      X.text = t,
      left.label.col = 'white',
      bottom.label.col = 'white',
      row.title = x,
      column.title = y,
      X.text.col = t.col,
      legend = F,
      title = paste("Heatmap of", var1, "and", var2, collapse = " ")

    )
    return(g)
  }

  var1 <- data[, vars[1]]
  var1.is.cat <- is.cat(var1)

  if (interactive) {
    if (length(vars) == 1) {
      if (var1.is.cat) {
        pt <- data.frame(prop.table(table(var1)))
        hc <- highcharter::highchart() %>%
          highcharter::hc_xAxis(categories = factor(pt$Var1)) %>%
          highcharter::hc_add_series(name = "Frequncy", data = pt$Freq)
        to.ret <-
          hc %>% highcharter::hc_chart(type = "column") %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Histogram of", vars[1])) %>% highcharter::hc_xAxis(title = list(text = vars[1]))
      } else{
        to.ret <- highcharter::hchart(object = stats::density(
          as.numeric(var1),
          area = TRUE,
          color = "#B71C1C",
          name = vars[1]
        )) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Density of", vars[1])) %>% highcharter::hc_xAxis(title = list(text = vars[1]))
      }
    } else{
      var2 <- data[, vars[2]]
      var2.is.cat <- is.cat(var2)
      if (var1.is.cat & var2.is.cat) {
        features <- data.frame(f1 = var1, f2 = var2)
        cols <- colnames(features)
        pt <- data.frame(table(as.character(interaction(features))))
        pt$t1 <- limma::strsplit2(pt$Var1, "\\.")[, 1]
        pt$t2 <- limma::strsplit2(pt$Var1, "\\.")[, 2]
        pt %>% dplyr::filter(pt$t1 != 0 & pt$t2 != 0) -> pt
        pt %>% dplyr::group_by(pt$t1) %>% dplyr::summarize(Freq = sum(pt$Freq)) -> pt1
        t1 <- NULL
        Freq <- NULL
        t2 <- NULL
        pt2 <- pt
        to.ret <-
          highcharter::hchart(pt2,
                              "column",
                              highcharter::hcaes(
                                x = t1,
                                y = Freq,
                                group = t2
                              )) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Relative histogram of", paste(vars[1], vars[2], sep = "-"))) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = "number"))
      }
      if (var1.is.cat & !var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var1), f2 = var2)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(features$f1) %>% dplyr::summarise(m = mean(features$f2, na.rm = T)) -> f
        to.ret <-
          highcharter::hcboxplot(x = var2, var = var1) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Boxplot of", vars[2])) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = vars[2]))
      }
      if (!var1.is.cat & var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var2), f2 = var1)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(features$f1) %>% dplyr::summarise(m = mean(features$f2, na.rm = T)) -> f
        to.ret <-
          highcharter::hcboxplot(x = var1, var = var2) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Boxplot of", vars[1])) %>% highcharter::hc_xAxis(title = list(text = vars[2])) %>% highcharter::hc_yAxis(title = list(text = vars[1]))
      }
      if (!var1.is.cat & !var2.is.cat) {
        d <- data.frame(v1 = var1, v2 = var2)
        v1 <- NULL
        v2 <- NULL
        to.ret <-
          highcharter::hchart(d, "scatter", highcharter::hcaes(x = v1, y = v2)) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Scatter plot of", paste(vars[1], vars[2], sep = "-"))) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = vars[2]))
      }
    }
  } else {
    if (length(vars) == 1) {
      if (var1.is.cat) {
        to.ret <- plot_cat(data, vars[1])
      } else{
        to.ret <- plot_cont(data, vars[1])
      }
    } else{
      var2 <- data[, vars[2]]
      var2.is.cat <- is.cat(var2)

      if (var1.is.cat & var2.is.cat) {
        to.ret <- plot_cat_cat(data, vars[1], vars[2])
      }
      if (var1.is.cat & !var2.is.cat) {
        to.ret <- plot_cont_cat(data, cont_var = vars[2], cat_var = vars[1])
      }
      if (!var1.is.cat & var2.is.cat) {
        to.ret <- plot_cont_cat(data, cont_var = vars[1], cat_var = vars[2])
      }
      if (!var1.is.cat & !var2.is.cat) {
        to.ret <- plot_cont_cont(data, vars[1], vars[2])
      }
    }
  }
  to.ret
}
