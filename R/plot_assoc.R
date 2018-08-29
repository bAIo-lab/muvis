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
#' \item{A categorical and a continuous variable}{Plots a boxplot of the continuous variable for different levels of the categorical variable.}
#' \item{Two continuous variables}{Plots a scatter plot of two variables.}
#' \item{Two categorical variables}{Plots a relative histogram showing distribution of one variable for each level of the other.}
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
#' plot_assoc(data, vars = "PAD600")
#' plot_assoc(data, vars = "SMD410", interactive = TRUE)
#'
#' ## One continuous variable
#' plot_assoc(data, vars = "LBXTC")
#' plot_assoc(data, vars = "BMXBMI", interactive = TRUE)
#'
#' ## One continuous and one categorical variable
#' plot_assoc(data, vars = c("LBXTC", "RIAGENDR"))
#' plot_assoc(data, vars = c("BMXBMI", "PAD600"), interactive = TRUE)
#
#' ##  Two continuous variables
#' plot_assoc(data, vars = c("LBXTC", "BMXBMI"))
#' plot_assoc(data, vars = c("LBXVIE", "LBXVIC"), interactive = TRUE)
#'
#' ## Two categorical variables
#' plot_assoc(data, vars = c("SMD410", "PAD600"))
#' plot_assoc(data, vars = c("PAD600", "SMD410"), interactive = TRUE)
#'
#' ## With raw data
#' plot_assoc(NHANES, vars = "RIDAGEYR", levels = 15)
#'
#' @import     ggplot2
#' @importFrom ggthemes theme_foundation
#' @importFrom scales manual_pal
#' @importFrom ggExtra ggMarginal
#' @importFrom grid unit grid.text gpar
#' @importFrom dplyr pull group_by summarise summarize filter
#' @importFrom highcharter highchart hc_xAxis hc_yAxis hc_add_series hc_chart hc_add_theme hc_theme_google hcpie hcboxplot hc_add_series_scatter hc_title hcaes
#' @importFrom limma strsplit2
#' @importFrom stats density cor.test chisq.test aov p.adjust
#' @importFrom dplyr %>%
#' @importFrom ggplotify as.ggplot

plot_assoc <- function(data,
                       vars,
                       levels = NULL,
                       interactive = FALSE) {
  is.cat <- function(var) {
    if (is.null(levels))
      return(is.factor(var))
    else
      return(!length(unique(var[!is.na(var)])) > levels)
  }

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
        pt <- data.frame(prop.table(table(var1)))
        to.ret <-
          ggplot2::ggplot(data = data, ggplot2::aes(factor(data[, vars[1]]), fill = factor(data[, vars[1]]))) +
          ggplot2::geom_bar(alpha = 0.7) +
          ggplot2::labs(title = paste("Barplot for", vars[1], sep = " ")) +
          ggplot2::labs(x = vars[1]) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.6)) + theme_Publication() + scale_fill_Publication() +
          ggplot2::guides(fill = ggplot2::guide_legend(title = vars[1]))
      } else{
        ..density.. <- NULL
        to.ret <-
          ggplot2::ggplot(data = data, ggplot2::aes(data[, vars[1]])) +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ..density..),
            binwidth  = diff(range(data[, vars[1]], na.rm = T)) / 20,
            fill = "steelblue",
            col = "grey",
            alpha = 0.7
          ) + ggplot2::geom_density(col = "gray") +
          ggplot2::labs(title = paste("Histogram for", vars[1], sep = " ")) +
          ggplot2::labs(x = vars[1]) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.6)) + theme_Publication() + scale_fill_Publication()
      }
    } else{
      var2 <- data[, vars[2]]
      var2.is.cat <- is.cat(var2)

      if (var1.is.cat & var2.is.cat) {
        lbl <-
          paste(
            "Pearson's chi-squared test p.value =",
            formatC(
              test_pair(data, vars[1], vars[2], levels = levels),
              digits = 4,
              format = "g"
            )
          )
        txt_grob <-
          grid::grid.text(
            lbl,
            x = 0.5,
            y = 0.5,
            gp = grid::gpar(
              col = "firebrick",
              fontsize = 14,
              fontface = "bold"
            ),
            draw = F
          )
        to.ret <-
          ggplot2::ggplot(data) +
          ggplot2::geom_bar(alpha = 0.7,  ggplot2::aes(x = factor(data[, vars[1]]), fill = factor(data[, vars[1]]))) +
          ggplot2::ggtitle(paste("Relative Histogram of", vars[1], "and", vars[2], collapse = " ")) +
          ggplot2::facet_grid(factor(data[, vars[2]]) ~ ., scales = "free") +
          ggplot2::labs(x = vars[1]) +
          ggplot2::theme(axis.text.x = ggplot2::element_text(vjust = 0.6))  + theme_Publication() + scale_fill_Publication() +
          ggplot2::guides(fill = "none")

        g <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(to.ret))
        strip_both <- which(grepl('strip-', g$layout$name))
        fills <- as.integer(unique(factor(data[, vars[2]])))
        k <- 1
        for (i in strip_both) {
          j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
          g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <-
            fills[k] + 1
          k <- k + 1
        }
        to.ret <- ggplotify::as.ggplot(g) +
          ggplot2::annotation_custom(txt_grob)
      }
      if (var1.is.cat & !var2.is.cat) {
        lbl <-
          paste("ANOVA test p.value =",
                formatC(
                  test_pair(data, vars[1], vars[2], levels = levels),
                  digits = 4,
                  format = "g"
                ))
        txt_grob <-
          grid::grid.text(
            lbl,
            x = 0.5,
            y = 0.8,
            gp = grid::gpar(
              col = "firebrick",
              fontsize = 14,
              fontface = "bold"
            ),
            draw = F
          )
        to.ret <-
          ggplot2::ggplot(data, ggplot2::aes(
            x = factor(var1),
            y = var2,
            col = factor(var1)
          )) +
          ggplot2::geom_boxplot() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1)) +
          ggplot2::ggtitle(paste("Boxplot for", vars[1], "and", vars[2], collapse = " ")) +
          ggplot2::labs(x = vars[1],
                        y = vars[2],
                        color = vars[1]) +
          ggplot2::annotation_custom(txt_grob) +
          theme_Publication() + scale_fill_Publication()
      }
      if (!var1.is.cat & var2.is.cat) {
        lbl <-
          paste("ANOVA test p.value =",
                formatC(
                  test_pair(data, vars[1], vars[2], levels = levels),
                  digits = 4,
                  format = "g"
                ))
        txt_grob <-
          grid::grid.text(
            lbl,
            x = 0.5,
            y = 0.8,
            gp = grid::gpar(
              col = "firebrick",
              fontsize = 14,
              fontface = "bold"
            ),
            draw = F
          )
        to.ret <-
          ggplot2::ggplot(data, ggplot2::aes(
            x = factor(var2),
            y = var1,
            col = factor(var2)
          )) +
          ggplot2::geom_boxplot() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(hjust = 1)) +
          ggplot2::ggtitle(paste("Boxplot for", vars[2], "and", vars[1], collapse = " ")) +
          ggplot2::labs(x = vars[2],
                        y = vars[1],
                        color = vars[2]) +
          ggplot2::annotation_custom(txt_grob) +
          theme_Publication() + scale_fill_Publication()
      }
      if (!var1.is.cat & !var2.is.cat) {
        lbl <-
          paste("Correlation test p.value =",
                formatC(
                  test_pair(data, vars[1], vars[2], levels = levels),
                  digits = 4,
                  format = "g"
                ))
        txt_grob <-
          grid::grid.text(
            lbl,
            x = 0.5,
            y = 0.8,
            gp = grid::gpar(
              col = "firebrick",
              fontsize = 14,
              fontface = "bold"
            ),
            draw = F
          )
        g <-
          ggplot2::ggplot(data, ggplot2::aes(x = var1, y = var2)) +
          ggplot2::geom_jitter(color = "steelblue") +
          ggplot2::geom_smooth(method = "lm",
                               se = TRUE,
                               color = "black") +
          ggplot2::ggtitle(paste("Scatter plot for", vars[1], "and", vars[2], collapse = " ")) +
          ggplot2::labs(x = vars[1],
                        y = vars[2],
                        color = vars[1]) +
          ggplot2::annotation_custom(txt_grob) +
          theme_Publication() + scale_fill_Publication()


        to.ret <-
          ggplotify::as.ggplot(
            ggExtra::ggMarginal(
              g,
              type = "histogram",
              fill = "transparent",
              color = "black"
            )
          )
      }
    }
  }
  to.ret
}
