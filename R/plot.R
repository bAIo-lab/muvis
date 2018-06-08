#' interactive visulizaiton tool for datasets.
#'
#'
#' @description
#' A function to interctively visualize one or a pair of the variables.
#'
#' @param data a dataframe. It is strongly recommended that the dataframe has no missing data and is preprocessed.
#' @param vars a vector of length one or two including the name (or index) of a (tow) column(s) of data.
#' @param levels an integer to indicate the maximum levels of categorical variables. The default is 7.
#' @param pie a logical variable. If TRUE the function returns a pie chart for one variable.
#'
#'
#' @author  Elyas Heidari
#'
#' @return  if pie = TRUE the function returns pie chart, otherwise, there may be 5 scenarios:
#' \item{}{one categorical variable: plots the histogram of the variable.}
#' \item{}{one continuous variable: plots the density plot of the variable.}
#' \item{}{one categorical and one continuous variable: plots a boxplot of the continuous variable for different levels of the categorical variable.}
#' \item{}{two continuous variables: plots a scatter plot of two variables.}
#'
#' @examples
#'
#' @export
#'
#' @importFrom dplyr pull group_by summarise summarize
#' @importFrom highcharter highchart hc_xAxis hc_add_series hc_chart hc_add_theme hc_theme_google hcpie hcboxplot hc_add_series_scatter
#' @importFrom limma strsplit2


plot <- function(data,
                 vars,
                 levels = 5,
                 pie = F) {
  is.cat <- function(var) {
    !length(unique(var)) > levels

  }
  var1 <- dplyr::pull(data[, vars[1]])
  var1.is.cat <- is.cat(var1)
  if (!pie) {
    if (length(vars) == 1) {
      if (var1.is.cat) {
        pt <- data.frame(prop.table(table(var1)))
        hc <- highcharter::highchart() %>%
          highcharter::hc_xAxis(categories = factor(pt$Var1)) %>%
          highcharter::hc_add_series(name = "Frequncy", data = pt$Freq)
        to.ret <-
          list(
            hc %>% highcharter::hc_chart(type = "column") %>% highcharter::hc_add_theme(highcharter::hc_theme_google()),
            highcharter::hcpie(vec1)
          )
      } else{
        to.ret <- highcharter::hchart(object = density(
          as.numeric(var1),
          area = TRUE,
          color = "#B71C1C",
          name = vars[1]
        )) %>% highcharter::hc_add_theme(highcharter::hc_theme_google())
      }
    } else{
      var2 <- dplyr::pull(data[, vars[2]])
      var2.is.cat <- is.cat(var2)
      if (var1.is.cat & var2.is.cat) {
        features <- data.frame(f1 = var1, f2 = var2)
        cols <- colnames(features)
        pt <- data.frame(table(as.character(interaction(features))))
        pt$t1 <- limma::strsplit2(pt$Var1, "\\.")[, 1]
        pt$t2 <- limma::strsplit2(pt$Var1, "\\.")[, 2]
        pt %>% filter(t1 != 0 & t2 != 0) -> pt
        pt %>% dplyr::group_by(t1) %>% dplyr::summarize(Freq = sum(Freq)) -> pt1
        rownames(pt1) <- pt1$t1
        # pt %>% mutate(Freq = Freq/pt1[as.character(t1),"Freq"]) -> pt2
        # pt2$Freq <- pt2$Freq[,1]
        pt2 <- pt
        to.ret <-
          highcharter::hchart(pt2, "column", hcaes(
            x = t1,
            y = Freq,
            group = t2
          )) %>% highcharter::hc_add_theme(highcharter::hc_theme_google())
      }
      if (var1.is.cat & !var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var1), f2 = var2)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(f1) %>% dplyr::summarise(m = mean(f2, na.rm = T)) -> f
        # hc <- highchart() %>%
        #   hc_xAxis(categories = 1:5) %>%
        #   hc_add_series(name = "Mean", data = f$m[2:6])
        # to.ret <-
        #   hc %>% hc_chart(type = "column") %>% hc_add_theme(hc_theme_google())
        to.ret <- highcharter::hcboxplot(x = var2, var = var1) %>% highcharter::hc_add_theme(highcharter::hc_theme_google())
      }
      if (!var1.is.cat & var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var2), f2 = var1)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(f1) %>% dplyr::summarise(m = mean(f2, na.rm = T)) -> f
        # hc <- highchart() %>%
        #   hc_xAxis(categories = 1:5) %>%
        #   hc_add_series(name = "Mean", data = f$m[2:6])
        # to.ret <-
        #   hc %>% hc_chart(type = "column") %>% hc_add_theme(hc_theme_google())
        to.ret <- highcharter::hcboxplot(x = var1, var = var2) %>% highcharter::hc_add_theme(highcharter::hc_theme_google())
      }
      if (!var1.is.cat & !var2.is.cat) {
        d <- data.frame(v1 = var1, v2 = var2)
        to.ret <-
          highcharter::highchart() %>% highcharter::hc_add_series_scatter(d$v1, d$v2) %>% highcharter::hc_add_theme(highcharter::hc_theme_google())
      }
    }
  } else{
    to.ret <- highcharter::hcpie(as.factor(var1))
  }

  to.ret
}
