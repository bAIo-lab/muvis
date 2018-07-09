#' interactive plot
#'
#'
#' @description
#' Plots one or a pair of variables.
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
#' \item{}{two categorical variables: plots a relative histogram showing distribution of one variable for each level of the other.}
#'
#' @export
#'
#' @importFrom dplyr pull group_by summarise summarize filter
#' @importFrom highcharter highchart hc_xAxis hc_yAxis hc_add_series hc_chart hc_add_theme hc_theme_google hcpie hcboxplot hc_add_series_scatter hc_title hcaes
#' @importFrom limma strsplit2
#' @importFrom stats density


plot <- function(data,
                 vars,
                 levels = 5,
                 pie = F) {
  is.cat <- function(var) {
    !length(unique(var)) > levels

  }
  var1 <- data[, vars[1]]
  var1.is.cat <- is.cat(var1)
  if (!pie) {
    if (length(vars) == 1) {
      if (var1.is.cat) {
        pt <- data.frame(prop.table(table(var1)))
        hc <- highcharter::highchart() %>%
          highcharter::hc_xAxis(categories = factor(pt$Var1)) %>%
          highcharter::hc_add_series(name = "Frequncy", data = pt$Freq)
        to.ret <-
          hc %>% highcharter::hc_chart(type = "column") %>% highcharter::hc_add_theme(highcharter::hc_theme_google())%>%
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
        rownames(pt1) <- pt1$t1
        # pt %>% mutate(Freq = Freq/pt1[as.character(t1),"Freq"]) -> pt2
        # pt2$Freq <- pt2$Freq[,1]
        pt2 <- pt
        to.ret <-
          highcharter::hchart(pt2, "column", highcharter::hcaes(
            x = t1,
            y = Freq,
            group = t2
          )) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Relative histogram of", paste(vars[1], vars[2], sep = "-"))) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = "number"))}
      if (var1.is.cat & !var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var1), f2 = var2)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(features$f1) %>% dplyr::summarise(m = mean(features$f2, na.rm = T)) -> f
        # hc <- highchart() %>%
        #   hc_xAxis(categories = 1:5) %>%
        #   hc_add_series(name = "Mean", data = f$m[2:6])
        # to.ret <-
        #   hc %>% hc_chart(type = "column") %>% hc_add_theme(hc_theme_google())
        to.ret <- highcharter::hcboxplot(x = var2, var = var1) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Boxplot of", vars[2])) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = vars[2]))
      }
      if (!var1.is.cat & var2.is.cat) {
        features <-
          data.frame(f1 = as.factor(var2), f2 = var1)
        features$f2 <- as.numeric(features$f2)
        features %>% dplyr::group_by(features$f1) %>% dplyr::summarise(m = mean(features$f2, na.rm = T)) -> f
        # hc <- highchart() %>%
        #   hc_xAxis(categories = 1:5) %>%
        #   hc_add_series(name = "Mean", data = f$m[2:6])
        # to.ret <-
        #   hc %>% hc_chart(type = "column") %>% hc_add_theme(hc_theme_google())
        to.ret <- highcharter::hcboxplot(x = var1, var = var2) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Boxplot of", vars[1])) %>% highcharter::hc_xAxis(title = list(text = vars[2])) %>% highcharter::hc_yAxis(title = list(text = vars[1]))
      }
      if (!var1.is.cat & !var2.is.cat) {
        d <- data.frame(v1 = var1, v2 = var2)
        to.ret <-
          highcharter::hchart(d, "scatter", highcharter::hcaes(x = v1, y = v2)) %>% highcharter::hc_add_theme(highcharter::hc_theme_google()) %>%
          highcharter::hc_title(text = paste("Scatter plot of", paste(vars[1], vars[2], sep = "-"))) %>% highcharter::hc_xAxis(title = list(text = vars[1])) %>% highcharter::hc_yAxis(title = list(text = vars[2]))
      }
    }
  } else{
    to.ret <- highcharter::hcpie(as.factor(var1)) %>%
      highcharter::hc_title(text = paste("Pie chart of", vars[1]))
  }

  to.ret
}
