#' Test for association between each paired variables and construct heatmap using heatmaply package.
#'
#' @description
#' Tests for association between each paired variables:
#' \describe{
#' \item{Categorical-Categorical}{Using pearson's chi-squared test (chisq.test function from stats package).}
#' \item{Continuous-Continuous}{Using correlation test (cor.test function from stats package).}
#' \item{Categorical-Continuous}{Using analysis of variance model (aov function from stats package).}
#' }
#' Also adjusts p.values using Benjamini & Hochberg method (p.adjust function from stats package) and constructs heatmap using heatmaply function.
#'
#' @param data a dataframe. It is strongly recommended that the dataframe has no missing data and is preprocessed.
#' @param vars a list including the name (or index) of columns of data.
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' @param plot Logical indicating if the heatmap should be constructed. Defaults to FALSE.
#'
#' @details This provides a wrapper to \code{chisq.test}, \code{cor.test}, \code{aov}, \code{p.adjust} from \code{stats} package to test association between variables
#' And a wrapper to \code{heatmaply} package to construct heatmap.
#'
#'
#' @author Elyas Heidari
#'
#' @return If plot = FALSE, returns a matrix containing p.values of tests between each two variables. Otherwise returns A list which contains:
#' \item{matrix}{A matrix containing p.values of tests between each two variables.}
#' \item{heatmap}{A plotly object containing heatmap related to matrix.}
#'
#' @examples
#' data("Nhanes")
#' ## Using raw data
#' df <- Nhanes[sample(nrow(Nhanes), 1000), ]
#' test_matrix <- test_assoc(data = df, vars = colnames(df), plot = FALSE, levels = 15)
#'
#' ## Using preprocessed data
#' data <- data_preproc(Nhanes, levels = 15)
#' data$SEQN <- NULL
#' ## Outputs the heatmap too (plot = TRUE)
#' test_mat_heatmap <- test_assoc(data = data, vars = colnames(data[, 1:20]), plot = TRUE)
#'
#' @export
#'
#' @importFrom stats p.adjust
#' @importFrom heatmaply heatmaply
#' @importFrom plotly layout


test_assoc <- function(data,
                       vars,
                       levels = NULL,
                       plot = FALSE) {

  to.ret <- matrix(NA, ncol = length(vars), nrow = length(vars))
  for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
      if (i == j)
        next
      to.ret[i, j] <- test_pair(data, vars[i], vars[j], levels = levels)
    }
  }

  to.ret <- stats::p.adjust(to.ret, method = "BH")
  to.ret <- matrix(to.ret, ncol = length(vars), nrow = length(vars))

  rownames(to.ret) <- colnames(to.ret) <- vars
  if (plot) {
    heat.map <-
      heatmaply::heatmaply(to.ret) %>% plotly::layout(margin = list(l = 130, b = 40))
    return(list(matrix = to.ret, heatmap = heat.map))
  } else {
    return(matrix = to.ret)
  }
}
