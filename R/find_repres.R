#' Find representative variables.
#'
#'
#' @description
#' Uses \code{\link[muvis]{min_forest}} community detection and selects a number of variables from each community as representative.
#'
#'
#' @param data A normalized dataframe or matrix with no missing data of continuous and (or) categorical measurements.
#' @param ratio A number between (0, 1) indicating the ratio of data variables to be chosen as representative. Defaults to 0.1.
#' @param weighted A logical indicating if the number of nodes selected from each community is weighted. Defaults to FALSE. See details for more information.
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable.
#' Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' If it is set, first will run \code{\link[muvis]{data_preproc}} to specify categorical and continuous variables.
#'
#'
#' @details
#' The function uses \code{\link[muvis]{min_forest}} to detect communities of variables. It sorts each community nodes based on their degree and selects an equal number of each based on \code{ratio} parameter.
#' If \code{weighted} is TRUE it will select different number of nodes from each community based on their sizes.
#'
#'
#'
#' @author Vahid Balazadeh, Elyas Heidari
#'
#' @examples
#' data("NHANES")
#' ## Using raw data
#' repres_vars <- find_repres(data = NHANES[1:500, ], ratio = .2, levels = 10)
#' repres_vars
#'
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' ## With \code{weighted = TRUE}
#' repres_vars <- find_repres(data = data[1:500, ], weighted = TRUE)
#' repres_vars
#'
#' @return a vector containing names of representative variables.
#'
#' @export
#'
#'
#' @importFrom igraph degree
#'

find_repres <- function (data,
                         ratio = 0.1,
                         weighted = FALSE,
                         levels = NULL) {
  mf <- min_forest(data, levels = levels, community = T)
  df <-
    data.frame(
      node = mf$network$x$nodes$id,
      community = mf$network$x$nodes$community,
      degree = igraph::degree(mf$graph)[match(mf$network$x$nodes$id, names(igraph::degree(mf$graph)))]
    )
  df <- df[order(df$community,-df$degree),]

  community_lengths <-
    sapply(c(1:length(unique(
      mf$communities
    ))), function(x)
      sum(mf$communities == x))
  if (!weighted)
    weights <-
    rep(1 / length(community_lengths), length(community_lengths))
  else
    weights <-
    sapply(community_lengths, function(x)
      x / sum(community_lengths))

  community_no <- round(weights * (ratio * ncol(data)))

  return(as.vector(unlist(sapply(c(
    1:length(community_lengths)
  ), function(x) {
    df <- df[df$community == x, ]
    if (community_no[x] > nrow(df))
      as.vector(df$node)
    else
      as.vector(df$node[1:community_no[x]])
  }))))

}
