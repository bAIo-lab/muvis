#' Find representative variables.
#'
#'
#' @description
#' Uses \code{\link[muvis]{graph_vis}} community detection on the input graph and selects a number of variables from each community as representative.
#'
#'
#' @param graph An arbitrary graph object of data in R.
#' @param ratio A number between (0, 1) indicating the ratio of variables to be chosen as representative. Defaults to 0.1.
#' @param weighted A logical indicating if the number of nodes selected from each community is weighted. Defaults to FALSE. See details for more information.
#'
#'
#' @details
#' The function uses \code{\link[muvis]{graph_vis}} to detect communities of variables in the \code{graph} input. It sorts each community nodes based on their degree and selects an equal number of each based on \code{ratio} parameter.
#' If \code{weighted} is TRUE it will select different number of nodes from each community based on their sizes.
#'
#'
#'
#' @author Vahid Balazadeh, Elyas Heidari
#'
#' @examples
#' data("NHANES")
#' ## Using \code{\link[muvis]{min_forest}} graph
#' mf <- min_forest(data = NHANES[1:1000, ], levels = 10)
#' repres_vars <- find_repres(graph = mf$graph, ratio = .2)
#'
#' ## Using \code{\link[muvis]{ggm}} graph with \code{weighted = TRUE}
#' gm <- ggm(data = NHANES[1:1000, ], levels = 10)
#' repres_vars <- find_repres(graph = gm$graph, weighted = TRUE)
#'
#' @return a vector containing names of representative variables.
#'
#' @export
#'
#'
#' @importFrom igraph degree
#'

find_repres <- function (graph,
                         ratio = 0.1,
                         weighted = FALSE) {
  gv <- graph_vis(graph, community = T)
  df <-
    data.frame(
      node = gv$network$x$nodes$id,
      community = gv$network$x$nodes$community,
      degree = igraph::degree(gv$graph)[match(gv$network$x$nodes$id, names(igraph::degree(gv$graph)))]
    )
  df <- df[order(df$community,-df$degree),]

  community_lengths <-
    sapply(c(1:length(unique(
      gv$communities
    ))), function(x)
      sum(gv$communities == x))
  if (!weighted)
    weights <-
    rep(1 / length(community_lengths), length(community_lengths))
  else
    weights <-
    sapply(community_lengths, function(x)
      x / sum(community_lengths))

  community_no <- round(weights * (ratio * length(gv$network$x$nodes)))

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
