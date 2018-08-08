#' construct and visualize causal (directed) networks
#'
#' @description
#' A wrapper of skeleton function in pcalg package. Estimates the skeleton of a Directed Acyclic Graph (DAG) using PC, FCI, or RFCI algorithms.
#' 
#' @param  data A normalized dataframe or matrix with no missing data.
#' @param  dtype Either "guassian" for continuous/ordinal discrete data or "discrete" for discrete/nominal data. (defult = "gaussian") See datails for more description.
#' @param  alpha A threshold for partial correlation thresholding dtype (default = 0.05). To be used only when the dtype "threshold" is used.
#' @param  community a logical value to show if the node communities should be detected and colored in the returned graph.(default = TRUE)
#' @param  betweenness a logical value to show if the node betweenness measurements should be computed and returned from the function.(default = TRUE)
#' @param  plot a logical value to show if the graph should be plotted.(default = TRUE)
#' @param  ... remaning parameters of skleton funciton.
#'
#' @details There is no specific distribution needed for the data. The parameter dtype will be used for determining the data type to be provided as the input of the function. However, it is highly recommended to use "guassian" data type for both continuous and ordinal discrete data.
#'
#' @references  D. Colombo and M.H. Maathuis (2014).Order-independent constraint-based causal structure learn- ing. Journal of Machine Learning Research 15 3741-3782.
#' @references  D. Colombo, M. H. Maathuis, M. Kalisch, T. S. Richardson (2012). Learning high-dimensional directed acyclic graphs with latent and selection variables. Ann. Statist. 40, 294-321.
#' @references  M. Kalisch and P. Buehlmann (2007). Estimating high-dimensional directed acyclic graphs with the PC-algorithm, JMLR 8 613-636.
#' @references  P. Spirtes, C. Glymour and R. Scheines (2000). Causation, Prediction, and Search, 2nd edition, MIT Press.
#'
#' @author Elyas Heidari
#'
#'
#' @return A list in which each element is the details of a specific fitted dtype.
#' \item{graph}{an igraph object of the graphical model.}
#' \item{betweenness}{betweenness measurements of each edge.}
#' \item{communities}{a named vector indicating the community of each node.}
#' \item{network}{a visNetwork plot of the graphical model.}
#'
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  gRbase cov2pcor stepwise
#' @importFrom  purrr map
#' @importFrom  gRim cmod
#' @importFrom  graph graphNEL
#' @importFrom  SIN sinUG getgraph
#' @importFrom  glasso glasso
#' @importFrom  igraph cluster_louvain betweenness membership V intersection
#' @importFrom  stats C cov.wt
#' @importFrom  dtypes as
#' @importFrom  graph nodes
#' @importFrom  pcalg skeleton



dgm <-
  function(data,
           alpha = 1e-5,
           dtype = "gaussian",
           community = TRUE,
           betweenness = TRUE,
           plot = TRUE,
           ...) {
    if (dtype == "gaussian") {
      S.data <- cov.wt(data, dtype = "ML")$cov
      C.data <- cov2cor(S.data)
      suffStat <- list(C = C.data, n = nrow(data))
      skeleton.data <-
        pcalg::skeleton(suffStat,
                        gaussCItest,
                        p = ncol(data),
                        alpha = alpha,
                        ...)
      nodes(skeleton.data@graph) <- names(data)
      to.ret <- graph.vis(skeleton.data@graph, directed = T)
    }
    if (dtype == "discrete") {
      suffStat <- list(dm = data, adaptDF = T)
      skeleton.data <-
        pcalg::skeleton(suffStat,
                        disCItest,
                        p = ncol(data),
                        alpha = alpha,
                        ...)
      nodes(skeleton.data@graph) <- names(data)
      to.ret <-
        graph.vis(
          skeleton.data@graph,
          directed = T,
          community = community,
          betweenness = betweenness,
          plot = plot
        )
    }
    to.ret
  }


