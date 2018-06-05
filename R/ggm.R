#' construct and visualize a gussian graphical model indicating the associations between continuous variables in data
#' @description
#' A function to fit a gussian graphical model to continuous variables using six methods, i.e, stepwise AIC, stepwise BIC, stepwise significance test, partial correlation tresholding, edgewise significance test, and glasso.
#' The function also visualizes the graphical model and decompose the graph into the underlying communities.
#'
#' @param data A normalized dataframe or matrix with no missing data of continuous measurments.
#' @param treshold A treshhold for partial correlation tresholding method (default = 0.05).
#' @param significance A cutoff for edge significance (default = 0.05).
#' @param rho (Non-negative) regularization parameter for glasso (default = 0.1).
#'
#' @details The function combines six methods to construct the model, that is, the edge set is the intersection of all edge sets each of which is found by a method. The package gRim is used to implement AIC, BIC, and stepwise significance test. The method glasso from the package glasso is used to provide a sparse inverse covariance matrix.
#'
#' @references  Højsgaard, S., Edwards, D., & Lauritzen, S. (2012). Graphical Models with R. Springer US. \url{https://doi.org/10.1007/978-1-4614-2299-0}
#' @references  Friedman, J., Hastie, T., & Tibshirani, R. (2007). Sparse inverse covariance estimation with the graphical lasso. Biostatistics, 9(3), 432–441. \url{https://doi.org/10.1093/biostatistics/kxm045}
#' @references  Abreu, G. C. G., Edwards, D., & Labouriau, R. (2010). High-Dimensional Graphical Model Search with thegRapHDRPackage. Journal of Statistical Software, 37(1). \url{https://doi.org/10.18637/jss.v037.i01}
#'
#'
#' @author Elyas Heidari
#'
#'
#' @return A list in which each element is the details of a specific fitted method.
#' \item{aic}{Fit details of aic model.}
#' \item{bic}{Fit details of bic model.}
#' \item{test}{Fit details of significance test model.}
#' \item{pcor}{Partial correlaiton matrix used for tresholding method.}
#' \item{significance}{Significance matrix consisting p.values of each edge.}
#' \item{glasso}{Partial correlation matrix calculated by glasso.}
#' \item{betweenness}{Betweenness measurement for each node.}
#'
#' @export
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom gRbase cov2pcor stepwise
#' @importFrom  gRim cmod
#' @importFrom graph graphNEL
#' @importFrom  SIN sinUG getgraph
#' @importFrom  glasso glasso
#' @importFrom  igraph cluster_louvain betweenness
#' @importFrom methods as
#' @importFrom stats C cov.wt
#'



ggm <-
  function(data,
           treshold = 0.05,
           significance = 0.05,
           rho = 0.1) {
    model <- gRim::cmod( ~ . ^ ., data = data)
    aic <- gRbase::stepwise(model)
    bic <- gRbase::stepwise(model, k = log(nrow(data)))
    test <- gRbase::stepwise(model, criterion = "test")
    S <- stats::cov.wt (data, method = "ML")$cov
    PC <- gRbase::cov2pcor(S)
    Z <- abs(PC)
    Z[Z < threshold] <- 0
    diag(Z) <- 0
    Z[Z > 0] <- 1
    g.thresh <- as(Z, "graph::graphNEL")
    thresh <- gRim::cmod(g.thresh, data = data)
    psin <- SIN::sinUG(S, n = nrow(data))
    gsin <- as(SIN::getgraph(psin, significance), "graph::graphNEL")
    res.lasso <- glasso::glasso(stats::C, rho = rho)
    AM <- abs(res.lasso$wi) > treshold
    diag(AM) <- F
    g.lasso <- as(AM, "graph::graphNEL")
    nodes(g.lasso) <- colnames(data)
    othermodels <-
      list(
        as(test, "igraph"),
        as(thresh, "igraph"),
        as(gsin, "igraph"),
        as(g.lasso, "igraph"),
        as(aic, "igraph"),
        as(bic, "igraph")
      )
    commonedges <- do.call(igraph::intersection, othermodels)
    bt <-
      igraph::betweenness(as(commonedges, "igraph"), V(as(commonedges, "igraph")))
    fc <- igraph::cluster_louvain(as(commonedges, "igraph"))
    data <- visNetwork::toVisNetworkData(as(commonedges, "igraph"))
    data$nodes$group <- igraph::membership(fc)
    visNetwork::visNetwork(nodes = data$nodes, edges = data$edges)  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ))
    list(
      aic = aic$fitinfo,
      bic = bic$fitinfo,
      test = test$fitinfo,
      pcor = PC,
      glasso = g.lasso,
      betweenness = bt
    )
  }
