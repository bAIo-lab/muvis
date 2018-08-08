#' construct and visualize a minimum forest based on Chow-Liu algorithm
#'
#'
#' @description
#' Fits a minimum forest to data and visualizes it.
#'
#'
#' @param data A normalized dataframe or matrix with no missing data of continuous and (or) categorical measurements.
#' @param stat Measure to be minimized: LR, AIC, or BIC (the default). The default is BIC. It can also be a user-defined function with the format: FUN (model, dataset, previous, forbEdges); where the parameters are defined as in chStat. The function must return a structure as in chStat.
#' @param community A logical value to show if the node communities should be detected and colored in the returned graph. (default = TRUE)
#' @param betweenness A logical value to show if the node betweenness measurements should be computed and returned from the function. (default = TRUE)
#' @param plot A logical value to show if the graph should be plotted. (default = FALSE)
#'
#'
#' @details
#' The function is a wrapper for bnlearn package implementing several algorithms including Constraint-based algorithms (i.e., Max-Min Parents and Children, Semi-Interleaved HITON-PC, and Grow-Shrink), Score-based algorithms (i.e., Hill-Climbing and Tabu Search), and Hybrid algorithms (i.e., Max-Min Hill-Climbing), and Local Discovery algorithms (i.e, Max-Min Parents and Children and ARACNE). If one uses a more than one algorithm, the function combines all of the algorithms and returns a graph based on the combination. The graph is constructed based on the strength of associations calculated by bootstrapping.
#'
#'
#' @references  Chow, C.K., and Liu, C.N. (1968) Approximating discrete probability distributions with dependence trees. IEEE Transactions on Information Theory, Vol. IT-14, 3:462-7.
#' @references  Edwards, D., de Abreu, G.C.G. and Labouriau, R. (2010). Selecting high- dimensional mixed graphical models using minimal AIC or BIC forests. BMC Bioinformatics, 11:18.
#'
#'
#' @author  Elyas Heidari
#'
#'
#' @return a list containing:
#' \item{summary}{a gRapHD object which is the fit model.}
#' \item{graph}{an igraph object.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{network}{a visNetwork plot of the graph.}
#' \item{communities}{a named vector indicating the community of each node.}
#'
#' @export mini.forest
#'
#'
#' @importFrom gRapHD minForest neighbourhood stepw
#' @importFrom igraph make_undirected_graph betweenness fastgreedy.community membership
#' @importFrom visNetwork toVisNetworkData visNetwork visOptions visEdges
#' @importFrom igraph V
#' @importFrom magrittr %>%
#'
#'

mini.forest <-
  function(data,
           stat = "BIC",
           community = TRUE,
           betweenness = TRUE,
           plot = FALSE) {

    my.forest <- gRapHD::minForest(data, homog = F, stat = stat)
    nby <-
      gRapHD::neighbourhood(my.forest, orig = 1, rad = 2000)$v[, 1]
    bc.marg <- data[, nby]
    mbF <- gRapHD::minForest(bc.marg)
    mbG <- gRapHD::stepw(model = mbF,
                         data = bc.marg,
                         stat = stat)
    rpl <- function(x) {
      colnames(data[, nby])[x]
    }
    l <- apply(mbG@edges, 2, rpl)
    colnames(l) <- c("from", "to")
    l <- data.frame(l)
    nodes <- unique(c(as.character(l$from), as.character(l$to)))
    nodes <- data.frame(id = nodes, label = nodes)
    edges <- l
    e <- c()
    for (i in 1:dim(edges)[1]) {
      e <- c(e, edges[i, ])
    }
    e <- unlist(e)
    e <- as.character(e)
    title = paste0("<p>", paste("statistic =", mbG@statSeq), "</p>")
    g <- igraph::make_undirected_graph(e)
    val <-
      graph.vis(
        g,
        community = community,
        betweenness = T,
        plot = plot,
        directed = F
      )

    vnet <- val$network
    edges <-  vnet$x$edges
    edges[, "title"] <- title
    vn <- visNetwork::visNetwork(vnet$x$nodes, edges, height = "500px", width = "100%")  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ))
    if (community) {
      list(
        summary = mbG,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn,
        communities = val$communities
      )
    } else {
      list(
        summary = mbG,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn
      )
    }
  }
