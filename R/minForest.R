#' construct and visualize a minimum forest based on Chow-Liu algorithm
#'
#'
#' @description
#' Fits a minimum forest to data.
#' Also visualizes the the minimum forest.
#'
#'
#' @param  data a normalized dataframe or matrix with no missing data of continuous and (or) categorical measurements.
#' @param  stat measure to be minimized: LR, AIC, or BIC (the default). Default is BIC. It can also be a user-defined function with the format: FUN(model, dataset, previous, forbEdges); where the parameters are defined as in chStat. The function must return a structure as in chStat.
#' @param  community a logical value. If TRUE (the default) the network will be colored into communities of edge-dense subgraphs.
#' @param  interactive (default = TRUE)
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
#' \item{}{a gRapHD object which is the fit model.}
#' \item{}{betweenness measurements for each node.}
#' \item{}{the highcharter plot of the network.}
#'
#' @usage min.forest(data, stat = "BIC", interactive = TRUE, community = TRUE)
#' @export min.forest
#'
#'
#' @importFrom gRapHD minForest neighbourhood stepw
#' @importFrom igraph make_undirected_graph betweenness fastgreedy.community membership
#' @importFrom visNetwork toVisNetworkData visNetwork visOptions visEdges
#' @importFrom igraph V
#'
min.forest <-
  function(data,
           stat = "BIC",
           community = TRUE,
           interactive = TRUE) {
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
    edges[, "title"] <- title
    g <- igraph::make_undirected_graph(e)
    val <-
      graph.vis(
        g,
        community = community,
        betweenness = T,
        interactive = interactive,
        directed = F
      )
    # bc <- igraph::betweenness(
    #   g,
    #   v = igraph::V(g),
    #   directed = F,
    #   weights = NULL,
    #   nobigint = TRUE,
    #   normalized = FALSE
    # )
    # if (community) {
    #   fc <- igraph::fastgreedy.community(g)
    #   groups <- igraph::membership(fc)
    #   groups <- groups[as.character(nodes$id)]
    #   groups -> nodes$group
    # }
    # vn <- visNetwork::visNetwork(nodes, edges, height = "500px", width = "100%")  %>%
    #   visNetwork::visOptions(highlightNearest = list(
    #     enabled = T,
    #     degree = 1,
    #     hover = T
    #   ))
    list(
      summary = mbG,
      graph = val$graph,
      betweenness = val$betweenness,
      network = val$network
    )
  }
