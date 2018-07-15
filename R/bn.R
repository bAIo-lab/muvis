#' construct and visualize a Bayesian (directed) Network
#'
#'
#' @description
#' Fits a Bayesian network (a directed acyclic graph) to variables and performs bootstrapping.
#' The function also visualizes the structure of the Bayesian network and decompose the graph into the underlying communities.
#'
#'
#' @param data a normalized dataframe or matrix with no missing data of continuous and (or) categorical measurements.
#' @param C.alg a vector of Constraint-based and Local Discovery algorithms from \{"gs", "mmpc", "si.hiton.pc"\}. Default is NULL.
#' @param S.alg a vector of Score-based and Hybrid algorithms from \{"pc.stable", "hc", "tabu", "mmhc", "aracne"\}. The default is "mmhc".
#' @param blacklist a two-column dataframe which includes edges to be excluded from the network.
#' @param whitelist a two-column dataframe which includes edges to be included from the network.
#' @param R an integer to demonstrate the number of runs of bootstrap sampling. The default value is 10.
#' @param m an integer to demonstrate the number of rows to be sampled from dataset in each bootstrap sampling. The default value is the number of rows in data.
#' @param str.thresh a double between 0 and 1 indicating the threshold value for an association to be considered in the network. The default value is 0.
#' @param dir.thresh a double between 0 and 1 indicating the threshold value for a direction of an edge to be considered in the network. The default value is 0.
#' @param community a logical value. If TRUE (the default) the network will be colored into communities of edge-dense subgraphs.
#' @param plot (default = TRUE)
#'
#'
#'
#' @details
#' The function is a wrapper for bnlearn package implementing several algorithms including Constraint-based algorithms (i.e., Max-Min Parents and Children, Semi-Interleaved HITON-PC, and Grow-Shrink), Score-based algorithms (i.e., Hill-Climbing and Tabu Search), and Hybrid algorithms (i.e., Max-Min Hill-Climbing), and Local Discovery algorithms (i.e, Max-Min Parents and Children and ARACNE). If one uses a more than one algorithm, the function combines all of the algorithms and returns a graph based on the combination. The graph is constructed based on the strength of associations calculated by bootstrapping.
#'
#'
#' @references  Nagarajan R, Scutari M, Lebre S (2013). "Bayesian Networks in R with Applications in Systems Biology". Springer.
#' @references  Scutari M (2010). "Learning Bayesian Networks with the bnlearn R Package". Journal of Statistical Software, 35(3), 1-22. URL http://www.jstatsoft.org/v35/i03/.
#' @references  Scutari M (20107). "Bayesian Network Constraint-Based Structure Learning Algorithms: Parallel and Optimized Implementations in the bnlearn R Package". Journal of Statistical Software, 77(2), 1-20. URL http://www.jstatsoft.org/v77/i02/.
#' @references  Koller D, Friedman N (2009). Probabilistic Graphical Models: Principles and Techniques. MIT Press.
#' @references  Korb K, Nicholson AE (2010). Bayesian Artificial Intelligence. Chapman & Hall/CRC, 2nd edition.
#' @references  Pearl J (1988). Probabilistic Reasoning in Intelligent Systems: Networks of Plausible Inference. Morgan Kaufmann.
#'
#'
#' @author  Elyas Heidari
#'
#' @return  returns a list including a dataframe including the edges of the network with their strength and strength of directions and a highcharter plot.
#'
#'
#'
#' @export
#'
#' @importFrom purrr map
#' @importFrom utils View
#' @importFrom dplyr mutate
#' @importFrom igraph graph.data.frame cluster_louvain membership as.undirected
#' @importFrom visNetwork toVisNetworkData visNetwork visOptions visEdges
#' @importFrom bnlearn boot.strength
#' @importFrom magrittr %>%

bn <-
  function(data,
           C.alg = NULL,
           S.alg = c("mmhc"),
           blacklist = NULL,
           whitelist = NULL,
           R = 10,
           m = 0,
           str.thresh = 0,
           dir.thresh = 0,
           community = T,
           plot = T) {
    data <- data.matrix(data)
    data <- data.frame(data)
    if (m == 0) {
      m <- dim(data)[1]
    }

    S.alg %>% purrr::map(
      function(alg)
        bnlearn::boot.strength(
          data,
          R = R,
          m = m,
          algorithm =  alg,
          algorithm.args = list(blacklist = blacklist,
                                whitelist = whitelist)
        )
    ) -> S.boot
    S.boot %>% purrr::map(function(x)
      x$strength) -> S.strength
    S.boot %>% purrr::map(function(x)
      x$direction) -> S.direction
    strength <- do.call(rbind, S.strength)
    direction <- do.call(rbind, S.direction)
    if (length(C.alg) != 0) {
      C.alg %>% purrr::map(
        function(alg)
          bnlearn::boot.strength(
            data,
            R = R,
            m = m,
            algorithm =  alg,
            algorithm.args = list(blacklist = blacklist,
                                  whitelist = whitelist)
          )
      ) -> C.boot
      C.boot %>% purrr::map(function(x)
        x$strength) -> C.strength
      C.boot %>% purrr::map(function(x)
        x$direction) -> C.direction
      C.strength <- do.call(rbind, C.strength)
      C.direction <- do.call(rbind, C.direction)
      strength <- rbind(C.strength, strength)
      direction <- rbind(C.direction , direction)
    }
    direction <- colMeans(direction, na.rm = TRUE)
    strength <- colMeans(strength, na.rm = TRUE)

    bn.s <- S.boot[[1]]
    bn.s[which(
      !is.na(strength) &
        strength >= str.thresh &
        !is.na(direction) & direction >= dir.thresh
    ), ] -> bn.s
    utils::View(bn.s)
    bn.s1 <- bn.s
    bn.s2 <- bn.s
    rownames(bn.s1) <- paste0(bn.s1$from, bn.s1$to)
    bn.s2$from <- bn.s$to
    bn.s2$to <- bn.s$from
    rownames(bn.s2) <- paste0(bn.s2$from, bn.s2$to)
    rownames(bn.s) %>% purrr::map(function(x)
      (bn.s1[x, 4] > bn.s2[x, 4])) -> direction
    direction <- unlist(direction)

    g <- bn.s[direction, ]
    g <- igraph::graph.data.frame(bn.s)

    graph.vis(
      g,
      directed = T,
      plot = plot,
      community = community,
      betweenness = T
    )
    # data <- visNetwork::toVisNetworkData(g)
    # if (community) {
    #   fc <- igraph::cluster_louvain(igraph::as.undirected(g))
    #   groups <- igraph::membership(fc)
    #   data$nodes %>% dplyr::mutate(group = groups[data$nodes$id]) -> data$nodes
    # }
    # vs <- visNetwork::visNetwork(nodes = data$nodes, edges = data$edges)  %>%
    #   visNetwork::visOptions(highlightNearest = list(
    #     enabled = T,
    #     degree = 1,
    #     hover = T
    #   )) %>% visNetwork::visEdges(arrows = "to")
    # list(graph = bn.s, igraph=val$graph, betweenness = val$betweenness, network = val$network)
  }
