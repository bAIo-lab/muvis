#' construct and visualize a minimal forest based on Chow-Liu algorithm
#'
#'
#' @description
#' Fits a minimal forest to data and visualizes it.
#'
#'
#' @param data A normalized dataframe or matrix with no missing data of continuous and (or) categorical measurements.
#' @param stat Measure to be minimized: LR, AIC, or BIC (the default). The default is BIC. It can also be a user-defined function with the format: FUN (model, dataset, previous, forbEdges); where the parameters are defined as in chStat. The function must return a structure as in chStat.
#' @param community A logical value to show if the node communities should be detected and colored in the returned graph. (default = TRUE)
#' @param betweenness A logical value to show if the node betweenness measurements should be computed and returned from the function. (default = TRUE)
#' @param plot A logical value to show if the graph should be plotted. (default = FALSE)
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable.
#' Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' If it is set, first will run \code{\link[muvis]{data_preproc}} to specify categorical and continuous variables.
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
#' @examples
#' data("NHANES")
#' ## Using raw data
#' mf <- min_forest(data = NHANES[1:200, ], stat = "BIC", plot = TRUE, levels = 5)
#'
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' mf <- min_forest(data = data[1:200, ], stat = "BIC", plot = FALSE)
#'
#' @return a list containing:
#' \item{significanse}{A data.frame containing edges with p-statistics and p.values.}
#' \item{summary}{a gRapHD object which is the fit model.}
#' \item{graph}{an igraph object.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{network}{a visNetwork plot of the graph.}
#' \item{communities}{a named vector indicating the community of each node.}
#'
#' @export
#'
#'
#' @importFrom gRapHD minForest neighbourhood stepw
#' @importFrom igraph make_undirected_graph betweenness fastgreedy.community membership
#' @importFrom visNetwork toVisNetworkData visNetwork visOptions visEdges
#' @importFrom igraph V
#' @importFrom dplyr %>%
#'
#'

min_forest <-
  function(data,
           stat = "BIC",
           community = TRUE,
           betweenness = TRUE,
           plot = FALSE,
           levels = NULL) {

    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)
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

    statistics = mbG@statSeq
    g <- igraph::make_undirected_graph(e)
    val <-
      graph_vis(
        g,
        community = community,
        betweenness = T,
        plot = plot,
        directed = F
      )

    vnet <- val$network
    edges <-  vnet$x$edges

    test_matrix <- test_assoc(data, vnet$x$nodes$id, levels = levels)
    e_names <- rbind(edges$from, edges$to)
    p_values <- apply(e_names, 2, function(x) test_matrix[x[1], x[2]])
    title = paste0("<p>", paste("P.value =", p_values), "</p>")
    edges[, "statistics"] <- statistics
    edges[, "title"] <- title

    significance <- data.frame(edges$from, edges$to)
    significance$statistics <- statistics
    significance$p.value <- p_values

    vn <- visNetwork::visNetwork(vnet$x$nodes, edges, height = "500px", width = "100%")  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      ))
    if (community) {
      list(
        significance = significance,
        summary = mbG,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn,
        communities = val$communities
      )
    } else {
      list(
        significance = significance,
        summary = mbG,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn
      )
    }
  }
