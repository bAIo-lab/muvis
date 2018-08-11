#' graph visualization and community detection
#'
#'
#' @description
#' Converts the graph to an igraph object, finds communities and plots it using qgraph package.
#'
#' @param graph An arbitrary graph object in R.
#' @param directed Set it TRUE when the graph is directed. (default = FALSE)
#' @param community A logical value to show if the node communities should be detected and colored in the returned graph. (default = TRUE)
#' @param betweenness A logical value to show if the node betweenness measurements should be computed and returned from the function. (default = TRUE)
#' @param plot A logical value to show if the graph should be plotted. (default = FALSE)
#' @param ... Any additional arguments described below.
#'
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @return If plot = TRUE it plots the non-interactive graph (If plot.community = TRUE plots communities too) also returns a list contains:
#' \item{graph}{an igraph object.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{network}{a visNetwork plot of the graph.}
#' \item{communities}{a named vector indicating the community of each node.}
#'
#' @export
#'
#' @examples
#'
#' ## Visualize Harman23.cor covariance matrix
#' require(datasets)
#' data("Harman23.cor")
#' \donttest{
#' graph_vis(Harman23.cor$cov, plot = TRUE, plot.community = TRUE)
#' }
#'
#' \dontshow{
#' graph_vis(Harman23.cor$cov, plot = TRUE, plot.community = TRUE)
#' }
#'
#' @section Additional arguments:
#' \describe{
#' \item{groups}{A list that indicates which community each node is. The automatic community detection will be ignored when it is set.}
#' \item{plot.community}{Logical indicating if communities should be plotted. Defaults to FALSE.}
#' \item{filename}{Name of the plot file without extension. (qgraph function argument)}
#' \item{filetype}{A character indicates the file type to save the plots in. (qgraph function argument)}
#' }
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  igraph cluster_louvain betweenness membership V layout_with_fr induced.subgraph as_adjacency_matrix degree as.undirected
#' @importFrom  methods as
#' @importFrom  dplyr %>%
#' @importFrom  qgraph qgraph

graph_vis <-
  function(graph,
           directed = F,
           community = T,
           betweenness = T,
           plot = F,
           ...) {
    arguments = list(...)

    usr_groups <- arguments$groups

    plot.community <- arguments$plot.community
    if (is.null(plot.community))
      plot.community = F

    plot_community <- function(graph, community_num) {
      t <- igraph::V(graph)$community == community_num
      v <-  igraph::V(graph)[t]
      sub_graph <- igraph::induced.subgraph(graph, v)
      qgraph::qgraph(
        igraph::as_adjacency_matrix(sub_graph),
        groups = as.factor(igraph::V(sub_graph)$community),
        layout = "spring",
        color = igraph::V(sub_graph)$color,
        label.norm = "OOOOOOO",
        labels = names(igraph::V(sub_graph)),
        vsize = max(1, 0.5 + 320 / (length(igraph::V(sub_graph)) + 50)),
        filename = paste(arguments$filename, community_num, sep = ""),
        filetype = arguments$filetype
      )

    }

    ig <- methods::as(graph, "igraph")

    if (betweenness) {
      not_sort_bt <-
        igraph::betweenness(ig, igraph::V(ig), directed = directed)
      bt <-
        sort(not_sort_bt,
             decreasing = T)
    } else {
      bt <- NULL
    }

    community_n <- 1
    if (community) {
      fc <- igraph::cluster_louvain(igraph::as.undirected(ig))
      igraph::V(ig)$community <- igraph::membership(fc)
      community_n <- length(unique(igraph::V(ig)$community))

    }

    if (!is.null(usr_groups)) {
      igraph::V(ig)$community <- usr_groups
      community_n <- length(unique(usr_groups))
    }

    data <- visNetwork::toVisNetworkData(ig)
    if (betweenness)
      nodes_title <-
      paste0("<p> Degree = ",
             igraph::degree(ig),
             "</br> Betweenness = ",
             not_sort_bt,
             "</p>")
    else
      nodes_title <-
      paste0("<p> Degree = ", igraph::degree(ig), "</p>")

    node_value <- igraph::degree(ig)
    if (community)
      data$nodes$group <- igraph::V(ig)$community

    nodes = data$nodes
    nodes[, "title"] <- nodes_title
    nodes[, "value"] <- node_value
    vs <-
      visNetwork::visNetwork(nodes = nodes, edges = data$edges)  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ))
    if (directed)
      vs <- vs %>% visNetwork::visEdges(arrows = "to")

    if (plot) {
      gg <- qgraph::qgraph(
        igraph::as_adjacency_matrix(ig),
        groups = as.factor(igraph::V(ig)$community),
        layout = "spring",
        palette = "ggplot2",
        vsize = max(1, 0.5 + 320 / (length(igraph::V(
          ig
        )) + 50)),
        filetype = arguments$filetype,
        filename = arguments$filename
      )
      igraph::V(ig)$color <- gg$graphAttributes$Nodes$color
    }

    if (community) {
      if (plot && plot.community) {
        for (i in 1:community_n)
          plot_community(ig, i)
      }
      com <- igraph::V(ig)$community
      names(com) <- names(igraph::V(ig))
      list(
        graph = ig,
        betweenness = bt,
        network = vs,
        communities = com
      )
    } else {
      list(graph = ig,
           betweenness = bt,
           network = vs)
    }

  }
