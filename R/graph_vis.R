#' graph visualization and community detection
#'
#'
#' @description
#' Converts the graph to igraph, finds communities and plots.
#'
#' @param graph an arbitrary graph object in R.
#' @param directed TRUE if the graph is directed (default = FALSE)
#' @param community (default = TRUE)
#' @param betweenness (default = TRUE)
#' @param plot (default = FALSE)
#'
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @return (If plot = TRUE it plots the noninteractive graph) A list contains:
#' \item{graph}{an igraph object}
#' \item{betweenness}{betweenness measurements of each edge. (if betweenness = TRUE)}
#' \item{network}{a highcharter plot of the graph.}
#' \item{communities}{a named vector of community number for each variable}
#'
#' @export graph.vis
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  igraph cluster_louvain betweenness membership V layout_with_fr induced.subgraph as_adjacency_matrix
#' @importFrom  methods as
#' @importFrom  grDevices rainbow
#' @importFrom  magrittr %>%
#' @importFrom  qgraph qgraph

graph.vis <-
  function(graph,
           directed = F,
           community = T,
           betweenness = T,
           plot = F) {
    plot_community <- function(graph, community_num) {
      t <- igraph::V(graph)$community == community_num
      v <-  igraph::V(graph)[t]
      sub_graph <- igraph::induced.subgraph(graph, v)
      qgraph::qgraph(
        igraph::as_adjacency_matrix(sub_graph),
        groups=as.factor(igraph::V(sub_graph)$community),
        layout="spring",
        color = igraph::V(sub_graph)$color,
        label.norm = "OOOO",
        labels = names(igraph::V(sub_graph)))

    }
    ig <- methods::as(graph, "igraph")

    if (betweenness) {
      bt <-
        sort(igraph::betweenness(ig, igraph::V(ig), directed = directed),
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

    data <- visNetwork::toVisNetworkData(ig)
    if (community)
      data$nodes$group <- igraph::V(ig)$community
    vs <-
      visNetwork::visNetwork(nodes = data$nodes, edges = data$edges)  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ))
    if (directed)
      vs <- vs %>% visNetwork::visEdges(arrows = "to")

    if (plot) {
      gg <- qgraph::qgraph(igraph::as_adjacency_matrix(ig),
                     groups=as.factor(igraph::V(ig)$community),
                     layout="spring", palette = "ggplot2",
                     vsize =  8*exp(-length(igraph::V(ig))/80)+ 0.5)
      igraph::V(ig)$color <- gg$graphAttributes$Nodes$color
    }
    if (community) {
      if (plot) {
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
