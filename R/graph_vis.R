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
#' @param plot (default = TRUE)
#'
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @return (If plot = TRUE it plots the noninteractive graph) A list contains:
#' \item{graph}{an igraph object}
#' \item{betweenness}{betweenness measurements of each edge. (if betweenness = TRUE)}
#' \item{network}{a highcharter plot of the graph.}
#'
#' @export graph.vis
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  igraph cluster_louvain betweenness membership V layout_with_fr
#' @importFrom  methods as

graph.vis <-
  function(graph,
           directed = F,
           community = T,
           betweenness = T,
           plot = T) {
    ig <- methods::as(graph, "igraph")

    colrs <- c('#e6194b'
               ,'#3cb44b'
               ,'#ffe119'
               ,'#0082c8'
               ,'#f58231'
               ,'#911eb4'
               ,'#46f0f0'
               ,'#f032e6'
               ,'#d2f53c'
               ,'#fabebe'
               ,'#008080'
               ,'#e6beff'
               ,'#aa6e28'
               ,'#fffac8'
               ,'#800000'
               ,'#aaffc3'
               ,'#808000'
               ,'#ffd8b1'
               ,'#000080'
               ,'#808080')
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
      mark_list <-
        lapply(c(1:community_n), function(x)
          igraph::V(ig)[igraph::V(ig)$community == x])
    } else {
      mark_list <- NULL
    }
    colrs <- sample(colrs, community_n + 1)
    igraph::V(ig)$color <- colrs[igraph::V(ig)$community]
    igraph::V(ig)$label_color <- colrs[community_n + 1]
    ig$layout <- igraph::layout_nicely(ig)
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

    if (plot)
      igraph::plot.igraph(
        ig,
        vertex.label = "",
        layout = ig$layout,
        vertex.size = 1 + 600 / (100 + length(igraph::V(ig))),
        vertex.color = igraph::V(ig)$color
      )
    list(graph = ig,
         betweenness = bt,
         network = vs)

  }
