#' graph visualization and community detection
#'
#'
#' @description
#' Converts the graph to igraph, finds communities and plots (non)interactivly.
#'
#' @param graph an arbitrary graph object in R.
#' @param directed TRUE if the graph is directed (default = FALSE)
#' @param community (default = TRUE)
#' @param betweenness (default = TRUE)
#' @param interactive (default = TRUE)
#'
#'
#' @author  Elyas Heidari, Vahid Balazadeh
#'
#' @return (If interactive = FALSE it plots the noninteractive graph) A list contains:
#' \item{graph}{an igraph object}
#' \item{betweenness}{betweenness measurements of each edge. (if betweenness = TRUE)}
#' \item{network}{a highcharter plot of the graph. (if interactive = TRUE)}
#'
#' @export
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  igraph cluster_louvain betweenness membership V layout_with_fr
#' @importFrom  methods as


graph.vis <-
  function(graph,
           directed = F,
           community = T,
           betweenness = T,
           interactive = T) {
    ig <- methods::as(graph, "igraph")
    if (betweenness) {
      bt <-
        sort(igraph::betweenness(ig, igraph::V(ig), directed = directed),
             decreasing = T)
    } else {
      bt <- NULL
    }
    if (community) {
      fc <- igraph::cluster_louvain(igraph::as.undirected(ig))
      igraph::V(ig)$community <- igraph::membership(fc)
      igraph::V(ig)$color <- igraph::V(ig)$community
      mark_list <-
        lapply(c(1:length(unique(
          igraph::V(ig)$community
        ))), function(x)
          igraph::V(ig)[igraph::V(ig)$community == x])
    } else {
      mark_list <- NULL
      igraph::V(ig)$color <- '#FF7F24'
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

    if (!interactive)
      igraph::plot.igraph(
        ig,
        vertex.label = "",
        layout = ig$layout,
        vertex.size = 1 + 600 / (100 + length(V(ig))),
        vertex.color = igraph::V(ig)$color,
        mark.groups = ig$mark.group
      )

    list(graph = ig,
         betweenness = bt,
         network = vs)

  }
