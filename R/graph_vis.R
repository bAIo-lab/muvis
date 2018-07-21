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
#' @param plot.community If TRUE it plots each community separately (default = TRUE)
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
#' @importFrom  igraph cluster_louvain betweenness membership V layout_with_fr
#' @importFrom  methods as
#' @importFrom  RColorBrewer brewer.pal

graph.vis <-
  function(graph,
           directed = F,
           community = T,
           betweenness = T,
           plot = T,
           plot.community = T) {

    plot_community <- function(graph, community_num) {

      t <- igraph::V(graph)$community == community_num
      v <-  igraph::V(graph)[t]
      layout <- graph$layout
      l<- layout[which(t), ]
      sub_graph <- igraph::induced.subgraph(graph, v)
      igraph::plot.igraph(sub_graph, vertex.size = 2 + 600 / (100 + length(v)),
                          layout=l, main=paste('community', community_num, sep = " "), vertex.label.family='Helvetica',
                          vertex.label.dist = -.5, vertex.label.cex = .8, vertex.label.font = 3, vertex.label.color='black')
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
      mark_list <-
        lapply(c(1:community_n), function(x)
          igraph::V(ig)[igraph::V(ig)$community == x])
    } else {
      mark_list <- NULL
    }
    colrs <- RColorBrewer::brewer.pal(community_n + 1, "Set3")
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
        vertex.size = 2 + 600 / (100 + length(igraph::V(ig))),
        vertex.color = igraph::V(ig)$color
      )
    if (community) {
      if (plot.community) {
        for (i in 1:community_n)
          plot_community(ig, i)
      }
      com <- igraph::V(ig)$community
      names(com) <- names(igraph::V(ig))
      list(graph = ig,
          betweenness = bt,
          network = vs, communities = com)
    } else {
      list(graph = ig,
           betweenness = bt,
           network = vs)
    }

  }
