#' construct and visualize Log Linear Model.
#'
#' @description
#' Fits a Log Linear Model to categorical-valued dataset and visualizes the fitted Graphical Model.
#'
#' @param  data A normalized dataframe or matrix with no missing data of continuous measurements.
#' @param  methods (default = "glasso")
#' @param  threshold A threshold for partial correlation thresholding method (default = 0.05). To be used only when the method "threshold" is used.
#' @param  significance A cutoff for edge significance (default = 0.05). To be used only when the method "significance" is used.
#' @param community (default = TRUE)
#'
#' @author Elyas Heidari
#'
#' @export
#'
#' @return A list in which each element is the details of a specific fitted method.
#' \item{graph}{an igraph object of the  model.}
#' \item{betweenness}{betweenness measurements of each edge.}
#' \item{network}{a highcharter plot of the  model.}
#'
#' @importFrom  visNetwork toVisNetworkData visNetwork visOptions
#' @importFrom  gRbase stepwise
#' @importFrom  purrr map
#' @importFrom  gRim dmod
#' @importFrom  igraph cluster_louvain betweenness membership V intersection
#' @importFrom  methods as

llm <-
  function(data,
           threshold = 0.05,
           significance = 0.05,
           methods = c("bic"),
           community = TRUE) {
    model <- gRim::dmod( ~ . ^ ., data = data)
    othermodels <- list()
    if ("aic" %in% methods) {
      othermodels$aic <- aic <- gRbase::stepwise(model)
    }
    if ("bic" %in% methods) {
      othermodels$bic <- gRbase::stepwise(model, k = log(nrow(data)))
    }
    if ("test" %in% methods) {
      othermodels$test <- gRbase::stepwise(model, criterion = "test")
    }
    othermodels <- othermodels %>% purrr::map(methods::as, "igraph")
    commonedges <- do.call(igraph::intersection, othermodels)
    bt <-
      igraph::betweenness(methods::as(commonedges, "igraph"),
                          igraph::V(methods::as(commonedges, "igraph")))
    data <-
      visNetwork::toVisNetworkData(methods::as(commonedges, "igraph"))
    if (community) {
      fc <- igraph::cluster_louvain(methods::as(commonedges, "igraph"))
      data$nodes$group <- igraph::membership(fc)
    }
    vs <-
      visNetwork::visNetwork(nodes = data$nodes, edges = data$edges)  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ))
    list(
      graph = methods::as(commonedges, "igraph"),
      betweenness = bt,
      network = vs
    )
  }
