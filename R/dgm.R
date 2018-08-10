#' construct and visualize causal (directed) networks
#'
#' @description
#' A wrapper of skeleton function in pcalg package. Estimates the skeleton of a Directed Acyclic Graph (DAG) using PC, FCI, or RFCI algorithms.
#'
#' @param  data A normalized dataframe or matrix with no missing data.
#' @param  dtype Either "guassian" for continuous/ordinal discrete data or "discrete" for discrete/nominal data. (default = "gaussian") See details for more description.
#' @param  alpha A threshold for partial correlation thresholding dtype (default = 0.05). To be used only when the dtype "threshold" is used.
#' @param  community A logical value to show if the node communities should be detected and colored in the returned graph. (default = TRUE)
#' @param  betweenness A logical value to show if the node betweenness measurements should be computed and returned from the function. (default = TRUE)
#' @param  plot A logical value to show if the graph should be plotted. (default = TRUE)
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable. Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' @param  ... Remaining parameters of skleton function.
#'
#' @details There is no specific distribution needed for the data. The parameter dtype will be used for determining the data type to be provided as the input of the function. However, it is highly recommended to use "guassian" data type for both continuous and ordinal discrete data.
#'
#' @references  D. Colombo and M.H. Maathuis (2014).Order-independent constraint-based causal structure learning. Journal of Machine Learning Research 15 3741-3782.
#' @references  D. Colombo, M. H. Maathuis, M. Kalisch, T. S. Richardson (2012). Learning high-dimensional directed acyclic graphs with latent and selection variables. Ann. Statist. 40, 294-321.
#' @references  M. Kalisch and P. Buehlmann (2007). Estimating high-dimensional directed acyclic graphs with the PC-algorithm, JMLR 8 613-636.
#' @references  P. Spirtes, C. Glymour and R. Scheines (2000). Causation, Prediction, and Search, 2nd edition, MIT Press.
#'
#' @author Elyas Heidari
#'
#'
#' @return A list in which each element is the details of a specific fitted dtype.
#' \item{significanse}{A data.frame containing edges with p.values.}
#' \item{graph}{an igraph object of the graphical model.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{communities}{a named vector indicating the community of each node.}
#' \item{network}{a visNetwork plot of the graphical model.}
#'
#' @export
#'
#' @importFrom  stats C cov.wt cov2cor
#' @importFrom  graph nodes
#' @importFrom  pcalg skeleton gaussCItest disCItest



dgm <-
  function(data,
           alpha = 1e-5,
           dtype = "gaussian",
           community = TRUE,
           betweenness = TRUE,
           plot = TRUE,
           levels = NULL,
           ...) {
    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)

    if (dtype == "gaussian") {
      S.data <- stats::cov.wt(data, method = "ML")$cov
      C.data <- stats::cov2cor(S.data)
      suffStat <- list(C = C.data, n = nrow(data))
      skeleton.data <-
        pcalg::skeleton(suffStat,
                        pcalg::gaussCItest,
                        p = ncol(data),
                        alpha = alpha,
                        ...)
      graph::nodes(skeleton.data@graph) <- names(data)
      val <- graph_vis(
        skeleton.data@graph,
        directed = T,
        community = community,
        betweenness = betweenness,
        plot = plot
      )

    }
    if (dtype == "discrete") {
      data <- sapply(data, function(x) as.integer(x) - 1)
      data <- data.frame(data)
      suffStat <- list(dm = data, adaptDF = T)

      skeleton.data <-
        pcalg::skeleton(suffStat,
                        pcalg::disCItest,
                        p = ncol(data),
                        alpha = alpha,
                        ...)
      graph::nodes(skeleton.data@graph) <- names(data)
      val <-
        graph_vis(
          skeleton.data@graph,
          directed = T,
          community = community,
          betweenness = betweenness,
          plot = plot
        )
    }
    vnet <- val$network
    edges <- vnet$x$edges

    test_matrix <-
      test_assoc(data, vnet$x$nodes$id, levels = levels)
    e_names <- rbind(edges$from, edges$to)
    p_values <-
      apply(e_names, 2, function(x)
        test_matrix[x[1], x[2]])
    title = paste0("<p>", paste("p.value =", p_values), "</p>")
    edges[, "title"] <- title

    significance <- data.frame(edges$from, edges$to)
    significance$p.value <- p_values

    vn <-
      visNetwork::visNetwork(vnet$x$nodes, edges, height = "500px", width = "100%")  %>%
      visNetwork::visOptions(highlightNearest = list(
        enabled = T,
        degree = 1,
        hover = T
      )) %>% visNetwork::visEdges(arrows = "to")
    if (community) {
      to.ret <- list(
        significance = significance,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn,
        communities = val$communities
      )
    } else {
      to.ret <- list(
        significance = significance,
        graph = val$graph,
        betweenness = val$betweenness,
        network = vn
      )
    }
  }
