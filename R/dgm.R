#' construct and visualize causal (directed) networks
#'
#' @description
#' A wrapper of \code{\link[pcalg]{fci}} function in \code{pcalg} package. Estimates a simplified Partial Ancestral Graph (PAG) using FCI algorithm.
#'
#' @param  data A normalized dataframe or matrix with no missing data.
#' @param  dtype Either "guassian" for continuous/ordinal discrete data or "discrete" for discrete/nominal data. (default = "gaussian") See details for more description.
#' @param  alpha significance level (number in (0,1)) for the individual conditional independence tests. Defaults to 1e-5.
#' @param  community A logical value to show if the node communities should be detected and colored in the returned graph. (default = TRUE)
#' @param  betweenness A logical value to show if the node betweenness measurements should be computed and returned from the function. (default = TRUE)
#' @param  plot A logical value to show if the graph should be plotted. (default = FLASE)
#' @param  levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable.
#' Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' If it is set, first will run \code{\link[muvis]{data_preproc}} to specify categorical and continuous variables.
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
#' @examples
#' data("NHANES")
#' ## Using raw data
#' ## Using "gaussian" method for continuous data
#' gaussian_dgm <- dgm(data = NHANES[1:1000, ], dtype = "gaussian", levels = 10)
#'
#' ## Using "discrete" method for categorical data
#' discrete_dgm <- dgm(data = NHANES[1000:1200, ], dtype = "discrete", levels = 5)
#'
#' ## Using preprocessed data
#' data <- data_preproc(NHANES, levels = 15)
#' data$SEQN <- NULL
#' prep_gauss_dgm <- dgm(data = data[1:1000, ], plot = TRUE)
#'
#'
#'
#' @return A list in which each element is the details of a specific fitted dtype.
#' \item{significance}{A data.frame containing edges with p.values.}
#' \item{graph}{an igraph object of the graphical model.}
#' \item{betweenness}{betweenness measurements of each node.}
#' \item{communities}{a named vector indicating the community of each node.}
#' \item{network}{a visNetwork plot of the graphical model.}
#'
#' @export
#'
#' @importFrom  stats C cov.wt cov2cor
#' @importFrom  igraph graph_from_adjacency_matrix
#' @importFrom  pcalg fci gaussCItest disCItest
#' @importFrom  dplyr %>%



dgm <-
  function(data,
           alpha = 1e-5,
           dtype = "gaussian",
           community = TRUE,
           betweenness = TRUE,
           plot = FALSE,
           levels = NULL) {
    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)

    is.cat <- function(var) {
      return(is.factor(var))
    }

    if (dtype == "gaussian") {
      data <- data[, sapply(data, function(x)
        ! is.cat(x))]
      S.data <- stats::cov.wt(data, method = "ML")$cov
      C.data <- stats::cov2cor(S.data)
      suffStat <- list(C = C.data, n = nrow(data))
      fci.data <-
        pcalg::fci(suffStat,
                        pcalg::gaussCItest,
                        p = ncol(data),
                        alpha = alpha)
      fci.data@amat <- ifelse(fci.data@amat == 2, 1, 0)
      rownames(fci.data@amat) <- names(data)
      colnames(fci.data@amat) <- names(data)
      ig <- igraph::graph_from_adjacency_matrix(fci.data@amat)
      val <- graph_vis(
        ig,
        directed = T,
        community = community,
        betweenness = betweenness,
        plot = plot
      )

    }
    if (dtype == "discrete") {
      data <-
        data[, sapply(data, function(x)
          is.cat(x) & (length(unique(x)) > 1))]
      data <- sapply(data, function(x)
        as.integer(x) - 1)
      data <- data.frame(data)
      suffStat <- list(dm = data, adaptDF = T)

      fci.data <-
        pcalg::fci(suffStat,
                        pcalg::disCItest,
                        p = ncol(data),
                        alpha = alpha)
      fci.data@amat <- ifelse(fci.data@amat == 2, 1, 0)
      rownames(fci.data@amat) <- names(data)
      colnames(fci.data@amat) <- names(data)
      ig <- igraph::graph_from_adjacency_matrix(fci.data@amat)
      val <-
        graph_vis(
          ig,
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
      )) %>% visNetwork::visEdges(arrows = "to", smooth = F)
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
