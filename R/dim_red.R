#' Dimensionality reduction and visualization.
#'
#'
#' @description
#' Reduce dimensionality with a method in \{ tsne, umap, repeated tsne, Momenton umap, L-BFGS umap, Early Exaggeration tsne, and Late Exaggeration tsne\}.
#'
#' @param X a dataset to be reduced in dimension.
#' @param method a character string as the name of the method. Available values are "tsne" (the default), "tsne_rep", "tsne_early", "tsne_late", "umap", "umap_mom",  "umap_l_bfgs".
#' @param verbose a logical value. If TRUE (the default) the details will be printed.
#'
#'
#' @author Elyas Heidari with a refrence smallvis package
#' @return a plot of data points in the 2 dimensional space.
#'
#'
#' @usage dim.reduce(X, method = "tsne", verbose = TRUE)
#' @export dim.reduce
#'
#'
#' @importFrom smallvis smallvis smallvis_rep


dim.reduce <- function(X, method = "tsne", verbose = TRUE) {
  # By default, we use all numeric columns found in a data frame, so you don't need to filter out factor or strings
  # set verbose = TRUE to log progress to the console
  # Automatically plots the results during optimization
  if (method == "tsne") {
    return(smallvis::smallvis(X, perplexity = 25, verbose = verbose))
  }


  # UMAP: see https://github.com/lmcinnes/umap
  # UMAP also has extra parameters, but we use the defaults here
  if (method == "umap") {
    return(smallvis::smallvis(
      X,
      method = "umap",
      perplexity = 25,
      eta = 0.01,
      verbose = verbose
    ))
  }





  # Repeat embedding 10 times and keep the one with the best cost
  if (method == "tsne_rep") {
    return(smallvis::smallvis_rep(
      nrep = 10,
      X = X,
      perplexity = 25,
      ret_extra = TRUE,
      verbose = verbose
    ))
  }


  # Classical momentum optimization instead of delta-bar-delta
  if (method == "umap_mom") {
    return(smallvis::smallvis(
      X,
      scale = FALSE,
      opt = list("mom", eta = 1e-2, mu = 0.8),
      method = "umap",
      Y_init = "spca",
      verbose = verbose
    ))
  }

  # L-BFGS optimization via the mize package
  if (method == "umap_l_bfgs") {
    smallvis::smallvis(
      X,
      scale = FALSE,
      opt = list("l-bfgs", c1 = 1e-4, c2 = 0.9),
      method = "umap",
      Y_init = "spca",
      max_iter = 300,
      verbose = verbose
    )
  }

  # Early Exaggeration
  if (method == "tsne_early") {
    smallvis::smallvis(
      X,
      eta = 100,
      exaggeration_factor = 4,
      stop_lying_iter = 100,
      verbose = verbose
    )
  }

  # and Late Exaggeration as suggested by Linderman and co-workers
  if (method == "tsne_late") {
    return(
      smallvis::smallvis(
        X,
        eta = 100,
        exaggeration_factor = 4,
        stop_lying_iter = 100,
        late_exaggeration_factor = 1.5,
        start_late_lying_iter = 900,
        verbose = verbose
      )
    )
  }

}
