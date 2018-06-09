#' Dimensionality reduction and visualization.
#'
#'
#' @description
#' A function to reduce dimensionality using seven methods, namely, tsne, umap, repeated tsne, Momenton umap, L-BFGS umap, Early Exaggeration tsne, and Late Exaggeration tsne.
#'
#' @param X a dataset to be reduced in dimension.
#'
#'
#' @author Elyas Heidari
#' @return a list of seven plots each of which is for a method mentioned in the description.
#'
#'
#' @examples
#'
#' @export
#'
#' @importFrom smallvis smallvis smallvis_rep


dim_reduce <- function(X) {

  plt <- list()
  # By default, we use all numeric columns found in a data frame, so you don't need to filter out factor or strings
  # set verbose = TRUE to log progress to the console
  # Automatically plots the results during optimization
  plt$tsne_X <- smallvis(X, perplexity = 25, verbose = TRUE)


  # UMAP: see https://github.com/lmcinnes/umap
  # UMAP also has extra parameters, but we use the defaults here
  plt$umap_X <-
    smallvis::smallvis(X,
             method = "umap",
             perplexity = 25,
             eta = 0.01)





  # Repeat embedding 10 times and keep the one with the best cost
  plt$tsne_X_best <-
    smallvis::smallvis_rep(
      nrep = 10,
      X = X,
      perplexity = 25,
      ret_extra = TRUE
    )


  # Classical momentum optimization instead of delta-bar-delta
  plt$umap_X_mom <-
    smallvis::smallvis(
      X,
      scale = FALSE,
      opt = list("mom", eta = 1e-2, mu = 0.8),
      method = "umap",
      Y_init = "spca"
    )

  # L-BFGS optimization via the mize package
  plt$umap_X_lbfgs <-
    smallvis::smallvis(
      X,
      scale = FALSE,
      opt = list("l-bfgs", c1 = 1e-4, c2 = 0.9),
      method = "umap",
      Y_init = "spca",
      max_iter = 300
    )

  # Early Exaggeration
  plt$tsne_X_ex <-
    smallvis::smallvis(
      X,
      eta = 100,
      exaggeration_factor = 4,
      stop_lying_iter = 100
    )

  # and Late Exaggeration as suggested by Linderman and co-workers
  plt$tsne_X_lex <-
    smallvis::smallvis(
      X,
      eta = 100,
      exaggeration_factor = 4,
      stop_lying_iter = 100,
      late_exaggeration_factor = 1.5,
      start_late_lying_iter = 900
    )
  plt

}
