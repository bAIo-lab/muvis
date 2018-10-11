#' Dimensionality reduction and visualization.
#'
#'
#' @description
#' Reduce dimensionality with a method in \{tsne, umap, pca\}.
#'
#' @param data   A normalized dataframe or matrix with no missing data to be reduced in dimension.
#' @param method A character string as the name of the method. Available values are "pca" (the default), "tsne", "umap".
#' @param annot1 A vector of continuous or factor values to color samples in the resulted plot (the order of values should be the same as the order of rows in data). Default to NULL.
#' @param annot1.name The name of the variable indicating annot1 vector. Defaults to "annot1".
#' @param annot2 A vector of factor values indicating sample shapes to plot (the order of values should be the same as the order of rows in data). Default to NULL.
#' @param annot2.name The name of the variable indicating annot2 vector. Defaults to "annot2".
#' @param levels An integer value indicating the maximum number of levels of a categorical variable. To be used to distinguish the categorical variable.
#' Defaults to NULL because it is supposed that \code{data} has been preprocessed using \code{\link[muvis]{data_preproc}} and the categorical variables are specified.
#' If it is set, first will run \code{\link[muvis]{data_preproc}} to specify categorical and continuous variables.
#'
#'
#' @author Elyas Heidari
#' @return A plot of data points in the 2 dimensional space.
#'
#' @export
#'
#' @examples
#' data("NHANES")
#'
#' ## Using different methods on the raw data
#' df <- NHANES[sample(nrow(NHANES), 500), ]
#' plt_pca <- dim_reduce(df, method = "pca", levels = 15)
#' plt_tsne <- dim_reduce(df, method = "tsne", annot1 = df$BMXBMI, annot1.name = "BMI", levels = 15)
#' plt_umap <- dim_reduce(df, method = "umap", annot1 = df$LBXTC, annot1.name = "Total Cholesterol",
#' annot2 = as.factor(df$RIAGENDR), annot2.name = "Gender", levels = 15)
#' @importFrom Rtsne Rtsne
#' @importFrom stats prcomp dist
#' @import     ggplot2
#' @importFrom smallvis smallvis
#' @importFrom leaflet colorNumeric

dim_reduce <-
  function(data,
           method = "pca",
           annot1 = NULL,
           annot1.name = "annot1",
           annot2 = NULL,
           annot2.name = "annot2",
           levels = NULL) {
    if (!is.null(levels))
      data <- data_preproc(data, levels = levels)

    data <- data.matrix(data)
    theme_Publication <- ggplot2::theme_minimal

    color_set <- c("#440154FF",
                   "#3B528BFF",
                   "#FDE725FF",
                   "#21908CFF",
                   "#5DC863FF"
    )



    tsneRun <- function(distMat,
                        perplexity = 10,
                        theta = 0,
                        max_iter = 5000) {
      tsneRes <-
        Rtsne::Rtsne(
          distMat,
          perplexity = perplexity,
          theta = theta,
          max_iter = max_iter,
          is_distance = TRUE,
          dims = 2
        )
      tsneRes <- tsneRes$Y
      rownames(tsneRes) <- labels(distMat)
      colnames(tsneRes) <- c("x", "y")
      tsneRes
    }

    if (is.null(annot1)) {
      if (method == "pca") {
        pcRes <- stats::prcomp(data, scale. = FALSE, center = TRUE)
        eigs <- pcRes$sdev ^ 2
        eigs <- eigs / sum(eigs)
        plotTab <- data.frame(pcRes$x[, c(1, 2)])
        to.ret <- ggplot2::ggplot(plotTab, ggplot2::aes(x = PC1, y = PC2)) +
          ggplot2::geom_point(col=color_set[2]) +
          ggplot2::ggtitle(paste("Scatter plot for PCA")) +
          theme_Publication() +
          ggplot2::labs(x = "component 1", y = "component 2")
      }

      if (method == "tsne") {
        distViab <- stats::dist(data)
        plotTab <-
          data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
        plotTab$sampleID <- rownames(plotTab)
        to.ret <- ggplot2::ggplot(plotTab, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_point(col=color_set[2]) +
          ggplot2::ggtitle(paste("Scatter plot for t-SNE")) +
          theme_Publication() + scale_fill_Publication() +
          ggplot2::labs(x = "component 1", y = "component 2")

      }
      if (method == "umap") {
        distViab <- dist(data)
        plotTab <-
          data.frame(
            smallvis::smallvis(
              t(distViab),
              method = "umap",
              perplexity = 40,
              eta = 0.01,
              epoch_callback = FALSE,
              verbose = FALSE
            )
          )
        plotTab$sampleID <- rownames(plotTab)
        X1 <- NULL
        X2 <- NULL
        to.ret <- ggplot2::ggplot(plotTab, ggplot2::aes(x = X1, y = X2)) +
          ggplot2::geom_point(col = color_set[2]) +
          ggplot2::ggtitle(paste("Scatter plot for UMAP")) +
          theme_Publication() +
          ggplot2::labs(x = "component 1", y = "component 2")
      }
    } else{
      if(is.factor(annot1)){
        pal <-leaflet::colorNumeric(color_set, domain = c(1:nlevels(annot1)))
        scale <- ggplot2::scale_color_manual(values = sapply(c(1:nlevels(annot1)), pal))
      } else {
        scale <- ggplot2::scale_color_gradientn(colours = color_set)
      }
      if (is.null(annot2)) {
        if (method == "pca") {
          pcRes <- prcomp(data, scale. = FALSE, center = TRUE)
          eigs <- pcRes$sdev ^ 2
          eigs <- eigs / sum(eigs)
          plotTab <- data.frame(pcRes$x[, c(1, 2)])
          PC1 <- NULL
          PC2 <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(x = PC1, y = PC2, col = annot1)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste("Scatter plot for PCA")) +
            theme_Publication() +
            ggplot2::labs(x = "component 1",
                          y = "component 2",
                          colour = annot1.name) + scale
        }
        if (method == "tsne") {
          distViab <- dist(data)
          plotTab <-
            data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
          plotTab$sampleID <- rownames(plotTab)
          x <- NULL
          y <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(x = x, y = y, col = annot1)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste("Scatter plot for t-SNE")) +
            theme_Publication() +
            ggplot2::labs(x = "component 1",
                          y = "component 2",
                          colour = annot1.name) + scale

        }
        if (method == "umap") {
          distViab <- stats::dist(data)
          plotTab <-
            data.frame(
              smallvis::smallvis(
                t(distViab),
                method = "umap",
                perplexity = 40,
                eta = 0.01,
                epoch_callback = FALSE,
                verbose = FALSE
              )
            )
          plotTab$sampleID <- rownames(plotTab)
          X1 <- NULL
          X2 <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(x = X1, y = X2, col = annot1)) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste("Scatter plot for UMAP")) +
            theme_Publication() + scale_fill_Publication() +
            ggplot2::labs(x = "component 1",
                          y = "component 2",
                          colour = annot1.name) + scale
        }
      } else{
        if (method == "pca") {
          pcRes <- stats::prcomp(data, scale. = FALSE, center = TRUE)
          eigs <- pcRes$sdev ^ 2
          eigs <- eigs / sum(eigs)
          plotTab <- data.frame(pcRes$x[, c(1, 2)])
          PC1 <- NULL
          PC2 <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(x = PC1,
                                         y = PC2)) +
            ggplot2::geom_point(ggplot2::aes(colour = annot1,
                                    shape = annot2)) +
            ggplot2::ggtitle(paste("Scatter plot for PCA")) +
            theme_Publication() + scale_fill_Publication() +
            ggplot2::labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            ) + scale
        }
        if (method == "tsne") {
          distViab <- stats::dist(data)
          plotTab <-
            data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
          x <- NULL
          y <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(
              x = x,
              y = y,
              col = annot1,
              shape = annot2
            )) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste("Scatter plot for t-SNE")) +
            theme_Publication() + scale_fill_Publication() +
            ggplot2::labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            ) + scale

        }
        if (method == "umap") {
          distViab <- stats::dist(data)
          plotTab <-
            data.frame(
              smallvis::smallvis(
                t(distViab),
                method = "umap",
                perplexity = 40,
                eta = 0.01,
                epoch_callback = FALSE,
                verbose = FALSE
              )
            )
          X1 <- NULL
          X2 <- NULL
          to.ret <-
            ggplot2::ggplot(plotTab, ggplot2::aes(
              x = X1,
              y = X2,
              col = annot1,
              shape = annot2
            )) +
            ggplot2::geom_point() +
            ggplot2::ggtitle(paste("Scatter plot for UMAP")) +
            theme_Publication() + scale_fill_Publication() +
            ggplot2::labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            ) + scale
        }
      }
    }
    to.ret
  }
