#' Dimensionality reduction and visualization.
#'
#'
#' @description
#' Reduce dimensionality with a method in \{tsne, umap, pca\}.
#'
#' @param data a dataset to be reduced in dimension.
#' @param method a character string as the name of the method. Available values are "pca" (the default), "tsne", "umap".
#' @param annot1 Defaults to NULL.
#' @param annot1.name Defaults to "annot1".
#' @param annot2 Defaults to NULL.
#' @param annot2.name Defaults to "annot2".
#'
#'
#' @author Elyas Heidari
#' @return A plot of data points in the 2 dimensional space.
#'
#' @export
#'
#' @importFrom Rtsne Rtsne
#' @importFrom stats prcomp dist
#' @import     ggplot2
#' @importFrom smallvis smallvis

dim_reduce <-
  function(data,
           method = "pca",
           annot1 = NULL,
           annot1.name = "annot1",
           annot2 = NULL,
           annot2.name = "annot2") {

    theme_Publication <- function(base_size = 14) {
      (
        ggthemes::theme_foundation(base_size = base_size)
        + ggplot2::theme(
          plot.title = ggplot2::element_text(
            face = "bold",
            size = ggplot2::rel(1.2),
            hjust = 0.5
          ),
          text = ggplot2::element_text(),
          panel.background = ggplot2::element_rect(colour = NA),
          plot.background = ggplot2::element_rect(colour = NA),
          panel.border = ggplot2::element_rect(colour = NA),
          axis.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(1)),
          axis.title.y = ggplot2::element_text(angle = 90, vjust = 2),
          axis.title.x = ggplot2::element_text(vjust = -0.2),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.ticks = ggplot2::element_line(),
          panel.grid.major = ggplot2::element_line(colour = "#f0f0f0"),
          panel.grid.minor = ggplot2::element_blank(),
          legend.key = ggplot2::element_rect(colour = NA),
          legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.size = grid::unit(0.2, "cm"),
          legend.spacing = grid::unit(0, "cm"),
          legend.title = ggplot2::element_text(face = "italic"),
          plot.margin = grid::unit(c(10, 5, 5, 5), "mm"),
          strip.background = ggplot2::element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
          strip.text = ggplot2::element_text(face = "bold")
        )
      )

    }

    scale_fill_Publication <- function(...) {
      ggplot2::discrete_scale("fill", "Publication", scales::manual_pal(
        values = c(
          "#386cb0",
          "#fdb462",
          "#7fc97f",
          "#ef3b2c",
          "#662506",
          "#a6cee3",
          "#fb9a99",
          "#984ea3",
          "#ffff33"
        )
      ), ...)

    }

    scale_colour_Publication <- function(...) {
      discrete_scale("colour",
                     "Publication",
                     scales::manual_pal(
                       values = c(
                         "#386cb0",
                         "#fdb462",
                         "#7fc97f",
                         "#ef3b2c",
                         "#662506",
                         "#a6cee3",
                         "#fb9a99",
                         "#984ea3",
                         "#ffff33"
                       )
                     ),
                     ...)
    }


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
        to.ret <- ggplot(plotTab, aes(x = PC1, y = PC2)) +
          geom_point() +
          ggtitle(paste("Scatter plot for PCA")) +
          theme_Publication() + scale_fill_Publication() +
          labs(x = "component 1", y = "component 2")
      }

      if (method == "tsne") {
        distViab <- stats::dist(data)
        plotTab <-
          data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
        plotTab$sampleID <- rownames(plotTab)
        to.ret <- ggplot(plotTab, aes(x = x, y = y)) +
          geom_point() +
          ggtitle(paste("Scatter plot for t-SNE")) +
          theme_Publication() + scale_fill_Publication() +
          labs(x = "component 1", y = "component 2")

      }
      if (method == "umap") {
        distViab <- dist(data)
        plotTab <-
          smallvis::smallvis(
            t(distViab),
            method = "umap",
            perplexity = 40,
            eta = 0.01,
            epoch_callback = FALSE,
            verbose = FALSE
          )
        plotTab$sampleID <- rownames(plotTab)
        to.ret <- ggplot(plotTab, aes(x = x, y = y)) +
          geom_point() +
          ggtitle(paste("Scatter plot for UMAP")) +
          theme_Publication() + scale_fill_Publication() +
          labs(x = "component 1", y = "component 2")
      }
    } else{
      if (is.null(annot2)) {
        if (method == "pca") {
          pcRes <- prcomp(data, scale. = FALSE, center = TRUE)
          eigs <- pcRes$sdev ^ 2
          eigs <- eigs / sum(eigs)
          plotTab <- data.frame(pcRes$x[, c(1, 2)])
          PC1 <- NULL
          PC2 <- NULL
          to.ret <-
            ggplot(plotTab, aes(x = PC1, y = PC2, col = annot1)) +
            geom_point() +
            ggtitle(paste("Scatter plot for PCA")) +
            theme_Publication() + scale_fill_Publication() +
            labs(x = "component 1",
                 y = "component 2",
                 colour = annot1.name)
        }
        if (method == "tsne") {
          distViab <- dist(data)
          plotTab <-
            data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
          plotTab$sampleID <- rownames(plotTab)
          x <- NULL
          y <- NULL
          to.ret <- ggplot(plotTab, aes(x = x, y = y, col = annot1)) +
            geom_point() +
            ggtitle(paste("Scatter plot for t-SNE")) +
            theme_Publication() + scale_fill_Publication() +
            labs(x = "component 1",
                 y = "component 2",
                 colour = annot1.name)

        }
        if (method == "umap") {
          distViab <- stats::dist(data)
          plotTab <-
            smallvis::smallvis(
              t(distViab),
              method = "umap",
              perplexity = 40,
              eta = 0.01,
              epoch_callback = FALSE,
              verbose = FALSE
            )
          plotTab$sampleID <- rownames(plotTab)
          to.ret <- ggplot(plotTab, aes(x = x, y = y, col = annot1)) +
            geom_point() +
            ggtitle(paste("Scatter plot for UMAP")) +
            theme_Publication() + scale_fill_Publication() +
            labs(x = "component 1",
                 y = "component 2",
                 colour = annot1.name)
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
            ggplot(plotTab, aes(
              x = PC1,
              y = PC2,
              col = annot1,
              shape = annot2
            )) +
            geom_point() +
            ggtitle(paste("Scatter plot for PCA")) +
            theme_Publication() + scale_fill_Publication() +
            labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            )
        }
        if (method == "tsne") {
          distViab <- stats::dist(data)
          plotTab <-
            data.frame(tsneRun(distViab, perplexity = 2, max_iter = 10000))
          x <- NULL
          y <- NULL
          to.ret <-
            ggplot(plotTab, aes(
              x = x,
              y = y,
              col = annot1,
              shape = annot2
            )) +
            geom_point() +
            ggtitle(paste("Scatter plot for t-SNE")) +
            theme_Publication() + scale_fill_Publication() +
            labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            )

        }
        if (method == "umap") {
          distViab <- stats::dist(data)
          plotTab <-
            smallvis::smallvis(
              t(distViab),
              method = "umap",
              perplexity = 40,
              eta = 0.01,
              epoch_callback = FALSE,
              verbose = FALSE
            )
          x <- NULL
          y <- NULL
          to.ret <-
            ggplot(plotTab, aes(
              x = x,
              y = y,
              col = annot1,
              shape = annot2
            )) +
            geom_point() +
            ggtitle(paste("Scatter plot for UMAP")) +
            theme_Publication() + scale_fill_Publication() +
            labs(
              x = "component 1",
              y = "component 2",
              colour = annot1.name,
              shape = annot2.name
            )
        }
      }
    }
    to.ret
  }
