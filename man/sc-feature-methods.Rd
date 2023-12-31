% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc-feature.R
\name{sc_feature}
\alias{sc_feature}
\alias{sc_feature,Seurat-method}
\alias{sc_feature,Seurat}
\alias{sc_feature,SingleCellExperiment-method}
\alias{sc_feature,SingleCellExperiment}
\title{sc_feature}
\usage{
sc_feature(
  object,
  features,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  ncol = 3,
  density = FALSE,
  grid.n = 100,
  joint = FALSE,
  joint.fun = prod,
  common.legend = TRUE,
  ...
)

\S4method{sc_feature}{Seurat}(
  object,
  features,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  ncol = 3,
  density = FALSE,
  grid.n = 100,
  joint = FALSE,
  joint.fun = prod,
  common.legend = TRUE,
  ...
)

\S4method{sc_feature}{SingleCellExperiment}(
  object,
  features,
  dims = c(1, 2),
  reduction = NULL,
  cells = NULL,
  slot = "data",
  mapping = NULL,
  ncol = 3,
  density = FALSE,
  grid.n = 100,
  joint = FALSE,
  joint.fun = prod,
  common.legend = TRUE,
  ...
)
}
\arguments{
\item{object}{Seurat object}

\item{features}{selected features (i.e., genes)}

\item{dims}{selected dimensions (must be a two-length vector)
that are used in visualization}

\item{reduction}{reduction method, default is NULL and will
use the default setting store in the object}

\item{cells}{selected cells to plot (default is all cells)}

\item{slot}{slot to pull expression data from (e.g., 'count' or 'data')}

\item{mapping}{aesthetic mapping}

\item{ncol}{number of facet columns if 'length(features) > 1'}

\item{density}{whether plot the 2D weighted kernel density, default is FALSE.}

\item{grid.n}{number of grid points in the two directions to estimate 2D
weighted kernel density, default is 100.}

\item{joint}{whether joint the multiple features with \code{joint.fun},
default is FALSE.}

\item{joint.fun}{how to joint the multiple features if \code{joint=TRUE},
default is prod.}

\item{common.legend}{whether to use \code{facet_wrap} to display the multiple
\code{features}, default is TRUE.}

\item{...}{additional parameters pass to 'scattermore::geom_scattermore()'}
}
\value{
dimension reduction plot colored by selected features
}
\description{
sc_feature
}
\examples{
library(scuttle)
library(scater)
library(scran)
library(ggplot2)
sce <- mockSCE()
sce <- logNormCounts(sce)
clusters <- clusterCells(sce, assay.type = 'logcounts')
colLabels(sce) <- clusters
sce <- runTSNE(sce, assay.type = 'logcounts')
set.seed(123)
genes <- rownames(sce) |> sample(6)
p1 <- sc_feature(sce, genes[1], slot='logcounts', reduction = 'TSNE')
p2 <- sc_feature(sce, genes, slot='logcounts', reduction = 'TSNE')
f1 <- sc_dim(sce, slot='logcounts', reduction = 'TSNE') +
      sc_dim_geom_feature(sce, genes[1], color='black')
f2 <- sc_dim(sce, alpha=.3, slot='logcounts', reduction = 'TSNE') +
    ggnewscale::new_scale_color() +
    sc_dim_geom_feature(sce, genes, mapping=aes(color=features)) +
    scale_color_viridis_d()
p1 + p2 + f1 + f2
}
