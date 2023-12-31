% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sc-dot.R
\name{sc_dot}
\alias{sc_dot}
\alias{sc_dot,Seurat-method}
\alias{sc_dot,Seurat}
\alias{sc_dot,SingleCellExperiment-method}
\alias{sc_dot,SingleCellExperiment}
\title{sc_dot}
\usage{
sc_dot(
  object,
  features,
  group.by = NULL,
  split.by = NULL,
  cols = c("lightgrey", "blue"),
  col.min = -2.5,
  col.max = 2.5,
  dot.min = 0,
  dot.scale = 6,
  slot = "data",
  .fun = NULL,
  mapping = NULL,
  scale = TRUE,
  scale.by = "radius",
  scale.min = NA,
  scale.max = NA,
  cluster.idents = FALSE,
  ...
)

\S4method{sc_dot}{Seurat}(
  object,
  features,
  group.by = NULL,
  split.by = NULL,
  cols = c("lightgrey", "blue"),
  col.min = -2.5,
  col.max = 2.5,
  dot.min = 0,
  dot.scale = 6,
  slot = "data",
  .fun = NULL,
  mapping = NULL,
  scale = TRUE,
  scale.by = "radius",
  scale.min = NA,
  scale.max = NA,
  cluster.idents = FALSE,
  ...
)

\S4method{sc_dot}{SingleCellExperiment}(
  object,
  features,
  group.by = NULL,
  split.by = NULL,
  cols = c("lightgrey", "blue"),
  col.min = -2.5,
  col.max = 2.5,
  dot.min = 0,
  dot.scale = 6,
  slot = "data",
  .fun = NULL,
  mapping = NULL,
  scale = TRUE,
  scale.by = "radius",
  scale.min = NA,
  scale.max = NA,
  cluster.idents = FALSE,
  ...
)
}
\arguments{
\item{object}{Seurat or SingleCellExperiment object}

\item{features}{selected features}

\item{group.by}{grouping factor}

\item{split.by}{additional split factor}

\item{cols}{colors of the points}

\item{col.min}{minimum scaled averaged expression threshold}

\item{col.max}{maximum scaled averaged expression threshold}

\item{dot.min}{the threshold of percentage of cells for the the smallest dot}

\item{dot.scale}{Scaling factor for size of points}

\item{slot}{slot to pull expression data from (e.g., 'count' or 'data')}

\item{.fun}{user defined function that will be applied to selected features (default is NULL and there is no data operation)}

\item{mapping}{aesthetic mapping}

\item{scale}{whether to scale the expression value (default to TRUE)}

\item{scale.by}{scale the size of the points by \code{size} or \code{radius}}

\item{scale.min}{lower limit of scaling}

\item{scale.max}{upper limit of scaling}

\item{cluster.idents}{Order identities by hierarchical clusters based on average expression and perventage of expression (default is FALSE)}

\item{...}{additional parameters pass to 'ggplot2::geom_point()'}
}
\value{
dot plot to visualize feature expression distribution
}
\description{
sc_dot
}
\examples{
library(scuttle)
library(scater)
library(scran)
library(ggplot2)
sce <- mockSCE()
sce <- logNormCounts(sce)
set.seed(123)
genes <- rownames(sce) |> sample(6) 
sc_dot(sce, genes[1:5], 'Treatment', slot = 'logcounts')

}
\seealso{
\link[Seurat:DotPlot]{DotPlot};
}
