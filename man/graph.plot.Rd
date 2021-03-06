% Generated by roxygen2 (4.0.0): do not edit by hand
\name{graph.plot}
\alias{graph.plot}
\title{A wrapper for plotting graphs}
\usage{
graph.plot(g, edge.weight = NULL, min.edge = NULL, max.edge = NULL,
  delete.vertices = NULL, select.vertices = NULL, min.degree = NULL,
  use.tkplot = FALSE, ego = NULL, return.graph = FALSE)
}
\arguments{
\item{g}{A graph object in the \code{\link{igraph}} format}

\item{min.edge.value}{Numerical scalar. Edges with a lower weight will be deleted}

\item{max.edge.value}{Like min.edge.value, but for the max value}

\item{delete.vertices}{Either a logical vector (TRUE/FALSE) with TRUE meaning that a vertex will be deleted, or indices for vertices that will be deleted}

\item{select.vertices}{Same as delete.vertices, but inversed}

\item{min.degree}{Numerical scalar, indicating the minimum number of edges a vertex needs to have in order to be included in the graph. Set to 1 to ignore unconnected vertices}

\item{use.tkplot}{Logical. If TRUE, tkplot is used instead of plot, which allows some interactive editing of the network}

\item{return.graph}{Logical. If TRUE, the function will return the (filtered) graph object}
}
\value{
if return.graph is TRUE, A graph object in the \code{\link{igraph}} format
}
\description{
A wrapper for plotting graphs in the \code{\link{igraph}} format that incorporates the functions to delete edges and vertices
}

