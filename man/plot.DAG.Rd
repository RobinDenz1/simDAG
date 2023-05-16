\name{plot.DAG}
\alias{plot.DAG}

\title{
Plot a \code{DAG} object
}
\description{
Using the node information contained in the \code{DAG} object this function plots the corresponding DAG in a quick and dirty way.
}
\usage{
\method{plot}{DAG}(x, ...)
}
\arguments{
  \item{x}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{\link{empty_dag}} or \code{\link{node}} for more details.
  }
  \item{...}{
Currently not used.
  }
}
\details{
This function uses the \pkg{ggraph} package to plot the directed acyclic graph defined by the \code{DAG} object needed to use the \code{\link{sim_from_dag}} function. This function is meant to be used only for development purposes (e.g. checking if the node lists are correctly specified). It is very simple and does not support any arguments to make the plot better.

If a publication ready graph should be produced, this function should probably not be used. Instead, users can use the \code{\link{dag2matrix}} function to obtain an adjacency matrix from the \code{DAG} and directly use this matrix and the \pkg{ggraph} package to get much better plots. A small example of this is shown below.
}
\author{
Robin Denz
}
\examples{
library(simDAG)
library(ggplot2)
library(igraph)
library(ggraph)

# 2 root nodes, 1 child node
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("sex", "age"), betas=c(1.1, 0.4),
       intercept=-2)

plot(dag)

# do it manually
adj_mat <- dag2matrix(dag)

g1 <- igraph::graph.adjacency(adjmatrix=adj_mat, mode="directed")

# this code produces equivalent results as the code above
ggraph::ggraph(g1, layout="graphopt") +
    ggraph::geom_edge_link(ggplot2::aes(
      start_cap=ggraph::label_rect(.data$node1.name),
      end_cap=ggraph::label_rect(.data$node2.name)),
      arrow=ggplot2::arrow(length=unit(4, "mm"))) +
    ggraph::geom_node_label(aes(label=.data$name))
}
