\name{plot_dag}
\alias{plot_dag}

\title{
Plot a DAG as defined by \code{root_nodes} and \code{child_nodes} lists
}
\description{
Using the \code{root_nodes} and \code{child_nodes} lists, this function plots the corresponding DAG in a quick and dirty way.
}
\usage{
plot_dag(root_nodes, child_nodes)
}
\arguments{
  \item{root_nodes}{
A list of root nodes, as used in the \code{\link{sim_from_dag}} function.
  }
  \item{child_nodes}{
A list of child nodes, as used in the \code{\link{sim_from_dag}} function.
  }
}
\details{
This function uses the \pkg{ggraph} package to plot the directed acyclic graph defined by the node list objects needed to use the \code{\link{sim_from_dag}} function. This function is meant to be used only for development purposes (e.g. checking if the node lists are correctly specified). It is very simple and does not support any arguments to make the plot better.

If a publication ready graph should be produced, this function should probably not be used. Instead, users can use the \code{\link{nodes2adjacency_mat}} function to obtain an adjacency matrix from the node lists and directly use this matrix and the \pkg{ggraph} package to get much better plots. A small example of this is shown below.
}
\author{
Robin Denz
}
\examples{
\dontrun{
library(ggplot2)
library(igraph)
library(ggraph)

# 2 root nodes, 1 child node
root_nodes <- list(list(dist="rnorm",
                        params=list(mean=50, sd=4),
                        name="age"),
                   list(dist="rbernoulli",
                        params=list(p=0.5),
                        name="sex"))
child_nodes <- list(list(parents=c("sex", "age"),
                         type="binomial",
                         name="smoking",
                         betas=c(1.1, 0.4),
                         intercept=-2))

# use the quick and dirty function
plot_dag(root_nodes, child_nodes)

# do it manually
mat <- nodes2adjacency_mat(root_nodes, child_nodes)

g1 <- igraph::graph.adjacency(adjmatrix=adj_mat, mode="directed")

# this code produces equivalent results as the code above
ggraph::ggraph(adj_graph, layout="graphopt") +
    ggraph::geom_edge_link(ggplot2::aes(
      start_cap=ggraph::label_rect(.data$node1.name),
      end_cap=ggraph::label_rect(.data$node2.name)),
      arrow=ggplot2::arrow(length=unit(4, "mm"))) +
    ggraph::geom_node_label(aes(label=.data$name))
}
}
