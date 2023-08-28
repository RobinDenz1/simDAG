\name{plot.DAG}
\alias{plot.DAG}

\title{
Plot a \code{DAG} object
}
\description{
Using the node information contained in the \code{DAG} object this function plots the corresponding DAG in a quick and convenient way. Some options to customize the plot are available, but it may be advisable to use other packages made explicitly to visualize DAGs instead if those do not meet the users needs.
}
\usage{
\method{plot}{DAG}(x, layout="nicely", node_size=0.2,
     node_names=NULL, node_color="black",
     node_fill="red", node_linewidth=0.5,
     node_linetype="solid", node_alpha=1,
     node_text_color="black", node_text_alpha=1,
     node_text_size=8, node_text_family="sans",
     node_text_fontface="bold", arrow_color="black",
     arrow_linetype="solid", arrow_linewidth=1,
     arrow_alpha=1, arrow_head_size=0.3,
     arrow_head_unit="cm", arrow_type="closed",
     arrow_node_dist=0.03, gg_theme=ggplot2::theme_void(),
     include_td_nodes=TRUE, mark_td_nodes=TRUE,
     ...)
}
\arguments{
  \item{x}{
A \code{DAG} object created using the \code{\link{empty_dag}} function with nodes added to it using the \code{+} syntax. See \code{\link{empty_dag}} or \code{\link{node}} for more details.
  }
  \item{layout}{
A single character string specifying the layout of the plot. This internally calls the \code{layout_} function of the \pkg{igraph} package, which offers a great variety of ways to layout the nodes of a graph. Defaults to \code{"nicely"}. Some other options are: \code{"as_star"}, \code{"as_tree"}, \code{"in_circle"}, \code{"on_spere"}, \code{"randomly"} and many more. For more details see \code{?layout_}.
  }
  \item{node_size}{
Either a single positive number or a numeric vector with one entry per node in the DAG, specifying the radius of the circles used to draw the nodes. If a single number is supplied, all nodes will be the same size (default).
  }
  \item{node_names}{
A character vector with one entry for each node in the DAG specifying names that should be used for in the nodes or \code{NULL} (default). If \code{NULL}, the node names that were set during the creation of the DAG object will be used as names.
  }
  \item{node_color}{
A single character string specifying the color of the outline of the node circles.
  }
  \item{node_fill}{
A single character string specifying the color with which the nodes are filled. Ignored if time-varying nodes are present and both \code{include_td_nodes} and \code{mark_td_nodes} are set to \code{TRUE}.
  }
  \item{node_linewidth}{
A single number specifying the width of the outline of the node circles.
  }
  \item{node_linetype}{
A single character string specifying the linetype of the outline of the node circles.
  }
  \item{node_alpha}{
A single number between 0 and 1 specifying the transparency level of the nodes.
  }
  \item{node_text_color}{
A single character string specifying the color of the text inside the node circles.
  }
  \item{node_text_alpha}{
A single number between 0 and 1 specifying the transparency level of the text inside the node circles.
  }
  \item{node_text_size}{
A single number specifying the size of the text inside of the node circles.
  }
  \item{node_text_family}{
A single character string specifying the family of the text inside the node circles.
  }
  \item{node_text_fontface}{
A single character string specifying the fontface of the text inside the node circles.
  }
  \item{arrow_color}{
A single character string specifying the color of the arrows between the nodes.
  }
  \item{arrow_linetype}{
A single character string specifying the linetype of the arrows.
  }
  \item{arrow_linewidth}{
A single number specifying the width of the arrows.
  }
  \item{arrow_alpha}{
A single number between 0 and 1 specifying the transparency level of the arrows.
  }
  \item{arrow_head_size}{
A single number specifying the size of the arrow heads. The unit for this size parameter can be changed using the \code{arrow_head_unit} argument.
  }
  \item{arrow_head_unit}{
A single character string specifying the unit of the \code{arrow_head_size} argument.
  }
  \item{arrow_type}{
Either \code{"open"} or \code{"closed"}, which controls the type of head the arrows should have. See \code{?arrow}.
  }
  \item{arrow_node_dist}{
A single positive number specifying the distance between nodes and the arrows. By setting this to values greater than 0 the arrows will not touch the node circles, leaving a bit of space instead.
  }
  \item{gg_theme}{
A \code{ggplot2} theme. By default this is set to \code{theme_void}, to get rid off everything but the plotted nodes (e.g. everything about the axis and the background). Might be useful to change this to something else when searching for good parameters of the number arguments of this function.
  }
  \item{include_td_nodes}{
Whether to include time-varying nodes added to the \code{dag} using the \code{\link{node_td}} function or not. If one node is both specified as a time-fixed and time-varying node, it's parents in both calls will be pooled and it will be considered a time-varying node if this argument is \code{TRUE}. It will, however, also show up if it's argument is \code{FALSE}. In this case however, only the parents of that node in the standard \code{\link{node}} call will be considered.
  }
  \item{mark_td_nodes}{
Whether to distinguish time-varying and time-fixed nodes by \code{fill} color. If \code{TRUE}, the color will be set automatically using the standard \code{ggplot2} palette, ignoring the color specified in \code{node_fill}. Ignored if \code{include_td_nodes=FALSE} or if there are no time-varying variables.
  }
  \item{...}{
Currently not used.
  }
}
\details{
This function uses the \pkg{igraph} package to find a suitable layout for the plot and then uses the \pkg{ggplot2} package in conjunction with the \code{geom_circle} function of the \pkg{ggforce} package to plot the directed acyclic graph defined by a \code{DAG} object. Since it returns a \code{ggplot} object, the user may use any standard \code{ggplot2} syntax to augment the plot or to save it using the \code{ggsave} function.

Note that there are multiple great packages specifically designed to plot directed acyclic graphs, such as the \pkg{igraph} package. This function is not meant to be a competitor to those packages. The functionality offered here is rather limited. It is designed to produce decent plots for small DAGs which are easy to create. If this function is not enough to create an adequate plot, users can use the \code{\link{dag2matrix}} function to obtain an adjacency matrix from the \code{DAG} object and directly use this matrix and the \pkg{igraph} package (or similar ones) to get much better plots.

If the \code{DAG} supplied to this function contains time-varying variables, the resulting plot may contain cycles or even bi-directional arrows, depending on the \code{DAG}. The reason for that is, that the time-dimension is not shown in the plot. Note also that even though, technically, every time-varying node has itself as a parent, no arrows showing this dependence will be added to the plot.
}
\author{
Robin Denz
}
\value{
Returns a standard \code{ggplot2} object.
}
\seealso{
\code{\link{empty_dag}}, \code{\link{node}}, \code{\link{node_td}}
}
\examples{
library(simDAG)
library(ggplot2)
library(igraph)
library(ggforce)

# 2 root nodes, 1 child node
dag <- empty_dag() +
  node("age", type="rnorm", mean=50, sd=4) +
  node("sex", type="rbernoulli", p=0.5) +
  node("smoking", type="binomial", parents=c("sex", "age"), betas=c(1.1, 0.4),
       intercept=-2)

plot(dag)

# get plot using igraph
adj_mat <- dag2matrix(dag)

g1 <- igraph::graph.adjacency(adjmatrix=adj_mat, mode="directed")

plot(g1)

# plot with a time-varying node
dag <- dag +
  node_td("lottery", type="time_to_event", parents=c("age", "smoking"))

plot(dag)
}
