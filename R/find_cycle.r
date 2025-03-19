
## recursive search to find a cycle of length > 1 from a node to
## itself in a directed unweighted graph
#' @importFrom igraph neighborhood
find_cycle <- function(graph, start, current=start, path=c()) {

  # update path
  path <- c(path, current)

  # get all descendants
  neighbors <- names(
    neighborhood(graph=graph, nodes=current, mode="out", mindist=1)[[1]]
  )

  # if start node is in there and the path is long enough, terminate
  if (length(path) > 1 && start %in% neighbors) {
    path <- c(path, start)
    return(path)
  }

  # otherwise check next level
  for (i in seq_len(length(neighbors))) {
     out <- find_cycle(graph=graph, start=start, path=path,
                       current=neighbors[i])
     if (!is.null(out)) {
       return(out)
     }
  }
  return(NULL)
}
