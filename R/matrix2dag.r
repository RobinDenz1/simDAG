
## obtain a partially specified DAG object from an adjacency matrix and
## a list of node types
#' @export
matrix2dag <- function(mat, type) {

  dag <- empty_dag()

  node_names <- colnames(mat)
  for (i in seq_len(length(node_names))) {

    parents <- names(mat[, node_names[i]][mat[, node_names[i]]==1])

    if (length(parents)==0) {
      parents <- NULL
    }

    dag <- dag + node(name=node_names[i], type=type[[node_names[i]]],
                      parents=parents)
  }

  return(dag)
}
