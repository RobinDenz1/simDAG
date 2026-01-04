
## pearl do operator for DAG objects
#' @export
do <- function(dag, names, values) {

  check_inputs_do(dag=dag, names=names, values=values)

  # remove old node definitions
  names_roots <- lapply(dag$root_nodes, function(x){x$name})
  names_children <- lapply(dag$child_nodes, function(x){x$name})
  names_tx_nodes <- lapply(dag$tx_nodes, function(x){x$name})

  dag$root_nodes[names_roots %in% names] <- NULL
  dag$child_nodes[names_children %in% names] <- NULL
  dag$tx_nodes[names_tx_nodes %in% names] <- NULL

  # replace with constant value definition
  for (i in seq_len(length(names))) {
    if (inherits(values[[i]], "DAG.node")) {
      node_i <- values[[i]]
      node_i$name <- names[i]
      dag <- dag + node_i
    } else {
      dag <- dag + node(name=names[i], type="rconstant", constant=values[[i]])
    }
  }

  return(dag)
}
