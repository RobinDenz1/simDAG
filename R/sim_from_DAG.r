
## generate data from a DAG with defined nodes
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @export
sim_from_dag <- function(dag, n_sim, sort_dag=FALSE,
                         return_networks=FALSE, check_inputs=TRUE) {

  requireNamespace("data.table", quietly=TRUE)

  if (check_inputs) {
    check_inputs_sim_from_dag(dag=dag, n_sim=n_sim, sort_dag=sort_dag)
  }

  # sample from root nodes
  data <- vector(mode="list", length=length(dag$root_nodes))
  for (i in seq_len(length(dag$root_nodes))) {
    # add n to existing arguments
    args <- dag$root_nodes[[i]]$params
    args$n <- n_sim

    # call data generation function
    out <- tryCatch({
      data.table(do.call(dag$root_nodes[[i]]$type_fun, args))},
      error=function(e){
        stop("An error occured when processing root node '",
             dag$root_nodes[[i]]$name, "'. The message was:\n", e, call.=FALSE)
      }
    )

    if (ncol(out)==1) {
      colnames(out) <- dag$root_nodes[[i]]$name
    }

    data[[i]] <- out
  }
  data <- as.data.table(unlist(data, recursive=FALSE), check.names=TRUE)

  if (length(dag$child_nodes)==0 & return_networks) {
    return(list(data=data, networks=dag$networks))
  } else if (length(dag$child_nodes)==0) {
    return(data)
  }

  # put together networks and child nodes
  child_nodes <- c(dag$child_nodes, dag$networks)

  # if not already ordered properly, use topological
  # sorting to get the right data generation sequence
  if (sort_dag) {
    requireNamespace("Rfast", quietly=TRUE)
    adjacency_mat <- dag2matrix(dag=dag, include_root_nodes=FALSE,
                                include_networks=TRUE)
    index_children <- Rfast::topological_sort(adjacency_mat)
  } else {
    index_children <- get_order_from_index(child_nodes)
  }

  # go through DAG step by step
  for (i in index_children) {

    # initialize networks
    if (inherits(child_nodes[[i]], "DAG.network")) {
      network_i <- update_network(network=child_nodes[[i]],
                                  n_sim=n_sim,
                                  sim_time=0,
                                  data=data)
      dag$networks[[child_nodes[[i]]$name]] <- network_i
      next
    }

    # get the names of the nodes generating function
    fun_pos_args <- names(formals(child_nodes[[i]]$type_fun))

    # get relevant arguments
    args <- child_nodes[[i]]
    args$data <- data
    args$type_str <- NULL
    args$type_fun <- NULL
    args$time_varying <- NULL
    args$..index.. <- NULL

    if (!"name" %in% fun_pos_args) {
      args$name <- NULL
    }

    if ("dag" %in% fun_pos_args) {
      args$dag <- dag
    }

    # if a special formula is supplied, change arguments accordingly
    form <- child_nodes[[i]]$formula

    if (!is.null(form) && !is_formula(form) &&
        child_nodes[[i]]$type_str != "identity") {
      args <- args_from_formula(args=args, formula=form,
                                node_type=child_nodes[[i]]$type_str)
      args$data <- tryCatch({
        data_for_formula(data=data, args=args, networks=dag$networks)},
        error=function(e){
          stop("An error occured when interpreting the formula of node '",
               child_nodes[[i]]$name, "'. The message was:\n", e,
               call.=FALSE)
        }
      )
    }

    # remove intercept if not needed
    if (!"intercept" %in% fun_pos_args) {
      args$intercept <- NULL
    }
    # remove temporary mixed model stuff if there
    if (!is.null(args$mixed_terms)) {
      args$mixed_terms <- NULL
    }

    # call needed node function, add node name to possible errors
    node_out <- tryCatch({
      do.call(child_nodes[[i]]$type_fun, args)},
      error=function(e){
        stop("An error occured when processing node '",
             child_nodes[[i]]$name, "'. The message was:\n", e,
             call.=FALSE)
      }
    )
    data <- add_node_to_data(data=data, new=node_out,
                             name=child_nodes[[i]]$name)
  }

  # also return networks if specified
  if (return_networks) {
    out <- list(data=data,
                networks=dag$networks)
  } else {
    out <- data
  }
  return(out)
}

## given a node list, extracts the ..index.. parameter and returns
## a numeric vector containing the order in which the nodes / networks were
## added
get_order_from_index <- function(node_list) {
  index <- vapply(node_list,
                  FUN=function(x){x$..index..},
                  FUN.VALUE=numeric(1),
                  USE.NAMES=FALSE)
  names(index) <- seq_len(length(index))
  index <- as.numeric(names(sort(index)))
  return(index)
}
