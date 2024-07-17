
## generate data from a DAG with defined nodes
#' @importFrom data.table data.table
#' @importFrom data.table setDT
#' @export
sim_from_dag <- function(dag, n_sim, sort_dag=TRUE, check_inputs=TRUE) {

  requireNamespace("data.table")

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
      data.table(do.call(get(dag$root_nodes[[i]]$type), args))},
      error=function(e){
        stop("An error occured when processing root node '",
             dag$root_nodes[[i]]$name, "'. The message was: ", e)
      }
    )
    colnames(out) <- dag$root_nodes[[i]]$name

    data[[i]] <- out
  }
  data <- as.data.table(unlist(data, recursive=FALSE), check.names=TRUE)

  if (length(dag$child_nodes)==0) {
    return(data)
  }

  # if not already ordered properly, use topological
  # sorting to get the right data generation sequence
  if (sort_dag) {
    requireNamespace("Rfast")
    adjacency_mat <- dag2matrix(dag=dag, include_root_nodes=FALSE)
    index_children <- Rfast::topological_sort(adjacency_mat)
  } else {
    index_children <- seq_len(length(dag$child_nodes))
  }

  # go through DAG step by step
  for (i in index_children) {

    # get relevant arguments
    args <- dag$child_nodes[[i]]
    args$data <- data
    args$type <- NULL
    args$time_varying <- NULL

    if (dag$child_nodes[[i]]$type!="cox") {
      args$name <- NULL
    }

    # if a special formula is supplied, change arguments accordingly
    form <- dag$child_nodes[[i]]$formula

    if (!is.null(form) && !is_formula(form)) {
      args <- args_from_formula(args=args, formula=form,
                                node_type=dag$child_nodes[[i]]$type)
      args$data <- tryCatch({
        data_for_formula(data=data, args=args)},
        error=function(e){
          stop("An error occured when interpreting the formula of node '",
               dag$child_nodes[[i]]$name, "'. The message was:\n", e,
               call.=FALSE)
        }
      )
    }

    # call needed node function, add node name to possible errors
    node_out <- tryCatch({
      do.call(get(paste0("node_", dag$child_nodes[[i]]$type)), args)},
      error=function(e){
        stop("An error occured when processing node '",
             dag$child_nodes[[i]]$name, "'. The message was: ", e)
      }
    )
    data <- add_node_to_data(data=data, new=node_out,
                             name=dag$child_nodes[[i]]$name)
  }
  return(data)
}
