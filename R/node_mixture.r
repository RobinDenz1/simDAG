
## node that allows different node() specifications given some arbitrary
## conditions based on previously generated data
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table :=
#' @export
node_mixture <- function(data, parents, name, distr, default=NA) {

  if (!is.data.table(data)) {
    data <- as.data.table(data)
  }

  out <- rep(default, nrow(data))
  for (i in seq(2, length(distr), 2)) {

    # filter relevant rows for this level
    cond <- with(data, eval(str2lang(distr[[i-1]])))
    data_i <- copy(data[cond])

    # setup dag object for this specific part of data
    dag <- empty_dag() +
      node(colnames(data_i)[1], type=pass_input, input=data_i) +
      distr[[i]]

    # simulate the data
    out[cond] <- sim_from_dag(dag=dag, n_sim=nrow(data_i))[[distr[[i]]$name]]
    data[[name]] <- out
  }
  return(data[[name]])
}

## trick root node definition into just passing data as input
pass_input <- function(n, input) {
  return(input)
}

## check inputs for node_mixture() function
check_inputs_node_mixture <- function(parents, args) {

  distr <- args$distr

  if (length(distr)==0) {
    stop("'distr' must contain at least two entries.")
  } else if ((length(distr) %% 2) != 0) {
    stop("'distr' must contain an even number of entries, with",
         " the condition first and the node second.")
  } else if (!is.null(args$default) && length(args$default)!=1) {
    stop("'default' should be a single value of some kind.")
  }

  for (i in seq(1, length(distr), 2)) {
    if (!(length(distr[[i]])==1 && is.character(distr[[i]]))) {
      stop("'distr' must contain single character strings containing the",
           " condition for the corresponding node() definition first,",
           " then the node() objects.")
    }
    if (!inherits(distr[[i+1]], "DAG.node")) {
      stop("'distr' must contain DAG.node objects created using the",
           " node() function for the corresponding conditions only after",
           " the conditions.")
    } else if (distr[[i+1]]$time_varying==TRUE) {
      stop("Time-dependent nodes defined with the node_td() function",
           " are currently not supported in 'distr'.")
    }
  }
}
