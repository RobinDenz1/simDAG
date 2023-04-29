
# check whether a node name is appropriate
is_node_name <- function(name) {
  length(name)==1 && is.character(name)
}

# check whether a node type is appropriate
is_node_type <- function(type, call_from) {
  length(type)==1 && is.character(type) &&
    exists(paste0("node_", type), mode="function") &&
    !(call_from=="sim_from_dag" & type=="time_to_event")
}

# check whether a node dist (root nodes) is appropriate
is_node_dist <- function(dist) {
  length(dist)==1 && is.character(dist) && exists(dist, mode="function")
}

# check whether parents are defined appropriately
is_node_parents <- function(parents) {
  length(parents) > 0 && is.character(parents)
}

## checking the inputs of the sim_from_dag function
check_inputs_sim_from_dag <- function(n_sim, root_nodes, child_nodes,
                                      sort_dag) {
  # rudimentary type checks
  if (!(length(n_sim)==1 && is.numeric(n_sim) && n_sim > 0)) {
    stop("'n_sim' must be a single integer > 0.")
  } else if (!(length(sort_dag)==1 && is.logical(sort_dag))) {
    stop("'sort_dag' must be either TRUE or FALSE.")
  } else if (!is.list(root_nodes) & !is.data.frame(root_nodes)) {
    stop("'root_nodes' must be either a data.frame or a list.")
  } else if (!is.list(child_nodes)) {
    stop("'child_nodes' must be a list.")
  }

  ## check contents of root nodes
  if (is.data.frame(root_nodes)) {
    if (ncol(root_nodes)==0) {
      stop("If a data.frame is supplied to 'root_nodes' it needs to include",
           " at least one variable.")
    } else if (nrow(root_nodes)==0) {
      stop("If a data.frame is supplied to 'root_nodes' it needs to include",
           " at least one row.")
    }
  } else {
    # correct node names
    corr_names <- vapply(root_nodes,
                         FUN=function(x){is_node_name(x$name)},
                         FUN.VALUE=logical(1))
    if (!all(corr_names)) {
      stop("The 'name' parameter of all root_nodes needs to be a single",
           " character string. Problems in root_nodes: ",
           seq_len(length(root_nodes))[!corr_names])
    }
    # correct node dist
    corr_dist <- vapply(root_nodes,
                        FUN=function(x){is_node_dist(x$dist)},
                        FUN.VALUE=logical(1))
    if (!all(corr_dist)) {
      stop("The 'dist' parameter of all root_nodes needs to be a single",
           " character string naming a defined function.",
           " Problems in root_nodes: ",
           seq_len(length(root_nodes))[!corr_dist])
    }
    # correct node params
    corr_params <- vapply(root_nodes,
                          FUN=function(x){is.list(x$params)},
                          FUN.VALUE=logical(1))
    if (!all(corr_params)) {
      stop("The 'params' parameter of all root_nodes needs to be a list.",
           " Problems in root_nodes: ",
           seq_len(length(root_nodes))[!corr_params])
    }
  }

  ## check contents of child nodes
  # correct node names
  corr_names <- vapply(child_nodes,
                       FUN=function(x){is_node_name(x$name)},
                       FUN.VALUE=logical(1))
  if (!all(corr_names)) {
    stop("The 'name' parameter of all child_nodes needs to be a single",
         " character string. Problems in child_nodes: ",
         seq_len(length(child_nodes))[!corr_names])
  }
  # correct node parents
  corr_parents <- vapply(child_nodes,
                         FUN=function(x){is_node_parents(x$parents)},
                         FUN.VALUE=logical(1))
  if (!all(corr_parents)) {
    stop("The 'parents' parameter of all child_nodes needs to be a vector",
         " of character strings. Problems in child_nodes: ",
         seq_len(length(child_nodes))[!corr_parents])
  }
  # correct node type
  corr_type <- vapply(child_nodes,
                      FUN=function(x){is_node_type(x$type,
                                                   call_from="sim_from_dag")},
                      FUN.VALUE=logical(1))
  if (!all(corr_type)) {
    stop("The 'type' parameter of all child_nodes needs to be a single",
         " character string pointing to a function. Problems in child_nodes: ",
         seq_len(length(child_nodes))[!corr_type])
  }
  # TODO: other checks:
  #   - multinomial / cox should (at the moment) only be leaf nodes
  #   - check if data generation possible or if there are missing node definitions

}

## check the inputs of the node_conditional_probs function
# TODO: add checks for default_val and default_probs
check_inputs_node_conditional_probs <- function(data, parents, probs,
                                                default_val, default_probs) {

  dep_probs_length <- unlist(lapply(probs, length))
  if (min(dep_probs_length, na.rm=TRUE) != max(dep_probs_length, na.rm=TRUE)) {
    stop("There must be an equal number of probabilities for each",
         " possible value in 'probs'.")
  } else if (is.null(names(probs))) {
    stop("All elements in 'probs' must be named using levels of 'parents'.")
  } else if (length(parents) == 1 &&
             !all(names(probs) %in% unique(data[[parents]]))) {
    stop("All elements in 'probs' must correspond to levels in ", parents,
         ". The following elements are not: ",
         names(probs)[!names(probs) %in% unique(data[[parents]])])
  } else if (length(parents) > 1 &&
             !all(names(probs) %in%
                  unique(interaction(data[, parents, with=FALSE])))) {
    stop("All elements in 'probs' must correspond to levels defined by the",
         "combined strata of all 'parents'. The following elements are not: ",
         names(probs)[!names(probs) %in%
                      unique(interaction(data[, parents, with=FALSE]))])
  }
}

## check whether the inputs to the long2start_stop function are valid
check_inputs_long2start_stop <- function(data, id, time, varying) {

  if (nrow(data)==0) {
    stop("'data' needs to have at least 1 row.")
  } else if (!(is.character(id) && length(id)==1)) {
    stop("'id' has to be a single character string, specifying the unique",
         " person identifier in 'data'.")
  } else if (!id %in% colnames(data)) {
    stop(id, " is not a valid column in 'data'.")
  } else if (!(is.character(data[[id]]) | is.integer(data[[id]]))) {
    stop("The column specified by 'id' must be a character, factor or",
         " integer variable.")
  } else if (!(is.character(time) && length(time)==1)) {
    stop("'time' has to be a single character string, specifying the",
         " variable containing points in time in 'data'.")
  } else if (!time %in% colnames(data)) {
    stop(time, " is not a valid column in 'data'.")
  } else if (!all(data[[time]] %% 1==0)) {
    stop("The variable specified by 'time' may only contain integers.")
  } else if (!((is.null(varying) | is.character(varying)))) {
    stop("'varying' must be a character vector specifying variables that",
         " change over time in 'data'.")
  } else if (!all(varying %in% colnames(data))) {
    stop("The following names in 'varying' are not contained in 'data': ",
         paste0(varying[!varying %in% colnames(data)], collapse=", "))
  }

  if (length(varying)==0) {
    warning("No time-varying variables specified.")
  }
}

## check user inputs to sim2start_stop, sim2long, sim2wide functions
check_inputs_sim2data <- function(sim, use_saved_states) {

  # errors
  if (!inherits(sim, "simDT")) {
    stop("'sim' needs to be a simDT object created using the",
         " sim_discrete_time() function.")
  } else if (!(is.logical(use_saved_states) & length(use_saved_states)==1)) {
    stop("'use_saved_states' must be either TRUE or FALSE.")
  } else if (use_saved_states & sim$save_states=="last") {
    stop("use_saved_states=TRUE cannot be used if save_states='last'",
         " was used in the original sim_discrete_time() function call.",
         " Set to FALSE or rerun simulation.")
  }

  # extract node_time_to_event objects
  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type})
  tte_names <- names(sim$tte_past_events)

  # raise warning due to missing information if needed
  if (length(tte_names) < length(sim$tx_nodes) & sim$save_states!="all") {
    warn_cols <- unlist(lapply(sim$tx_nodes[node_types!="time_to_event"],
                               FUN=function(x){x$name}))
    warning("Resulting data may be inaccurate for the following columns: '",
            paste0(warn_cols, collapse="', '"), "'\nbecause save_states!='all'",
            " in sim_discrete_time function call. See details.")
  }

  if (sim$save_states=="at_t") {
    warning("The output of this function may be inaccurate if",
            " save_states='at_t' was used in the original sim_discrete_time()",
            " function call. See documentation.")
  }
}
