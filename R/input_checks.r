
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
check_inputs_node_conditional_probs <- function(data, parents, probs) {

  dep_probs_length <- unlist(lapply(probs, length))
  if (min(dep_probs_length, na.rm=TRUE) != max(dep_probs_length, na.rm=TRUE)) {
    stop("There must be an equal number of probabilities for each",
         " possible value in 'probs'.")
  } else if (is.null(names(probs))) {
    stop("All elements in 'probs' must be named using levels of 'parents'.")
  } else if (length(parents) == 1 &&
             !all(unique(data[[parents]]) %in% names(probs))) {
    stop("All levels of variable ", parents, " need to be included in argument",
         " 'probs'.")
  } else if (length(parents) > 1 &&
             !all(unique(interaction(data[, parents, with=FALSE]))
                  %in% names(probs))) {
    stop("All levels of the interaction between all 'parents' need to be",
         " included in argument 'probs'.")
  }
}
