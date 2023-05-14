
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

# check whether a node type (root nodes) is appropriate
is_node_dist <- function(type) {
  length(type)==1 && is.character(type) && exists(type, mode="function")
}

# check whether parents are defined appropriately
is_node_parents <- function(parents) {
  length(parents) > 0 && is.character(parents)
}

## checking the inputs of the sim_from_dag function
check_inputs_sim_from_dag <- function(dag, n_sim, sort_dag) {

  # rudimentary type checks
  if (!(length(n_sim)==1 && is.numeric(n_sim) && n_sim > 0)) {
    stop("'n_sim' must be a single integer > 0.")
  } else if (!(length(sort_dag)==1 && is.logical(sort_dag))) {
    stop("'sort_dag' must be either TRUE or FALSE.")
  } else if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object creates using empty_dag() and",
         " node() function calls. See documentation.")
  }

  ## check contents of root nodes
  # correct node names
  corr_names <- vapply(dag$root_nodes,
                       FUN=function(x){is_node_name(x$name)},
                       FUN.VALUE=logical(1))
  if (!all(corr_names)) {
    stop("The 'name' parameter of all root_nodes needs to be a single",
         " character string. Problems in root_nodes: ",
         seq_len(length(dag$root_nodes))[!corr_names])
  }
  # correct node type
  corr_type <- vapply(dag$root_nodes,
                      FUN=function(x){is_node_dist(x$type)},
                      FUN.VALUE=logical(1))
  if (!all(corr_type)) {
    stop("The 'type' parameter of all root_nodes needs to be a single",
         " character string naming a defined function.",
         " Problems in root_nodes: ",
         seq_len(length(dag$root_nodes))[!corr_type])
  }

  ## check contents of child nodes
  # correct node names
  corr_names <- vapply(dag$child_nodes,
                       FUN=function(x){is_node_name(x$name)},
                       FUN.VALUE=logical(1))
  if (!all(corr_names)) {
    stop("The 'name' parameter of all child_nodes needs to be a single",
         " character string. Problems in child_nodes: ",
         seq_len(length(dag$child_nodes))[!corr_names])
  }
  # correct node parents
  corr_parents <- vapply(dag$child_nodes,
                         FUN=function(x){is_node_parents(x$parents)},
                         FUN.VALUE=logical(1))
  if (!all(corr_parents)) {
    stop("The 'parents' parameter of all child_nodes needs to be a vector",
         " of character strings. Problems in child_nodes: ",
         seq_len(length(dag$child_nodes))[!corr_parents])
  }
  # correct node type
  corr_type <- vapply(dag$child_nodes,
                      FUN=function(x){is_node_type(x$type,
                                                   call_from="sim_from_dag")},
                      FUN.VALUE=logical(1))
  if (!all(corr_type)) {
    stop("The 'type' parameter of all child_nodes needs to be a single",
         " character string pointing to a function. Problems in child_nodes: ",
         seq_len(length(dag$child_nodes))[!corr_type])
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

## check user inputs to sim2data function
check_inputs_sim2data <- function(sim, use_saved_states, to) {

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
  } else if (!(is.character(to) & length(to)==1 &
               to %in% c("start_stop", "long", "wide"))) {
    stop("'to' must be one of: 'start_stop', 'long', 'wide'.")
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

## check user input for plot.simDT function
check_inputs_plot.simDT <- function(right_boxes, box_hdist, box_vdist,
                                    box_l_width, box_l_height, box_r_width,
                                    box_r_height, box_1_text_left,
                                    box_1_text_right, box_2_text,
                                    box_l_node_labels, box_r_node_labels,
                                    box_last_text, tx_names) {

  # logical values
  if (!(is.logical(right_boxes) && length(right_boxes)==1)) {
    stop("'right_boxes' must be either TRUE or FALSE.")
  }

  # numeric values
  number_checks <- list(box_hdist, box_vdist, box_l_width, box_l_height,
                        box_r_width, box_r_height)
  check_names <- c("box_hdist", "box_vdist", "box_l_width", "box_l_height",
                   "box_r_width", "box_r_height")
  for (i in seq_len(length(number_checks))) {
    arg <- number_checks[[i]]
    if (!(is.numeric(arg) && length(arg==1))) {
      stop("'", check_names[[i]], "' must be a single number.")
    }
  }

  # single character values
  str_checks <- list(box_1_text_left, box_2_text, box_last_text)
  check_names <- c("box_1_text_left", "box_2_text", "box_last_text")
  for (i in seq_len(length(str_checks))) {
    arg <- str_checks[[i]]
    if (!(is.character(arg) && length(arg==1))) {
      stop("'", check_names[[i]], "' must be a single character string.")
    }
  }

  # is separate here because it allows NULL
  if (!(is.character(box_1_text_right) && length(box_1_text_right)==1) &&
      !is.null(box_1_text_right)) {
    stop("'box_1_text_right' must be a single character string or NULL.")
  }

  # multiple character values
  if (!(is.character(box_l_node_labels) &&
        length(box_l_node_labels)==length(tx_names)) &&
      !is.null(box_l_node_labels)) {
    stop("'box_l_node_labels' must be a character vector with one entry for",
         " each tx_node in the original sim_discrete_time() call.")
  } else if (!(is.character(box_r_node_labels) &&
               length(box_r_node_labels)==length(tx_names)) &&
             !is.null(box_r_node_labels)) {
    stop("'box_r_node_labels' must be a character vector with one entry for",
         " each tx_node in the original sim_discrete_time() call.")
  }
}

## check inputs for do() function
check_inputs_do <- function(dag, names, values) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() and node()",
         " functions. See ?node for a description on how to do this.")
  } else if (!(is.character(names) & length(names) > 0)) {
    stop("'names' must be a character vector of length > 0.")
  } else if (length(names) != length(values)) {
    stop("'names' must have the same length as 'values'.")
  }
}
