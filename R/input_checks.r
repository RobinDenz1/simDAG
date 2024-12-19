
## check whether something is a formula or not
is_formula <- function(x) {
  inherits(x, "formula")
}

# check whether a node name is appropriate
is_node_name <- function(name) {
  length(name) >= 1 && is.character(name)
}

# check whether a node type is appropriate
is_node_type <- function(type) {
  (length(type)==1 && is.character(type) &&
    exists(paste0("node_", type), mode="function")) | is.function(type)
}

# check whether a node type (root nodes) is appropriate
is_node_dist <- function(type) {
  (length(type)==1 && is.character(type) &&
     exists(type, mode="function", envir=globalenv())) | is.function(type)
}

# check whether parents are defined appropriately
is_node_parents <- function(parents) {
  length(parents) > 0 && is.character(parents)
}

# check whether object is a vector of beta coefficients
is_betas <- function(betas) {
  length(betas) > 0 && is.numeric(betas)
}

# check whether an object may be used as an intercept
is_intercept <- function(intercept) {
  length(intercept) == 1 && is.numeric(intercept)
}

## input checks for root nodes
check_inputs_root_node <- function(name, type) {

  if (!is_node_name(name)) {
    stop("The 'name' attribute must be character vector of length >= 1.")
  } else if (!is_node_dist(type)) {
    stop("The 'type' parameter of a root node must be a single",
         " character string naming a defined function.")
  } else if (any(name %in% c("identity", "node_identity"))) {
    stop("Nodes of type 'identity' cannot be used as root nodes, e.g. at",
         " least one variable name has to be mentioned in 'formula'. Use",
         " type='rconstant' instead to specify a constant value.")
  }
}

## input checks for child nodes
check_inputs_child_node <- function(name, type, parents, args, time_varying,
                                    formula) {

  if (!is_node_name(name)) {
    stop("The 'name' attribute must be character vector of length >= 1.")
  } else if (!is_node_type(type)) {
    stop("The 'type' parameter of a child node must be a single",
         " character string pointing to a function starting with 'node_'.")
  } else if (!is_node_parents(parents) & !time_varying) {
    stop("The 'parents' argument of a child node must be a character",
         " vector of length > 0.")
  } else if (!(is_formula(formula) | is.null(formula) |
               is.character(formula))) {
    stop("'formula' must be a valid formula object, a special formula or NULL.")
  }

  # type specific checks
  if (is.function(type)) {
    type <- extract_function_name(type)
    type <- correct_type_str(type)
  }

  type_check_fun_name <- paste0("check_inputs_node_", type)

  if (exists(type_check_fun_name, mode="function") &
      !type %in% c("conditional_distr", "conditional_prob", "time_to_event",
                   "competing_events")) {
    args$formula <- formula
    type_check_fun <- get(type_check_fun_name)
    type_check_fun(parents=parents, args=args)
  }
}

## general checks for all regression based nodes
check_inputs_node_regression <- function(parents, args, type) {

  if (is.null(args$betas) & !is.character(args$formula)) {
    stop("'betas' must be defined when using type='", type, "'.")
  } else if (is.null(args$intercept) & !is.character(args$formula)) {
    stop("'intercept' must be defined when using type='", type, "'.")
  }

  if (!is_betas(args$betas) & !is.character(args$formula)) {
    stop("'betas' must be a numeric vector when using type='", type, "'.")
  } else if (!is_intercept(args$intercept) & !is.character(args$formula)) {
    stop("'intercept' must be a single number when using type='", type, "'.")
  } else if ((length(parents) != length(args$betas)) & is.null(args$formula)) {
    stop("'betas' must have the same length as 'parents' when using",
         " type='", type, "'.")
  }
}

## input checks for gaussian nodes
check_inputs_node_gaussian <- function(parents, args) {

  check_inputs_node_regression(parents=parents, args=args, type="gaussian")

  if (is.null(args$error)) {
    stop("'error' must be defined when using type='gaussian'.")
  }
}

## input checks for binomial nodes
check_inputs_node_binomial <- function(parents, args) {
  check_inputs_node_regression(parents=parents, args=args, type="binomial")
}

## input checks for poisson nodes
check_inputs_node_poisson <- function(parents, args) {
  check_inputs_node_regression(parents=parents, args=args, type="poisson")
}

## input checks for negative_binomial nodes
check_inputs_node_negative_binomial <- function(parents, args) {
  check_inputs_node_regression(parents=parents, args=args,
                               type="negative_binomial")
}

## input checks for identity nodes
check_inputs_node_identity <- function(parents, args) {
  if (is.null(args$formula)) {
    stop("'formula' must be specified when using type='identity'.")
  }
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
  } else if (is_time_varying_dag(dag)) {
    stop("'The 'dag' object may not contain time-varying nodes. Use",
         " the 'sim_discrete_time' function instead or remove all",
         " time-varying nodes.")
  }
}

## check the inputs of the node_conditional_probs function
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
         paste0(names(probs)[!names(probs) %in% unique(data[[parents]])],
                collapse="  "))
  } else if (length(parents) > 1 &&
             !all(names(probs) %in%
                  unique(interaction(data[, parents, with=FALSE])))) {
    stop("All elements in 'probs' must correspond to levels defined by the",
         "combined strata of all 'parents'. The following elements are not: ",
         paste0(names(probs)[!names(probs) %in%
                      unique(interaction(data[, parents, with=FALSE]))],
                collaps="  "))
  } else if (!(is.null(default_probs) || (is.numeric(default_probs) &&
               all(default_probs <=1 & default_probs >= 0)))) {
    stop("'default_probs' must be a numeric vector containing only",
         " values between 0 and 1 or NULL.")
  } else if (!is.null(default_probs) &
             length(default_probs) != min(dep_probs_length, na.rm=TRUE)) {
    stop("'default_probs' should contain one entry for each possible",
         " output class.")
  } else if (length(default_val) != 1) {
    stop("'default_val' must be a vector of length 1.")
  }
}

## check inputs for node_conditional_distr function
check_inputs_node_conditional_distr <- function(data, parents, distr,
                                                default_distr,
                                                default_distr_args, default_val,
                                                coerce2numeric) {

  if (is.null(names(distr))) {
    stop("All elements in 'distr' must be named using levels of 'parents'.")
  } else if (length(parents) == 1 &&
             !all(names(distr) %in% unique(data[[parents]]))) {
    stop("All elements in 'distr' must correspond to levels in ", parents,
         ". The following elements are not: ",
         paste0(names(distr)[!names(distr) %in% unique(data[[parents]])],
                collapse="  "))
  } else if (length(parents) > 1 &&
             !all(names(distr) %in%
                  unique(interaction(data[, parents, with=FALSE])))) {
    stop("All elements in 'distr' must correspond to levels defined by the",
         "combined strata of all 'parents'. The following elements are not: ",
         paste0(names(distr)[!names(distr) %in%
                        unique(interaction(data[, parents, with=FALSE]))],
                collapse="  "))
  } else if (!is.function(default_distr) && !is.null(default_distr)) {
    stop("'default_distr' must be a function or NULL.")
  } else if (length(default_val) != 1) {
    stop("'default_val' must be of length 1.")
  } else if (!(is.logical(coerce2numeric) && length(coerce2numeric)==1)) {
    stop("'coerce2numeric' must be either TRUE or FALSE.")
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
  } else if (!(is.character(data[[id]]) | is.numeric(data[[id]]))) {
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
check_inputs_sim2data <- function(sim, use_saved_states, to, target_event,
                                  keep_only_first, overlap,
                                  remove_not_at_risk) {

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
  } else if (!(is.logical(keep_only_first) & length(keep_only_first)==1)) {
    stop("'keep_only_first' must be either TRUE or FALSE.")
  } else if (!(is.logical(overlap) & length(overlap)==1)) {
    stop("'overlap' must be either TRUE or FALSE.")
  } else if (!(is.logical(remove_not_at_risk) &
               length(remove_not_at_risk)==1)) {
    stop("'remove_not_at_risk' must be either TRUE or FALSE.")
  }

  # extract node_time_to_event objects
  node_types <- lapply(sim$tx_nodes, FUN=function(x){x$type_str})
  tte_names <- names(sim$tte_past_events)
  save_past_events <- unlist(lapply(sim$tx_nodes[node_types=="time_to_event"],
                                    FUN=function(x){x$save_past_events}))

  if (!is.null(target_event) && !(is.character(target_event) &&
                                  length(target_event)==1 &&
                                  target_event %in% tte_names)) {
    stop("'target_event' must be a single character string, specifying a",
         " time_to_event node used in the creation of 'sim'.")
  }

  # raise warning due to missing information if needed
  if (length(tte_names) < length(sim$tx_nodes) & sim$save_states!="all") {
    warn_cols <- unlist(lapply(sim$tx_nodes[node_types!="time_to_event"],
                               FUN=function(x){x$name}))
    warning("Resulting data may be inaccurate for the following columns: '",
            paste0(warn_cols, collapse="', '"), "'\nbecause save_states!='all'",
            " in sim_discrete_time() function call. See details.")
  }

  # raise warning when using save_past_events = FALSE
  if (sim$save_states!="all" && any(save_past_events==FALSE) ) {
    warning("Resulting data may be inaccurate because save_past_events",
            " was set to FALSE in one or more nodes in the original",
            " sim_discrete_time() function call. See details.")
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

## check inputs for dag_from_data function
check_inputs_dag_from_data <- function(dag, data, return_models, na.rm) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() and",
         " node() function calls. See ?node for more details.")
  } else if (!inherits(data, "data.frame")) {
    stop("'data' must be a data.frame or data.table.")
  } else if (!(is.logical(return_models) & length(return_models)==1)) {
    stop("'return_models' must be either TRUE or FALSE.")
  } else if (!(is.logical(na.rm) & length(na.rm)==1)) {
    stop("'na.rm' must be either TRUE or FALSE.")
  } else if (is_time_varying_dag(dag)) {
    stop("'dag' may not contain time-dependent nodes added with the",
         " node_td() function.")
  }

  # check if all nodes are in data
  dag_names <- c(lapply(dag$root_nodes, function(x){x$name}),
                 lapply(dag$child_nodes, function(x){x$name}))
  if (!all(dag_names %in% colnames(data))) {
    stop("All nodes in 'dag' must correspond to a column in 'data'.",
         "Missing columns: ", dag_names[!dag_names %in% colnames(data)])
  }
}

## check inputs for plot.DAG function
check_inputs_plot.DAG <- function(dag, node_size, node_names, arrow_node_dist,
                                  gg_theme, include_td_nodes) {

  if (!inherits(dag, "DAG")) {
    stop("'x' must be a DAG object created using the empty_dag() and node()",
         " functions.")
  }

  size_dag <- length(names_DAG(dag, include_tx_nodes=include_td_nodes))

  if (size_dag < 2) {
    stop("The supplied DAG must have at least two nodes.")
  } else if (!((length(node_names) == size_dag && is.character(node_names)) |
               is.null(node_names))) {
    stop("'node_names' must be a character vector with one name for each node",
         " or NULL.")
  } else if (!((length(node_size) == 1 | length(node_size) == size_dag) &&
                is.numeric(node_size) && all(node_size > 0))) {
    stop("'node_size' must be a numeric vector of length 1 or with one entry",
         " per node. May only contain positive numbers.")
  } else if (!(length(arrow_node_dist)==1 && is.numeric(arrow_node_dist) &&
               arrow_node_dist >= 0)) {
    stop("'arrow_node_dist' must a single number >= 0.")
  } else if (!ggplot2::is.theme(gg_theme)) {
    stop("'gg_theme' must be a ggplot2 theme object.")
  }
}

## check inputs of the sim_discrete_time function
check_inputs_sim_discrete_time <- function(n_sim, dag, t0_sort_dag,
                                           t0_data,t0_transform_fun,
                                           t0_transform_args,max_t,
                                           tx_nodes, tx_nodes_order,
                                           tx_transform_fun, tx_transform_args,
                                           save_states, save_states_at,
                                           verbose) {
  # rudimentary type checks
  if (!is_time_varying_dag(dag)) {
    stop("'dag' must contain at least one time-varying node added using",
         " the node_td() function. For dag objects with no time-varying",
         " nodes, please use the sim_from_dag() function instead.")
  }
  if (!is.null(t0_data)) {
    stopifnot("'t0_data' must be a data.frame." = is.data.frame(t0_data))
  }
  if (!is.null(t0_transform_fun)) {
    stopifnot("'t0_transform_fun' must be a function." =
                is.function(t0_transform_fun))
  }
  stopifnot("'t0_transform_args' must be a list." = is.list(t0_transform_args))
  stopifnot("'max_t' must be a single integer." =
              (length(max_t) == 1 && is.numeric(max_t)))
  stopifnot("'tx_nodes' must be a list." = is.list(tx_nodes))
  if (!is.null(tx_nodes_order)) {
    stopifnot("'tx_nodes_order' must be a vector of type numeric." =
                is.vector(tx_nodes_order, mode = "numeric"))
  }
  if (!is.null(tx_transform_fun)) {
    stopifnot("'tx_transform_fun' must be a function." =
                is.function(tx_transform_fun))
  }
  stopifnot("'tx_transform_args' must be a list." =
              is.list(tx_transform_args))
  stopifnot("'save_states' must be a single character." =
              (length(save_states) == 1 && is.character(save_states)))
  if (!is.null(save_states_at)) {
    if (!is.numeric(save_states_at))  {
      stop("'save_states_at' must be either a single integer",
           " or a vector of type numeric.")
    }
  }
  stopifnot("'verbose' must be logical." = is.logical(verbose))

  # check content of t0_data
  if (is.data.frame(t0_data)) {
    stopifnot("'t0_data' needs to include at least one variable." =
                (ncol(t0_data) != 0))
    stopifnot("'t0_data' needs to include at least one row." =
                (nrow(t0_data) != 0))
  }

  # check content of t0_transform_fun
  if (is.function(t0_transform_fun)) {
    stopifnot(
      "'t0_transform_fun' needs to include at least one line." =
        length(body(t0_transform_fun)) != 0)

    # check content of t0_transform_args
    names_fun <- names(formals(t0_transform_fun))
    names_args <- names(t0_transform_args)
    if (!all(names_args %in% names_fun)) {
      stop("The following arguments are in 't0_transform_args' but are",
           " not define in 't0_transform_fun': ",
           paste0(names_args[!names_args %in% names_fun], collapse=","))
    }
  }

  # check content of tx_nodes_order
  if (is.vector(tx_nodes_order)) {
    if (!identical(length(tx_nodes_order), length(tx_nodes))) {
      stop("'tx_nodes_order' must be the same length as",
           " the number of time-varying nodes in the dag.")
    }
  }

  # check content of tx_transform_fun
  if (is.function(tx_transform_fun)) {
    stopifnot(
      "'tx_transform_fun' needs to include at least one line." =
        length(body(tx_transform_fun)) != 0)

    # check content of t0_transform_args
    names_fun <- names(formals(tx_transform_fun))
    names_args <- names(tx_transform_args)
    if (!all(names_args %in% names_fun)) {
      stop("The following arguments are in 'tx_transform_args' but are",
           " not define in 'tx_transform_fun': ",
           paste0(names_args[!names_args %in% names_fun], collapse=","))
    }
  }

  # check content of save_states
  if (is.character(save_states)) {
    stopifnot("'save_states' must be either 'last', 'all' or 'at_t'." =
                is.element(save_states, c("last", "all", "at_t")))
  }
}

## checking the inputs of the node_time_to_event function
check_inputs_node_time_to_event <- function(data, parents, sim_time, name,
                                            prob_fun, prob_fun_args,
                                            event_duration, immunity_duration,
                                            save_past_events) {
  # rudimentary type checks
  stopifnot("'data' must be a data.frame." = is.data.frame(data))
  stopifnot("'parents' must be a vector of type character." =
              is.vector(parents, mode = "character"))
  stopifnot("'sim_time' must be a single integer." =
              (length(sim_time) == 1 && is.numeric(sim_time)))
  stopifnot("'name' must be a single character." =
              (length(name) == 1 && is.character(name)))
  stopifnot("'prob_fun' must be a function or a single number." =
              is.function(prob_fun) | (is.numeric(prob_fun) &
                                         length(prob_fun)==1))
  stopifnot("'event_duration' must be a single integer > 0." =
              (length(event_duration) == 1 && is.numeric(event_duration) &&
                event_duration > 0))
  stopifnot("'immunity_duration' must be a single integer." =
              (length(immunity_duration) == 1 &&
                 is.numeric(immunity_duration)))
  stopifnot("'save_past_events' must be either TRUE or FALSE." =
              (length(save_past_events) == 1 && is.logical(save_past_events)))

  # check content of data
  if (is.data.frame(data)) {
    stopifnot(
      "'data' needs to include at least one variable." =
        (ncol(data) != 0))
    stopifnot(
      "'data' needs to include at least one row." =
        (nrow(data) != 0))
  }

  # check content of prob_fun
  if (is.function(prob_fun)) {
    stopifnot(
      "'prob_fun' needs to include at least one line." =
        length(body(prob_fun)) != 0)

    # check content of prob_fun_args
    arg_names <- setdiff(names(formals(prob_fun)), c("data", "sim_time"))
    if (length(args) != 0) {
      for (i in seq_len(length(arg_names))) {
        if(!arg_names[i] %in% names(prob_fun_args) &
           inherits(formals(prob_fun)[[arg_names[i]]], "name")) {
          stop("All parameters of 'prob_fun' except 'data' and 'sim_time'",
               " must be included in the node_td() call if they don't have a",
               " default value.")
        }
      }
    }
  }
}

## checking the inputs of the node_competing_events function
check_inputs_node_competing_events <- function(data, parents, sim_time, name,
                                               prob_fun, prob_fun_args,
                                               event_duration,
                                               immunity_duration,
                                               save_past_events) {
  # rudimentary type checks
  stopifnot("'data' must be a data.frame." = is.data.frame(data))
  stopifnot("'parents' must be a vector of type character." =
              is.vector(parents, mode = "character"))
  stopifnot("'sim_time' must be a single integer." =
              (length(sim_time) == 1 && is.numeric(sim_time)))
  stopifnot("'name' must be a single character." =
              (length(name) == 1 && is.character(name)))
  stopifnot("'prob_fun' must be a function." =
              is.function(prob_fun))
  stopifnot("'event_duration' must be a vector of integers > 0." =
              (length(event_duration) > 1 && is.numeric(event_duration) &&
               all(event_duration > 0)))
  stopifnot(
    "'immunity_duration' must be a single integer >= max(event_duration)." =
              (length(immunity_duration) == 1 &&
               is.numeric(immunity_duration) && immunity_duration >=
                 max(event_duration)))
  stopifnot("'save_past_events' must be either TRUE or FALSE." =
              (length(save_past_events) == 1 && is.logical(save_past_events)))

  # check content of data
  if (is.data.frame(data)) {
    stopifnot(
      "'data' needs to include at least one variable." =
        (ncol(data) != 0))
    stopifnot(
      "'data' needs to include at least one row." =
        (nrow(data) != 0))
  }

  # check content of prob_fun
  if (is.function(prob_fun)) {
    stopifnot(
      "'prob_fun' needs to include at least one line." =
        length(body(prob_fun)) != 0)

    # check content of prob_fun_args
    if (length(setdiff(names(formals(prob_fun)),
                       c("data", "sim_time"))) != 0) {
      for (i in seq_len(length(setdiff(names(formals(prob_fun)),
                                 c("data", "sim_time"))))) {
        if(!is.element(setdiff(names(formals(prob_fun)),
                               c("data", "sim_time"))[i],
                       names(prob_fun_args))) {
          stop("All parameters of 'prob_fun' except 'data' and 'sim_time'",
               " must be included in the node_td() call if they don't have a",
               " default value.")
        }
      }
    }
  }
}

## check inputs for the sim_n_datasets() function
check_inputs_sim_n_datasets <- function(dag, n_repeats, n_cores,
                                        data_format, data_format_args,
                                        progressbar) {

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag()",
         " and node() or node_td() functions.")
  } else if (!(length(n_repeats)==1 && is.numeric(n_repeats) &&
               n_repeats >= 1)) {
    stop("'n_repeats' must a single positive number.")
  } else if (!(length(n_cores)==1 && is.numeric(n_cores) &&
               n_cores >= 1)) {
    stop("'n_cores' must be a single positive number.")
  } else if (!(length(data_format)==1 && is.character(data_format))) {
    stop("'data_format' must be a single character string.")
  } else if (!is.list(data_format_args)) {
    stop("'data_format_args' must be a list.")
  } else if (!(length(progressbar)==1 && is.logical(progressbar))) {
    stop("'progressbar' must be either TRUE or FALSE.")
  }
}
