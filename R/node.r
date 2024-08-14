
## define a single time-fixed node to grow DAG objects using the + syntax
#' @export
node <- function(name, type, parents=NULL, formula=NULL, ...) {

  # NOTE: there is a lot of ugly code here because I want to avoid
  #       partial matching of function arguments
  call <- sys.call()

  if (length(call) < 3) {
    stop("Arguments 'name' and 'type' must be specified.")
  }

  parents <- get_arg_from_call(call=call, envir=environment(),
                               name="parents", position=3)
  formula <- get_arg_from_call(call=call, envir=environment(),
                               name="formula", position=4)
  formula <- sanitize_formula(formula)

  if (inherits(formula, "formula")) {
    parents <- all.vars(formula)
    warning("Using regular formulas in 'formula' was deprecated in version",
            " 0.2.0 and will no longer be supported in the next version",
            " of this package. Please use the new custom formulas instead.")
  } else if (is.character(formula) & is.null(parents)) {
    parents <- parents_from_formula(formula, node_type=type)
  }

  # get additional arguments
  call_names <- names(call)
  rel_names <- call_names[!call_names %in% c("name", "type", "parents",
                                             "formula", "time_varying") &
                          call_names!=""]
  args <- lapply(call[rel_names], eval, envir=parent.frame())

  # create node list
  if (length(parents) == 0 || all(parents=="")) {

    check_inputs_root_node(name=name, type=type)

    node_list <- create_node_list(name=name, type=type, parents=NULL,
                                  formula=NULL, time_varying=FALSE,
                                  root=TRUE, args=args)
  } else {
    # NOTE: in an if statement because we need to allow child nodes that are
    #       almost completely empty for the dag_from_data function
    if (length(args) > 0) {
      check_inputs_child_node(name=name, type=type, parents=parents, args=args,
                              formula=formula, time_varying=FALSE)
    }

    node_list <- create_node_list(name=name, type=type, parents=parents,
                                  formula=formula, time_varying=FALSE,
                                  args=args, root=FALSE)
  }

  return(node_list)
}

## define a single time-varying node to grow DAG objects using the + syntax
#' @export
node_td <- function(name, type, parents=NULL, formula=NULL, ...) {

  call <- sys.call()

  if (length(call) < 3) {
    stop("Arguments 'name' and 'type' must be specified.")
  }

  parents <- get_arg_from_call(call=call, envir=environment(),
                               name="parents", position=3)
  formula <- get_arg_from_call(call=call, envir=environment(),
                               name="formula", position=4)
  formula <- sanitize_formula(formula)

  if (inherits(formula, "formula")) {
    parents <- all.vars(formula)
    warning("Using regular formulas in 'formula' was deprecated in version",
            " 0.2.0 and will no longer be supported in the next version",
            " of this package. Please use the new custom formulas instead.")
  } else if (is.character(formula) & is.null(parents)) {
    parents <- parents_from_formula(formula, node_type=type)
  }

  # get additional arguments
  call_names <- names(call)
  rel_names <- call_names[!call_names %in% c("name", "type", "parents",
                                             "formula") &
                            call_names!=""]
  args <- lapply(call[rel_names], eval, envir=parent.frame())

  # create node list
  if (length(parents) == 0 || all(parents=="")) {
    parents <- NULL
  }

  if (length(args) > 0) {
    check_inputs_child_node(name=name, type=type, parents=parents, args=args,
                            formula=formula, time_varying=TRUE)
  }

  node_list <- create_node_list(name=name, type=type, parents=parents,
                                formula=formula, root=FALSE,
                                time_varying=TRUE, args=args)
  return(node_list)
}

## extracts an argument from a sys.call() object given it's name
## and position, ignoring partial name matching
get_arg_from_call <- function(call, envir, name, position) {

  if (length(names(call))==0 & length(call) <= position) {
    argument <- formals("node")[[name]]
  } else if (length(names(call))==0 || all(names(call)[1:(position+1)]=="")) {
    argument <- eval(call[[position+1]], envir=envir)
  } else if (is.null(eval(call[[name]], envir=envir))) {
    argument <- formals("node")[[name]]
  } else {
    argument <- eval(call[[name]], envir=envir)
  }

  return(argument)
}

## given parameters, create a node list
create_node_list <- function(name, type, parents, formula, time_varying,
                             root, args) {

  name <- unique(name)
  if (length(name) > 1) {

    node_list <- vector(mode="list", length=length(name))
    for (i in seq_len(length(name))) {
      node_list[[i]] <- create_DAG.node(name=name[i], type=type,
                                        parents=parents, formula=formula,
                                        time_varying=time_varying,
                                        root=root, args=args)
    }
    class(node_list) <- "DAG.node"

  } else {
    node_list <- create_DAG.node(name=name, type=type,
                                 parents=parents, formula=formula,
                                 time_varying=time_varying,
                                 root=root, args=args)
  }

  return(node_list)
}

## given parameters, create a single DAG.node object
create_DAG.node <- function(name, type, parents, formula, time_varying,
                            root, args) {

  # create type_str and type_fun
  if (is.function(type)) {
    type_str <- extract_function_name(type)
    type_fun <- type
  } else if (root) {
    type_str <- type
    type_fun <- get(type)
  } else {
    type_str <- type
    type_fun <- get(paste0("node_", type))
  }

  type_str <- correct_type_str(type_str)

  node_list <- list(name=name,
                    type_str=type_str,
                    type_fun=type_fun,
                    parents=parents,
                    time_varying=time_varying)

  if (!is.null(formula)) {
    node_list$formula <- formula
  }

  if (root) {
    node_list$params <- args
  } else {
    node_list <- append(node_list, args)
  }

  class(node_list) <- "DAG.node"

  return(node_list)
}

## replaces the type "node_.." with just the latter for built in
## node types to produce the same output when supplied a function or string
correct_type_str <- function(type_str) {

  if (type_str %in% c("node_gaussian", "node_binomial", "node_conditional_prob",
                      "node_conditional_distr", "node_multinomial",
                      "node_poisson", "node_negative_binomial", "node_cox",
                      "node_time_to_event", "node_competing_events")) {
    type_str <- gsub("node_", "", type_str)
  }

  return(type_str)
}

## S3 print method for DAG.node objects
#' @export
print.DAG.node <- function(x, ...) {

  # list of DAG.nodes
  if (!is.character(x[[1]])) {
    cat("A list of DAG.node objects.")
  # root nodes
  } else if ((length(x$parents) == 0 || all(x$parents=="")) &&
             !x$type_str %in% c("time_to_event", "competing_events")) {
    cat("A DAG.node object specifying a single root node with:\n")
    cat("  - name: '", x$name, "'\n", sep="")
    cat("  - type: '", x$type_str, "'\n", sep="")

    if (length(x$params)==0) {
      cat("  - no additional parameters\n")
    } else {
      param_str <- paste0(paste0(names(x$params), "=", x$params), collapse=", ")
      cat("  - with parameters: ", param_str, "\n", sep="")
    }

  # child nodes
  } else {
    cat("A DAG.node object specifying a single child node with:\n")
    cat("  - name: '", x$name, "'\n", sep="")
    cat("  - type: '", x$type_str, "'\n", sep="")

    if (length(x$parents) > 0) {
      cat("  - parents: '", paste0(x$parents, collapse="', '"), "'\n", sep="")
    }

    if (length(x)==5) {
      cat("  - no additional parameters\n")
    } else {

      if (!is.null(x$betas)) {
        cat("  - betas: ", paste0(x$betas, collapse=", "), "\n", sep="")
      }

      if (!is.null(x$intercept)) {
        cat("  - intercept: ", x$intercept, "\n", sep="")
      }

      other_args <- names(x)[!names(x) %in% c("name", "type_str", "type_fun",
                                              "parents", "betas", "intercept",
                                              "time_varying")]
      if (length(other_args) > 0) {
        param_str <- paste0(other_args, collapse=", ")
        cat("  - with additional parameters: ", param_str, "\n", sep="")
      }
    }
  }
  if (is.character(x[[1]]) && x$time_varying) {
    cat("This node may change over time.\n")
  }
}

## S3 summary method for DAG.node objects
#' @export
summary.DAG.node <- function(object, ...) {

  if (!is.character(object[[1]])) {
    str_equations <- character()
    for (i in seq_len(length(object))) {
      str_equations_i <- structural_equation(object[[i]])
      str_equations <- c(str_equations, str_equations_i)
    }
  } else {
    str_equations <- structural_equation(object)
  }
  str_equations_print <- align_str_equations(str_equations)

  cat("A DAG.node object using the following structural equation(s):\n\n")
  cat(str_equations_print, sep="\n")

  return(invisible(str_equations))
}
