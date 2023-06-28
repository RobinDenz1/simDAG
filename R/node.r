
## define a single node to grow DAG objects using the + syntax
#' @export
node <- function(name, type, parents=NULL, formula=NULL,
                 time_varying=FALSE, ...) {

  # NOTE: there is a lot of ugly code here because I need to avoid
  #       partial matching of function arguments
  call <- sys.call()

  if (length(call) < 3) {
    stop("Arguments 'name' and 'type' must be specified.")
  }

  parents <- get_arg_from_call(call=call, envir=environment(),
                               name="parents", position=3)
  formula <- get_arg_from_call(call=call, envir=environment(),
                               name="formula", position=4)
  time_varying <- get_arg_from_call(call=call, envir=environment(),
                                    name="time_varying", position=5)

  if (inherits(formula, "formula")) {
    parents <- all.vars(formula)
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

    node_list <- list(name=name,
                      type=type,
                      parents=NULL,
                      time_varying=time_varying,
                      params=args)
  } else {
    # NOTE: in an if statement because we need to allow child nodes that are
    #       almost completely empty for the dag_from_data function
    if (length(args) > 0) {
      check_inputs_child_node(name=name, type=type, parents=parents, args=args)
    }

    node_list <- list(name=name,
                      type=type,
                      parents=parents,
                      time_varying=time_varying)

    if (!is.null(formula)) {
      node_list$formula <- formula
    }

    node_list <- append(node_list, args)
  }

  class(node_list) <- "DAG.node"

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

## S3 print method for DAG.node objects
#' @export
print.DAG.node <- function(x, ...) {

  # root nodes
  if (length(x$parents) == 0 || all(x$parents=="")) {
    cat("A DAG.node object specifying a single root node with:\n")
    cat("  - name: '", x$name, "'\n", sep="")
    cat("  - type: '", x$type, "'\n", sep="")

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
    cat("  - type: '", x$type, "'\n", sep="")
    cat("  - parents: '", paste0(x$parents, collapse="', '"), "'\n", sep="")

    if (length(x)==4) {
      cat("  - no additional parameters\n")
    } else {

      if (!is.null(x$betas)) {
        cat("  - betas: ", paste0(x$betas, collapse=", "), "\n", sep="")
      }

      if (!is.null(x$intercept)) {
        cat("  - intercept: ", x$intercept, "\n", sep="")
      }

      other_args <- names(x)[!names(x) %in% c("name", "type", "parents",
                                              "betas", "intercept",
                                              "time_varying")]
      if (length(other_args) > 0) {
        param_str <- paste0(other_args, collapse=", ")
        cat("  - with additional parameters: ", param_str, "\n", sep="")
      }
    }
  }
  if (x$time_varying) {
    cat("This node may change over time.\n")
  }
}

## S3 summary method for DAG.node objects
#' @export
summary.DAG.node <- function(object, ...) {
  print.DAG.node(x=object, ...)
}
