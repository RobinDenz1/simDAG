
## define a single node to grow DAG objects using the + syntax
#' @export
node <- function(name, type, parents=NULL, ...) {

  # NOTE: there is a lot of ugly code here because I need to avoid
  #       partial matching of function arguments
  call <- sys.call()

  if (length(call) < 3) {
    stop("Arguments 'name' and 'type' must be specified.")
  }

  # get parents
  if (length(names(call))==0 & length(call)==3) {
    parents <- NULL
  } else if (length(names(call))==0 || all(names(call)[1:4]=="")) {
    parents <- eval(call[[4]])
  } else {
    parents <- eval(call[["parents"]])
  }

  # get additional arguments
  call_names <- names(call)
  rel_names <- call_names[!call_names %in% c("name", "type", "parents") &
                          call_names!=""]
  args <- lapply(call[rel_names], eval, envir=parent.frame())

  # create node list
  if (length(parents) == 0 || all(parents=="")) {

    check_inputs_root_node(name=name, type=type)

    node_list <- list(name=name,
                      type=type,
                      parents=NULL,
                      params=args)
  } else {
    # NOTE: in an if statement because we need to allow child nodes that are
    #       almost completely empty for the dag_from_data function
    if (length(args) > 0) {
      check_inputs_child_node(name=name, type=type, parents=parents, args=args)
    }

    node_list <- list(name=name,
                      type=type,
                      parents=parents)
    node_list <- append(node_list, args)
  }

  class(node_list) <- "DAG.node"

  return(node_list)
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

    if (length(x)==3) {
      cat("  - no additional parameters\n")
    } else {

      if (!is.null(x$betas)) {
        cat("  - betas: ", paste0(x$betas, collapse=", "), "\n", sep="")
      }

      if (!is.null(x$intercept)) {
        cat("  - intercept: ", x$intercept, "\n", sep="")
      }

      other_args <- names(x)[!names(x) %in% c("name", "type", "parents",
                                              "betas", "intercept")]
      if (length(other_args) > 0) {
        param_str <- paste0(other_args, collapse=", ")
        cat("  - with additional parameters: '", param_str, "'\n", sep="")
      }
    }
  }
}

## S3 summary method for DAG.node objects
#' @export
summary.DAG.node <- function(object, ...) {
  print.DAG.node(x=object, ...)
}
