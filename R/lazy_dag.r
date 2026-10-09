
## given an expression that creates a partially complete DAG with some free
## param() calls included, just safe that expression instead of evaluating it
#' @export
lazy_dag <- function(expr) {

  if (missing(expr)) {
    stop("Please supply a DAG expression.", call.=FALSE)
  }

  # capture the expression without evaluating it
  expr <- substitute(expr)

  # identify its free parameters
  params <- find_param_names(expr)

  if (length(params) == 0L) {
    stop("No `param()` placeholders found in the DAG expression.", call.=FALSE)
  }

  # preserve the environment in which the expression was supplied
  envir <- parent.frame()

  # put together output
  out <- list(expr=expr, params=params, envir=envir)
  class(out) <- "lazy_DAG"

  return(out)
}

## substitute param() expressions by actual values and evaluate the defined
## DAG in the environment the lazy_dag() call was made in
#' @export
set_lazy_dag <- function(lazy_dag, params) {

  # accept a named list or a named atomic vector
  if (!is.list(params)) {
    if (is.atomic(params) && !is.null(names(params))) {
      params <- as.list(params)
    } else {
      stop("`params` must be a named list or named atomic vector.", call.=FALSE)
    }
  }

  check_inputs_set_lazy_dag(lazy_dag=lazy_dag, params=params)

  # replace placeholders throughout the original expression
  expr <- replace_param_calls(lazy_dag$expr, params)

  # evaluate the now fully specified DAG expression
  eval(expr, envir=lazy_dag$envir)
}

## identify if some stored language object is a call to param()
is_param_call <- function(x) {
  is.call(x) && is.symbol(x[[1L]]) && identical(as.character(x[[1L]]), "param")
}

## extract the parameter name from a param(NAME) call
get_param_name <- function(x) {

  if (!is_param_call(x)) {
    return(NULL)
  }

  if (length(x) != 2L || !is.symbol(x[[2L]])) {
    stop("Use `param()` with exactly one unquoted parameter name, ",
         "for example: `param(intercept)` would be valid syntax.", call.=FALSE)
  }

  return(as.character(x[[2L]]))
}

## recursively find all parameter names given an un-evaluated expression
find_param_names <- function(expr) {

  if (is.call(expr)) {
    name <- get_param_name(expr)

    if (!is.null(name)) {
      return(name)
    }

    expr <- as.list(expr)
  } else if (is.pairlist(expr) || is.expression(expr)) {
    expr <- as.list(expr)
  } else {
    return(character())
  }

  out <- unique(unlist(lapply(expr, find_param_names), use.names=FALSE))

  return(out)
}

## recursively replace param(NAME) with its actual value
replace_param_calls <- function(expr, params) {

  if (is.call(expr)) {

    name <- get_param_name(expr)

    if (!is.null(name)) {
      return(params[[name]])
    }

    for (i in seq_along(expr)) {
      expr[[i]] <- replace_param_calls(expr[[i]], params)
    }
  } else if (is.pairlist(expr) || is.expression(expr)) {
    for (i in seq_along(expr)) {
      expr[[i]] <- replace_param_calls(expr[[i]], params)
    }
  }

  return(expr)
}

## S3 print() method for lazy_DAG objects
#' @export
print.lazy_DAG <- function(x, ...) {

  # create fake DAG
  params <- rep(Inf, length(x$params))
  names(params) <- x$params
  dag <- set_lazy_dag(lazy_dag=x, params=params)

  # use regular print() method with extra arguments
  print.DAG(dag, lazy=TRUE, n_unspecified=length(x$params))
}

## S3 summary() method for lazy_DAG objects
#' @export
summary.lazy_DAG <- function(object, ...) {

  # create fake DAG
  params <- rep(Inf, length(object$params))
  names(params) <- object$params
  dag <- set_lazy_dag(lazy_dag=object, params=params)

  # use regular summary() method with extra arguments
  summary.DAG(dag, lazy=TRUE)
}

## S3 plot() method for lazy_DAG objects
#' @export
plot.lazy_DAG <- function(x, ...) {

  # create fake DAG
  params <- rep(Inf, length(x$params))
  names(params) <- x$params
  dag <- set_lazy_dag(lazy_dag=x, params=params)

  # use regular plot() method
  plot.DAG(x=dag, ...)
}

## check inputs for the set_lazy_dag() function
check_inputs_set_lazy_dag <- function(lazy_dag, params) {

  if (!inherits(lazy_dag, "lazy_DAG")) {
    stop("`x` must be a `lazy_DAG` object.", call.=FALSE)
  }

  param_names <- names(params)

  if (is.null(param_names) || anyNA(param_names) || any(!nzchar(param_names)) ||
      anyDuplicated(param_names)) {
    stop("`params` must have unique, non-empty names.", call.=FALSE)
  }

  # check that every required parameter has exactly one supplied value
  missing <- setdiff(lazy_dag$params, param_names)
  extra <- setdiff(param_names, lazy_dag$params)

  if (length(missing) > 0L) {
    stop("Missing parameter values: ", paste(missing, collapse=", "),
         call.=FALSE)
  } else if (length(extra) > 0L) {
    stop("Unused parameter values supplied: ", paste(extra, collapse=", "),
         call.=FALSE)
  } else if (any(vapply(params, is.null, logical(1)))) {
    stop("Parameter values must not be NULL.", call.=FALSE)
  }
}
