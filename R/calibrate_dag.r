
## automatically find suitable parameters that result in some user-defined
## target quantities to be satisfied in the resulting data
#' @export
calibrate_dag <- function(lazy_dag, targets, params, n_sim_run=100000,
                          n_sim_eval=1000000, n_reps_eval=1,
                          seed=stats::runif(1, 0, 100),
                          optimizer="auto", optim_method=NULL,
                          optim_control=list(maxit=200), objective_fun=NULL,
                          ...) {

  # should be a lazy DAG, not a regular DAG
  if (inherits(lazy_dag, "DAG")) {
    stop("'lazy_dag' should be a `lazy_DAG` object, created using lazy_dag()",
         ", not a regular `DAG` object.", call.=FALSE)
  } else if (!inherits(lazy_dag, "lazy_DAG")) {
    stop("'lazy_dag' must be a `lazy_DAG` object created using the",
         " lazy_dag() function.", call.=FALSE)
  }

  # warn if estimation is likely impossible
  if (length(targets) > length(params)) {
    warning("There are ", length(targets), " specified target values",
            " but only ", length(params), " free parameter(s). ",
            "Optimization will likely be impossible.", call.=FALSE)
  }

  # normalize and validate parameter specifications
  params <- normalize_params(params, lazy_dag)
  param_names <- names(params)
  start <- vapply(X=params, FUN=function(x){x$start}, FUN.VALUE=numeric(1))

  # missing ranges correspond to unbounded parameters
  lower <- vapply(
    X=params,
    FUN=function(x) {if (is.null(x$range)) -Inf else x$range[1]},
    FUN.VALUE=numeric(1)
  )

  upper <- vapply(
    X=params,
    FUN=function(x) {if (is.null(x$range)) Inf else x$range[2]},
    FUN.VALUE=numeric(1)
  )

  # extract target values
  target_values <- vapply(
    X=targets, FUN=function(x) {as.numeric(x$target)}, FUN.VALUE=numeric(1)
  )

  # extract target scales, with default scale = 1
  target_scales <- vapply(X=targets, FUN=get_target_scale, FUN.VALUE=numeric(1))

  if (any(!is.finite(target_scales) | target_scales <= 0)) {
    stop("Target scales must be finite and strictly positive.", call.=FALSE)
  }

  # automatically use uniroot() for the simple, bounded,
  # single-parameter/single-target case under optimizer = "auto"
  uniroot_possible <- length(targets) == 1L && length(params) == 1L &&
    !is.null(params[[1]]$range) && all(is.finite(params[[1]]$range))

  # determine which optimizer to use
  if (optimizer=="auto" && uniroot_possible && is.null(optim_method) &&
      is.null(objective_fun)) {
    optimizer <- "uniroot"
  } else if (optimizer=="auto") {
    optimizer <- "optim"
  }

  # warn if uniroot() specified but not possible
  if (optimizer=="uniroot" && !uniroot_possible) {
    warning("optimizer = 'uniroot' was used but uniroot() cannot be applied",
            " here (> 1 targets, > 1 params or no supplied range).",
            " Using optimizer = 'optim' instead.", call.=FALSE)
    optimizer <- "optim"
  }

  # infer parameter scaling if not explicitly supplied
  if (is.null(optim_control$parscale) &&
      optimizer %in% c("optim", "optim2", "optimh")) {
    optim_control$parscale <- vapply(X=params, FUN=parscale_default,
                                     numeric(1))
  }

  # apply chosen optimizer
  if (optimizer=="uniroot") {

    uniroot_obj <- stats::uniroot(
      f = get_objective,
      interval = params[[1]]$range,
      lazy_dag = lazy_dag,
      targets = targets,
      n_sim = n_sim_run,
      seed = seed,
      mode = "root",
      target_values = target_values,
      target_scales = target_scales,
      ...
    )

    est_params <- stats::setNames(uniroot_obj$root, param_names)
    optim_obj <- NULL
    method <- "uniroot"

  } else if (optimizer %in% c("optim", "optim2", "optimh")) {

    if (optimizer %in% c("optim2", "optimh")) {
      requireNamespace("calibrar", quietly=TRUE)
    }

    # default to L-BFGS-B, but permit other optim() methods
    if (is.null(optim_method) && optimizer %in% c("optim", "optim2")) {
      method <- "L-BFGS-B"
    } else if (is.null(optim_method) && optimizer=="optimh") {
      method <- "Nelder-Mead"
    } else {
      method <- optim_method
    }

    # methods other than L-BFGS-B and Brent cannot enforce bounds
    if ((any(is.finite(lower) | is.finite(upper))) &&
        !method %in% c("L-BFGS-B", "Brent") && optimizer=="optim") {
      stop("The selected method `", method, "` does not support parameter ",
           "bounds.", call.=FALSE)
    }

    if (method == "Brent" &&
        (length(params) != 1L || !all(is.finite(c(lower, upper))))) {
      stop("The Brent method requires exactly one parameter with finite",
           " bounds.", call.=FALSE)
    }

    opt_args <- list(
      par = start,
      fn = get_objective,
      method = method,
      control = optim_control,
      seed = seed,
      n_sim = n_sim_run,
      lazy_dag = lazy_dag,
      targets = targets,
      objective_fun = objective_fun,
      mode = "loss",
      target_values = target_values,
      target_scales = target_scales
    )

    if (method %in% c("L-BFGS-B", "Brent")) {
      opt_args$lower <- lower
      opt_args$upper <- upper
    }

    # forward extra simulation arguments
    opt_args <- c(opt_args, list(...))

    if (optimizer=="optim") {
      optim_obj <- do.call(stats::optim, opt_args)
    } else if (optimizer=="optim2") {
      optim_obj <- do.call(calibrar::optim2, opt_args)
    } else if (optimizer=="optimh") {
      optim_obj <- do.call(calibrar::optimh, opt_args)
    }

    est_params <- optim_obj$par
    uniroot_obj <- NULL

    if (length(names(est_params))==0) {
      names(est_params) <- lazy_dag$params
    }
  }

  # creation of final DAG
  dag <- set_lazy_dag(lazy_dag, params=est_params)

  # independent, higher-precision target evaluation
  if (!is.null(seed)) {
    seed_eval <- seed + 1
  } else {
    seed_eval <- NULL
  }

  # using a single large evaluation
  if (n_reps_eval==1) {
    target_eval <- estimate_targets(
      lazy_dag = lazy_dag,
      params = est_params,
      targets = targets,
      n_sim = n_sim_eval,
      seed = seed_eval,
      ...
    )
    d_target_eval <- NULL
  # performing an evaluation Monte-Carlo style
  } else {
    ldata <- sim_n_datasets(dag=dag, n_sim=n_sim_eval, n_repeats=n_reps_eval,
                            seed=seed_eval, ...)
    target_eval_reps <- lapply(X=ldata, FUN=apply_target_funs, targets=targets,
                               real_dag=dag)
    d_target_eval <- get_target_evaluation(
      target_eval_reps = target_eval_reps,
      targets = targets,
      target_values = target_values,
      target_scales = target_scales
    )
    target_eval <- colMeans(d_target_eval$d_target)
  }

  # given user inputs
  inputs <- list(
    lazy_dag = lazy_dag,
    params = params,
    targets = targets,
    target_values = target_values,
    target_scales = target_scales,
    n_sim_run = n_sim_run,
    n_sim_eval = n_sim_eval,
    n_reps_eval = n_reps_eval,
    seed = seed,
    optimizer = optimizer,
    optim_control = optim_control,
    optim_method = method
  )

  # put together output object
  out <- list(
    dag = dag,
    inputs = inputs,
    est_params = est_params,
    optim_obj = optim_obj,
    uniroot_obj = uniroot_obj,
    target_eval = target_eval,
    d_target_eval = d_target_eval
  )

  class(out) <- "calibrated_DAG"
  return(out)
}

## default scaling for parameters
parscale_default <- function(x) {
  if (!is.null(x$scale)) {
    out <- x$scale
  } else if (!is.null(x$range) && all(is.finite(x$range))) {
    out <- diff(x$range) / 2
  } else {
    out <- max(1, abs(x$start))
  }
  return(out)
}

## extract scale from element in target list and set to 1 if not found
get_target_scale <- function(x) {
  if (is.null(x$scale)) {
    out <- 1
  } else {
    out <- as.numeric(x$scale)
  }
  return(out)
}

## given a list of target evaluations, create three output data.tables
#' @importFrom data.table rbindlist
#' @importFrom data.table copy
#' @importFrom data.table set
get_target_evaluation <- function(target_eval_reps, targets, target_values,
                                  target_scales) {

  # get data.table of results
  d_target <- rbindlist(lapply(target_eval_reps, as.list))
  colnames(d_target) <- names(targets)

  # get errors
  d_target_errors <- copy(d_target)

  for (nm in names(target_values)) {
    set(d_target_errors, j=nm,
        value=d_target_errors[[nm]] - target_values[[nm]])
  }

  # get scaled errors
  d_target_scaled_errors <- copy(d_target_errors)

  for (nm in names(target_values)) {
    set(d_target_scaled_errors, j=nm,
        value=d_target_scaled_errors[[nm]] / target_scales[[nm]])
  }

  out <- list(
    d_target = d_target,
    d_target_errors = d_target_errors,
    d_target_scaled_errors = d_target_scaled_errors
  )
  return(out)
}

## estimate the value of the targets given specific parameter values using
## a single simulation run
estimate_targets <- function(lazy_dag, params, targets, n_sim, seed, ...) {

  if (length(names(params))==0) {
    names(params) <- lazy_dag$params
  }

  # get actual dag with supplied parameters
  real_dag <- set_lazy_dag(lazy_dag=lazy_dag, params=params)

  # get one simulation run
  data <- sim_n_datasets(dag=real_dag, n_sim=n_sim, n_repeats=1,
                         seed=seed, ...)[[1]]

  # evaluate all target functions
  out <- apply_target_funs(data=data, targets=targets, real_dag=real_dag)

  return(out)
}

## given data and a list of targets, apply the contained target functions
## to the data and return a numeric vector
apply_target_funs <- function(data, targets, real_dag) {

  out <- numeric(length(targets))
  for (i in seq_len(length(targets))) {
    if (!is.null(targets[[i]]$use_dag) && targets[[i]]$use_dag) {
      out[i] <- targets[[i]]$eval_fun(real_dag)
    } else {
      out[i] <- targets[[i]]$eval_fun(data)
    }
  }

  return(out)
}

## calculates the objective function that we use to find the solution
## in the later optim() call
get_objective <- function(params, targets, lazy_dag, n_sim, seed,
                          objective_fun=NULL, target_values, target_scales,
                          mode, est_targets=NULL, ...) {


  est_targets <- estimate_targets(
    lazy_dag = lazy_dag,
    params = params,
    targets = targets,
    n_sim = n_sim,
    seed = seed,
    ...
  )

  errors <- est_targets - target_values
  scaled_errors <- errors / target_scales

  # signed residual required by uniroot()
  if (mode == "root") {
    return(unname(scaled_errors))
  }

  # invalid target estimates are penalized
  if (any(!is.finite(scaled_errors))) {
    return(Inf)
  }

  # default objective: scaled sum of squares
  if (is.null(objective_fun)) {
    return(sum(scaled_errors^2))
  }

  # custom objective must return one finite numeric value
  out <- objective_fun(scaled_errors)

  if (!is.numeric(out) || length(out) != 1L || !is.finite(out)) {
    stop("`objective_fun` must return one finite numeric value.", call.=FALSE)
  }

  return(unname(out))
}

## transforms different valid "params" inputs into a standardized form
## and checks whether the input is actually valid
normalize_params <- function(params, lazy_dag) {

  param_names <- lazy_dag$params

  # allow a flat specification for one parameter
  if (is.atomic(params) && !is.null(names(params)) &&
      "start" %in% names(params)) {
    params <- as.list(params)
  }

  if (is.list(params) && "start" %in% names(params)) {
    if (length(param_names) != 1L) {
      stop("A non-nested parameter specification requires exactly ",
           "one free parameter.", call.=FALSE)
    }
    params <- stats::setNames(list(params), param_names)
  }

  if (!is.list(params) || is.null(names(params)) ||
      anyDuplicated(names(params))) {
    stop("`params` must be a named list of parameter specifications.",
         call.=FALSE)
  }

  missing <- setdiff(param_names, names(params))
  extra <- setdiff(names(params), param_names)

  if (length(missing) || length(extra)) {
    stop(
      "Parameter names must match the placeholders in `lazy_dag`.",
      if (length(missing) > 0) {
        paste0("\nMissing: ", paste(missing, collapse = ", "))
      },
      if (length(extra) > 0) {
        paste0("\nUnknown: ", paste(extra, collapse = ", "))
      },
      call.=FALSE
    )
  }

  params <- params[param_names]

  for (nm in param_names) {

    spec <- params[[nm]]

    # allow list(par1 = c(start = 1))
    if (is.atomic(spec) && !is.null(names(spec))) {
      spec <- as.list(spec)
    }

    if (!is.list(spec) || is.null(spec$start) || !is.numeric(spec$start) ||
        length(spec$start) != 1L || !is.finite(spec$start)) {
      stop("Parameter `", nm, "` must have a finite numeric `start` value.",
           call.=FALSE)
    }

    if (!is.null(spec$range)) {
      r <- spec$range

      if (!is.numeric(r) || length(r) != 2L || anyNA(r) || r[1] >= r[2]) {
        stop("`range` for parameter `", nm, "` must contain two increasing",
             " numeric bounds.", call.=FALSE)
      }
    }

    if (!is.null(spec$scale)) {
      if (!is.numeric(spec$scale) || length(spec$scale) != 1L ||
          !is.finite(spec$scale) || spec$scale <= 0) {
        stop("`scale` for parameter `", nm, "` must be finite and",
             " strictly positive.", call.=FALSE)
      }
    }

    params[[nm]] <- spec
  }

  return(params)
}
