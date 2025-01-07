
## generate multiple datasets from a single dag objects using
## either the sim_from_dag() or sim_discrete_time() function
## with or without parallel processing
#' @importFrom data.table setDTthreads
#' @export
sim_n_datasets <- function(dag, n_sim, n_repeats,
                           n_cores=parallel::detectCores(),
                           data_format="raw", data_format_args=list(),
                           seed=stats::runif(1), progressbar=TRUE, ...) {

  check_inputs_sim_n_datasets(dag=dag, n_repeats=n_repeats,
                              n_cores=n_cores, data_format=data_format,
                              data_format_args=data_format_args,
                              progressbar=progressbar)

  # without parallel processing
  if (n_cores == 1) {

    set.seed(seed)
    out <- vector(mode="list", length=n_repeats)
    for (i in seq_len(n_repeats)) {
      out[[i]] <- generate_one_dataset(dag=dag, data_format=data_format,
                                       data_format_args=data_format_args,
                                       n_sim=n_sim, ...)
    }

  # with parallel processing
  } else {

    requireNamespace("parallel")
    requireNamespace("doSNOW")
    requireNamespace("doRNG")
    requireNamespace("foreach")

    `%dorng%` <- doRNG::`%dorng%`

    # set up cluster
    cl <- parallel::makeCluster(n_cores, outfile="")
    doSNOW::registerDoSNOW(cl)

    args <- list(...)
    if ("sort_dag" %in% names(args)) {
      pckgs <- c("data.table", "simDAG", "Rfast")
    } else {
      pckgs <- c("data.table", "simDAG")
    }

    glob_funs <- ls(envir=.GlobalEnv)[vapply(ls(envir=.GlobalEnv), function(obj)
      "function"==class(eval(parse(text=obj)))[1], FUN.VALUE=logical(1))]

    # add progress bar
    if (progressbar) {
      pb <- utils::txtProgressBar(max=n_repeats, style=3)
      progress <- function(n) {utils::setTxtProgressBar(pb, n)}
      opts <- list(progress=progress)
    } else {
      opts <- NULL
    }

    # start simulation
    set.seed(seed)
    out <- foreach::foreach(i=seq_len(n_repeats), .packages=pckgs,
                            .export=glob_funs, .options.snow=opts) %dorng% {

      setDTthreads(1)

      generate_one_dataset <- utils::getFromNamespace("generate_one_dataset",
                                                      "simDAG")

      generate_one_dataset(dag=dag, data_format=data_format,
                           data_format_args=data_format_args,
                           n_sim=n_sim, ...)
    }
    on.exit(close(pb))
    on.exit(parallel::stopCluster(cl))
  }

  return(out)
}

## generate one dataset using either the sim_from_dag() or the
## sim_discrete_time() function and optionally format it
generate_one_dataset <- function(dag, data_format, data_format_args, n_sim,
                                 ...) {

  # simulate the dataset
  if (length(dag$tx_nodes) > 0) {
    dat <- sim_discrete_time(dag=dag, n_sim=n_sim, ...)
  } else {
    dat <- sim_from_dag(dag=dag, n_sim=n_sim, ...)
  }

  # transform it, if specified
  if (data_format %in% c("start_stop", "long", "wide") &&
      length(dag$tx_nodes) > 0) {

    data_format_args$to <- data_format
    data_format_args$sim <- dat
    dat <- do.call("sim2data", args=data_format_args)

  } else if (data_format != "raw") {
    data_format_args$data <- dat
    dat <- do.call(data_format, args=data_format_args)
  }

  return(dat)
}
