
## generate multiple datasets from a single dag objects using
## either the sim_from_dag() or sim_discrete_time() function
## with or without parallel processing
#' @importFrom data.table setDTthreads
#' @export
sim_n_datasets <- function(dag, n_sim, n_repeats, n_cores=1,
                           data_format="raw", data_format_args=list(),
                           seed=NULL, progressbar=TRUE, ...) {

  check_inputs_sim_n_datasets(dag=dag, n_repeats=n_repeats,
                              n_cores=n_cores, data_format=data_format,
                              data_format_args=data_format_args,
                              progressbar=progressbar)

  # select which function should be used
  if (length(dag$tx_nodes) > 0) {
    types <- vapply(dag$tx_nodes, FUN=function(x){x$type_str},
                    FUN.VALUE=character(1))
    if ("next_time" %in% types) {
      sim <- "DES"
    } else {
      sim <- "DTS"
    }
  } else {
    sim <- "DAG"
  }

  if (sim=="DES" & data_format %in% c("long", "wide")) {
    warning("Only the 'start_stop' format is supported with discrete-event",
            " simulations. No data transformation was carried out.",
            call.=FALSE)
  }

  # without parallel processing
  if (n_cores==1) {

    set.seed(seed)
    out <- vector(mode="list", length=n_repeats)
    for (i in seq_len(n_repeats)) {
      out[[i]] <- generate_one_dataset(dag=dag, data_format=data_format,
                                       data_format_args=data_format_args,
                                       n_sim=n_sim, sim=sim, ...)
    }

  # with parallel processing
  } else {

    requireNamespace("parallel", quietly=TRUE)
    requireNamespace("doSNOW", quietly=TRUE)
    requireNamespace("doRNG", quietly=TRUE)
    requireNamespace("foreach", quietly=TRUE)

    `%dorng%` <- doRNG::`%dorng%`

    # set up cluster
    cl <- parallel::makeCluster(n_cores, outfile="")
    doSNOW::registerDoSNOW(cl)

    args <- list(...)
    pckgs <- c("data.table", "simDAG")

    glob_funs <- ls(envir=.GlobalEnv)[vapply(ls(envir=.GlobalEnv), function(obj)
      "function"==class(eval(parse(text=obj)))[1], FUN.VALUE=logical(1))]

    # NOTE: this (for some reason) prevents errors when using custom functions
    #       in nodes that also call custom functions
    parallel::clusterExport(cl=cl, varlist=glob_funs)

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
                           n_sim=n_sim, sim=sim, ...)
    }
    on.exit(close(pb))
    on.exit(parallel::stopCluster(cl))
  }

  return(out)
}

## generate one dataset using either the sim_from_dag() or the
## sim_discrete_time() function and optionally format it
generate_one_dataset <- function(dag, data_format, data_format_args, n_sim,
                                 sim, ...) {

  # simulate the dataset
  if (sim=="DTS") {
    dat <- sim_discrete_time(dag=dag, n_sim=n_sim, ...)
  } else if (sim=="DES") {
    dat <- sim_discrete_event(dag=dag, n_sim=n_sim, ...)
  } else {
    dat <- sim_from_dag(dag=dag, n_sim=n_sim, ...)
  }

  # transform it, if specified
  if (data_format %in% c("start_stop", "long", "wide") &&
      sim=="DTS") {

    data_format_args$to <- data_format
    data_format_args$sim <- dat
    dat <- do.call("sim2data", args=data_format_args)

  } else if (data_format %in% c("start_stop", "long", "wide") &&
             sim=="DES") {
    # do nothing
  } else if (data_format != "raw") {
    data_format_args$data <- dat
    dat <- do.call(data_format, args=data_format_args)
  }

  return(dat)
}
