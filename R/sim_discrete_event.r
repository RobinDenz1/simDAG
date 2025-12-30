
## Framework function to perform discrete-event simulations
#' @importFrom data.table :=
#' @importFrom data.table copy
#' @importFrom data.table melt.data.table
#' @importFrom data.table merge.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table setkey
#' @importFrom data.table rbindlist
#' @importFrom data.table fifelse
#' @importFrom data.table setcolorder
#' @importFrom data.table setnames
#' @importFrom data.table shift
#' @importFrom data.table uniqueN
#' @importFrom data.table setnafill
#' @importFrom data.table rleid
#' @export
sim_discrete_event <- function(dag, n_sim=NULL, t0_sort_dag=FALSE,
                               t0_data=NULL, t0_transform_fun=NULL,
                               t0_transform_args=list(),
                               max_t=Inf, remove_if, break_if,
                               max_loops=1000, redraw_at_t=NULL,
                               allow_ties=FALSE, censor_at_max_t=FALSE,
                               target_event=NULL, keep_only_first=FALSE,
                               include_event_counts=TRUE, check_inputs=TRUE) {

  # silence devtools check() warnings
  .id <- .time <- .trunc_time <- .time_of_next_event <-
    .time_of_next_change <- .min_time_of_next_event <-
    .min_time_of_next_change <- .event_duration <- .immunity_duration <-
    .kind <- .change <- .event_count <- . <- start <- .col <-
    .time_cuts <- .is_new_event <- .is_new_change <- NULL

  if (!inherits(dag, "DAG")) {
    stop("'dag' must be a DAG object created using the empty_dag() and",
         " node_td() functions.", call.=FALSE)
  }

  requireNamespace("data.table", quietly=TRUE)

  if (check_inputs) {
    check_inputs_sim_discrete_event(dag=dag, n_sim=n_sim,
                                    t0_sort_dag=t0_sort_dag,
                                    t0_data=t0_data,
                                    t0_transform_fun=t0_transform_fun,
                                    t0_transform_args=t0_transform_args,
                                    max_t=max_t,
                                    censor_at_max_t=censor_at_max_t,
                                    redraw_at_t=redraw_at_t,
                                    include_event_counts=include_event_counts)
  }

  # add another node that enforces a time-break at specific points in time
  if (!is.null(redraw_at_t)) {
    dag <- dag + node_td(".time_cuts", type="next_time", prob_fun=1,
                         event_duration=0, distr_fun=timecuts,
                         distr_fun_args=list(cuts=redraw_at_t))
  }

  # handle sub-setting / break conditioning
  miss_remove_if <- missing(remove_if)
  miss_break_if <- missing(break_if)

  if (!miss_break_if) {
    break_expr <- substitute(break_if)
  }
  if (!miss_remove_if) {
    cond_expr <- substitute(remove_if)

    if (length(dag$networks) > 0) {
      warning("Using the 'remove_if' argument with one or more network() or",
              " network_td() calls added to the supplied DAG is currently",
              " not supported and may lead to errors.", call.=FALSE)
    }
  }

  tx_nodes <- prepare_next_time_nodes(dag$tx_nodes)

  # get initial data
  if (is.null(t0_data) & length(dag$root_nodes)==0 &
      length(dag$child_nodes)==0) {
    data <- data.table(.id=seq(1, n_sim))
  } else if (is.null(t0_data)) {
    dag$tx_nodes <- NULL
    data <- sim_from_dag(n_sim=n_sim,
                         dag=dag,
                         sort_dag=t0_sort_dag,
                         check_inputs=check_inputs)
    data[, .id := seq(1, n_sim)]
  } else {
    data <- as.data.table(t0_data)
    n_sim <- nrow(data)
    data[, .id := seq(1, n_sim)]
  }

  # perform an arbitrary data transformation right at the start
  if (!is.null(t0_transform_fun)) {
    t0_transform_args$data <- data
    data <- do.call(t0_transform_fun, args=t0_transform_args)
  }

  # set time to 0 at start
  data[, .time := 0]
  data[, .trunc_time := 0]

  # extract names and initialize relevant vars
  var_names <- vapply(tx_nodes, function(x){x$name}, FUN.VALUE=character(1))
  data[, (var_names) := FALSE]
  cnames <- copy(colnames(data))

  # initialize output list (including data at t = 0)
  out <- list(copy(data))

  # transform data to long-format
  data <- melt.data.table(
    data=data,
    id.vars=cnames,
    measure.vars=var_names,
    variable.name=".kind",
    value.name=".time_of_next_event"
  )
  setkey(data, .id)

  # initial column values
  data[, .time_of_next_event := .time_of_next_event * 1.2] # turn into double
  data[, .time_of_next_event := Inf]
  data[, .time_of_next_change := Inf]
  data[, .event_count := 0]

  # extract event_duration from all nodes and add them to the data
  event_durations <- vapply(tx_nodes, FUN=extract_node_arg,
                            FUN.VALUE=numeric(1), fun=node_next_time,
                            arg="event_duration")
  data[, .event_duration := rep(event_durations, n_sim)]

  # extract immunity_duration from all nodes and add them to the data
  immunity_durations <- vapply(tx_nodes, FUN=extract_node_arg,
                               FUN.VALUE=numeric(1), fun=node_next_time,
                               arg="immunity_duration")
  data[, .immunity_duration := rep(immunity_durations, n_sim)]

  # extract whether an event_count should be added
  event_count_ind <- vapply(tx_nodes, FUN=extract_node_arg,
                            FUN.VALUE=logical(1), fun=node_next_time,
                            arg="event_count")
  has_event_counts <- sum(event_count_ind) > 0

  if (has_event_counts) {
    event_count_rel <- var_names[event_count_ind]
    event_count_names <- paste0(event_count_rel, "_event_count")

    data[, (event_count_names) := 0]

    if (include_event_counts) {
      cnames <- c(cnames, event_count_names)
    }
  }

  loop_count <- 0

  ## main loop, runs until condition is met or nothing is left
  repeat {

    # for each node
    for (i in seq_len(length(var_names))) {

      rel_row <- data$.kind==var_names[i] &
        is.infinite(data$.time_of_next_change)

      args_p <- remove_node_internals(tx_nodes[[i]])

      # call prob_fun with correct arguments
      if (!is.null(tx_nodes[[i]]$prob_fun)) {
        args_p$data <- data[rel_row==TRUE]

        p_est <- tryCatch({
          do.call(tx_nodes[[i]]$prob_fun, args=args_p)},
          error=function(e){
            stop("Calling 'prob_fun' failed with error message:\n", e,
                 call.=FALSE)
          }
        )
      # use formula instead otherwise
      } else if (!is.null(tx_nodes[[i]]$formula)) {
        args_p <- args_from_formula(args=args_p, formula=tx_nodes[[i]]$formula,
                                    node_type="binomial")
        args_p$return_prob <- TRUE

        args_p$data <- tryCatch({
          data_for_formula(data=data[rel_row==TRUE], args=args_p,
                           networks=list())},
          error=function(e){
            stop("An error occured when interpreting the formula of node '",
                 tx_nodes[[i]]$name, "'. The message was:\n", e,
                 call.=FALSE)
          }
        )
        p_est <- do.call(node_binomial, args=args_p)
      } else {
        stop("Either 'prob_fun' or 'formula' need to be specified when",
             " using nodes of type='next_time'.", call.=FALSE)
      }

      # draw time until next event from truncated distribution
      args_dist <- tx_nodes[[i]]$distr_fun_args
      args_dist$n <- sum(rel_row)
      args_dist$rate <- p_est
      args_dist$l <- data[rel_row==TRUE]$.trunc_time

      distr_fun_out <- tryCatch({
        do.call(tx_nodes[[i]]$distr_fun, args=args_dist)},
        error=function(e){
          stop("Calling 'distr_fun' failed with error message:\n", e,
               call.=FALSE)
        }
      )

      data[rel_row==TRUE, .time_of_next_event := distr_fun_out]
    }

    # identify the minimum event time and minimum time until state change
    data[, `:=`(
      .min_time_of_next_event = min(.time_of_next_event),
      .min_time_of_next_change = min(.time_of_next_change)
    ), by=.id]

    # update simulation times
    data[, .time := fifelse(.min_time_of_next_event < .min_time_of_next_change,
                            .min_time_of_next_event, .min_time_of_next_change)]
    data[.time > .trunc_time, .trunc_time := .time]

    # check for new events / changes
    data[, .is_new_event := .time==.time_of_next_event]
    data[, .is_new_change := .time==.time_of_next_change]

    # increase event count
    if (has_event_counts) {
      data <- set_cols_to_value(data=data, .value=TRUE, type="event_count",
                                var_names=event_count_rel,
                                event_count_names=event_count_names,
                                allow_ties=allow_ties)
    }

    # set variables to TRUE if needed
    data <- set_cols_to_value(data=data, .value=TRUE, type="event",
                              var_names=var_names, allow_ties=allow_ties)

    # set .time_of_next_change for all new events
    data[.is_new_event==TRUE, `:=`(
      .time_of_next_change = .time + .event_duration,
      .time_of_next_event = Inf,
      .event_count = .event_count + 1
    )]
    data[.is_new_event==TRUE, .trunc_time := .time + .immunity_duration]

    # set variables back to FALSE if needed
    data <- set_cols_to_value(data=data, .value=FALSE, type="change",
                              var_names=var_names, allow_ties=allow_ties)

    # set .time_of_next_change back to Inf for all variables that
    # turned back to FALSE
    data[.time==.time_of_next_change, .time_of_next_change := Inf]

    # save state of the simulation
    out[[length(out) + 1]] <- data[!duplicated(data$.id), cnames, with=FALSE]

    # remove rows that no longer need to be updated
    data <- data[!(is.infinite(.event_duration) & .is_new_event==TRUE) &
                 !(is.infinite(.immunity_duration) & .is_new_change==TRUE) &
                 .time < max_t]

    # subset if specified
    if (!miss_remove_if) {
      data <- data[!(eval(cond_expr))]
    }

    # break if condition reached
    if (nrow(data)==0 | (!miss_break_if && eval(break_expr))) {
      break
    }

    # break if max loops reached
    if (loop_count==max_loops) {
      warning("Simulation stopped because the maximum amount of loops was",
              " reached. Adjust the 'max_loops' argument or use the",
              " 'break_if', 'remove_if' or 'max_t' arguments to define",
              " better ends of the simulation.", call.=FALSE)
      break
    }
    loop_count <- loop_count + 1
  }

  # create a start-stop output dataset
  d_start_stop <- rbindlist(out, fill=TRUE)
  setkey(d_start_stop, .id, .time)

  setnames(d_start_stop, old=".time", new="start")
  d_start_stop[, stop := shift(start, n=-1, fill=NA), by=.id]

  # re-order columns
  cnames <- cnames[!cnames %in% c(".id", ".time")]
  setcolorder(d_start_stop, neworder=c(".id", "start", "stop", cnames))

  d_start_stop[, .trunc_time := NULL]

  # remove time-cuts from output
  if (!is.null(redraw_at_t)) {
    d_start_stop <- d_start_stop[.time_cuts==FALSE & is.finite(start)]
    d_start_stop[, .time_cuts := NULL]
  }

  # potentially censor at max_t
  if (censor_at_max_t) {
    d_start_stop <- d_start_stop[start < max_t]
    d_start_stop[stop > max_t | is.na(stop), stop := max_t]
  }

  # transform it to be event centric, if specified
  if (!is.null(target_event)) {
    d_start_stop <- collapse_for_target_event(data=d_start_stop,
                                              target_event=target_event,
                                              keep_only_first=keep_only_first)
  }

  # fill NA values in event counts
  if (include_event_counts && has_event_counts) {
    setnafill(d_start_stop, type="const", fill=0, cols=event_count_names)
  }

  return(d_start_stop)
}

## node type for the sim_discrete_event() function
# NOTE: this function obviously doesn't really do anything, since the
#       sim_discrete_event() function does it all internally, but it
#       needs to be defined anyways so it is correctly processed in node()
#' @export
node_next_time <- function(data, formula, prob_fun, ..., distr_fun=rtexp,
                           distr_fun_args=list(), event_duration=Inf,
                           immunity_duration=event_duration,
                           event_count=FALSE) {
  return(NULL)
}

## get the value of a specific argument in a DAG.node object
extract_node_arg <- function(node, fun, arg) {
  if (is.null(node[[arg]]) & arg!="immunity_duration") {
    out <- formals(fun=fun)[[arg]]
  } else if (is.null(node[[arg]])) {
    out <- extract_node_arg(node=node, fun=fun, arg="event_duration")
  } else {
    out <- node[[arg]]
  }
  return(out)
}

## removes internal parts of a node (so that it can be used as an
## argument list for a do.call())
remove_node_internals <- function(node) {

  node$name <- NULL
  node$type_str <- NULL
  node$type_fun <- NULL
  node$parents <- NULL
  node$time_varying <- NULL
  node$..index.. <- NULL
  node$prob_fun <- NULL
  node$event_duration <- NULL
  node$immunity_duration <- NULL
  node$distr_fun <- NULL
  node$distr_fun_args <- NULL
  node$event_count <- NULL

  return(node)
}

## set defaults and correct arguments passed using node_next_time()
prepare_next_time_nodes <- function(nodes) {

  for (i in seq_len(length(nodes))) {

    # set default value of distr_fun
    if (is.null(nodes[[i]]$distr_fun)) {
      nodes[[i]]$distr_fun <- rtexp
    }

    # function that only returns
    if (is.numeric(nodes[[i]]$prob_fun)) {
      nodes[[i]]$.value <- nodes[[i]]$prob_fun
      nodes[[i]]$prob_fun <- pass_value
    }

    # prepare formula
    if (!is.null(nodes[[i]]$formula)) {
      nodes[[i]]$formula <- sanitize_formula(nodes[[i]]$formula)
    }
  }
  return(nodes)
}

## needed for prepare_next_time_nodes()
pass_value <- function(data, .value) {
  return(.value)
}

## used to trick sim_discrete_event() into recomputing hazards and times
## at pre-specified time points
timecuts <- function(n, rate, l, cuts) {

  next_time_cut <- findInterval(x=l, vec=cuts)
  next_time_cut <- cuts[next_time_cut + 1]
  next_time_cut[is.na(next_time_cut)] <- Inf

  return(next_time_cut)
}

## this function efficiently either sets the covariate values to TRUE if
## a new event occurred or sets them back to FALSE if needed
set_cols_to_value <- function(data, .value, type, var_names, allow_ties,
                              event_count_names=NULL) {

  .is_new_event <- .kind <- .id <- .is_new_change <- .time <-
    .time_of_next_event <- . <- .time_of_next_change <- .change <- NULL

  # slower version that allows ties
  if (allow_ties) {

    if (type=="event_count") {

      for (i in seq_len(length(var_names))) {
        data[.is_new_event==TRUE & .kind==var_names[i],
             (event_count_names[i]) := get(event_count_names[i]) + 1]
        data[, (event_count_names[i]) := max(get(event_count_names[i])),
             by=.id]
      }
    } else {

      for (.col in var_names) {
        if (type=="event") {
          data[.is_new_event==TRUE & .kind==.col, (.col) := TRUE]
          data[, (.col) := any(get(.col)), by=.id]
        } else if (type=="change") {
          data[.is_new_change==TRUE & .kind==.col, (.col) := FALSE]
          data[, (.col) := sum(get(.col))==.N, by=.id]
        }
      }
    }
  # faster version that does not allow ties
  } else {

    if (type=="event" | type=="event_count") {
      d_change <- data[.time==.time_of_next_event,
                       .(.change = .kind), by=.id]
    } else {
      d_change <- data[.time==.time_of_next_change,
                       .(.change = .kind), by=.id]
    }

    # check for ties
    if (uniqueN(d_change$.id)!=nrow(d_change)) {
      stop("Multiple changes at the same point in time occurred, but",
           " allow_ties=FALSE was used. Set allow_ties=TRUE and re-run",
           " this function.", call.=FALSE)
    }

    if (nrow(d_change) > 0) {
      data <- merge.data.table(data, d_change, by=".id", all.x=TRUE)

      for (i in seq_len(length(var_names))) {
        if (type=="event" | type=="change") {
          data[.change==var_names[i], (var_names[i]) := .value]
        } else {
          data[.change==var_names[i],
               (event_count_names[i]) := get(event_count_names[i]) + 1]
        }
      }
      data[, .change := NULL]
    }
  }
  return(data)
}
