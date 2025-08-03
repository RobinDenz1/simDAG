
## similar to node() and node_td(), but instead of creating an actual node
## for the DAG, it creates a network for the DAG
#' @export
network <- function(name, net, ...) {
  create_DAG.network(name=name, net=net, time_varying=FALSE,
                     create_at_t0=TRUE, ...)
}

## same as network(), but with a time-varying network
#' @export
network_td <- function(name, net, create_at_t0=TRUE, ...) {
  create_DAG.network(name=name, net=net, time_varying=TRUE,
                     create_at_t0=create_at_t0, ...)
}

## creates a DAG.network object to add to a DAG object
create_DAG.network <- function(name, net, time_varying, create_at_t0, ...) {

  check_inputs_network(name=name, net=net, time_varying=time_varying)

  out <- list(name=name,
              net=NULL,
              net_fun=NULL,
              args=list(...),
              time_varying=time_varying,
              create_at_t0=create_at_t0)
  class(out) <- "DAG.network"

  if (is.function(net)) {
    out$net_fun <- net
  } else {
    out$net <- net
  }

  return(out)
}

## S3 print method for DAG.network objects
#' @export
print.DAG.network <- function(x, ...) {
  cat("A DAG.network object specifying a network structure with:\n")
  cat("  - name: '", x$name, "'\n", sep="")

  if (igraph::is_igraph(x$net)) {

    if (igraph::is_weighted(x$net)) {
      weighted <- "weighted"
    } else {
      weighted <- "un-weighted"
    }

    if (igraph::is_directed(x$net)) {
      directed <- "directed"
    } else {
      directed <- "un-directed"
    }

    cat("  -", length(igraph::V(x$net)), "vertices\n")
    cat("  -", length(igraph::E(x$net)), directed, weighted, "edges\n")
  } else {
    cat("  - net: A function to generate a custom network\n")
  }
}

## S3 summary method for DAG.network objects
#' @export
summary.DAG.network <- function(object, ...) {
  print.DAG.network(x=object, ...)
}

## this function is used in the formula interface to specify that
## the content of a variable is supposed to be
## the aggregated information of neighbors in a network
#' @importFrom data.table data.table
#' @export
net <- function(expr, net=NULL, mode="all", order=1, na=NA) {

  if (is.null(net)) {
    name <- NA_character_
  } else {
    name <- net
  }

  out <- data.table(
    expr=deparse(substitute(expr)),
    name=name,
    mode=mode,
    order=order,
    na=na
  )
  return(out)
}

## extract net() terms from parsed formula parts
get_net_terms <- function(formula_parts) {
  net_terms <- formula_parts[startsWith(formula_parts, "net(")]
  return(net_terms)
}

## get all neighbors of every vertex in a graph in data.table format
## NOTE: this could be used for order = 1, but it is much slower, especially
#        for large graphs
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
get_neighbors_with_order <- function(g, order, mode) {

  ..id.. <- ..neighbor.. <- NULL

  if (order > 1 & igraph::is_weighted(g)) {
    warning("When using order > 1 for weighted graphs, the weights",
            " are ignored.", call.=FALSE)
  }

  lneighbors <- igraph::neighborhood(graph=g, order=order, mode=mode)

  out <- vector(mode="list", length=length(lneighbors))
  for (i in seq_len(length(lneighbors))) {
    out[[i]] <- data.table(..id..=i,
                           ..neighbor..=as.numeric(lneighbors[[i]]))
  }
  out <- subset(rbindlist(out), ..id.. != ..neighbor..)

  return(out)
}

## get a data.table of all undirected edges of an igraph object
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
get_all_edges <- function(g, mode, order) {

  ..id.. <- ..neighbor.. <- NULL

  # take computationally more expensive approach if order is specified
  if (order != 1) {
    d_con <- get_neighbors_with_order(g=g, mode=mode, order=order)
    return(d_con)
  }

  d_con <- as.data.table(igraph::as_data_frame(g, what="edges"))

  old <- c("from", "to")
  new <- c("..id..", "..neighbor..")

  # change names
  if (!is.null(igraph::E(g)$weight)) {
    old <- c(old, "weight")
    new <- c(new, "..weight..")
  }
  setnames(d_con, old=old, new=new)

  # get reverse of connections
  if (mode=="all" | mode=="in" | !igraph::is_directed(g)) {
    d_con2 <- copy(d_con)
    setnames(d_con2, old=c("..id..", "..neighbor.."),
             new=c("..neighbor..", "..id.."))
  }

  if (mode=="all" | !igraph::is_directed(g)) {
    d_con <- rbind(d_con, d_con2)
  } else if (mode=="in") {
    d_con <- d_con2
  }

  # vertex names should be numbers
  d_con[, ..id.. := as.numeric(..id..)]
  d_con[, ..neighbor.. := as.numeric(..neighbor..)]

  return(d_con)
}

## given a graph and data containing information for subjects in the graph,
## create a new data.table mapping the two, such that information for each
## neighbor is mapped to the observation
#' @importFrom data.table :=
#' @importFrom data.table merge.data.table
get_net_info <- function(g, data, net_name, mode, order) {

  n_vertices <- length(igraph::V(g))

  if (n_vertices < nrow(data)) {
    stop(paste0("The network named '", net_name, "' only contains ",
                n_vertices, " vertices, but the simulated data contains ",
                nrow(data), " observations. There should be at least ",
                nrow(data), " vertices in the network to represent each ",
                "observation."), call.=FALSE)
  } else if (n_vertices > nrow(data)) {
    stop(paste0("The network name '", net_name, "' contains ",
                n_vertices, " vertices, but the simulated data only",
                " has ", nrow(data), " observations. There should",
                " be one vertex per observation."), call.=FALSE)
  }

  d_con <- get_all_edges(g=g, mode=mode, order=order)
  d_net <- merge.data.table(d_con, data, by.x="..neighbor..", by.y="..id..",
                            all.x=TRUE, all.y=TRUE, allow.cartesian=TRUE)
  return(d_net)
}

## given the network relationships and the expressions given by the user,
## create a new data.table containing aggregated information about the
## neighbors of an observation
#' @importFrom data.table :=
#' @importFrom data.table set
aggregate_neighbors <- function(d_net, d_net_terms) {

  ..id.. <- ..neighbor.. <- NULL

  # aggregate all with connections
  agg_funs <- paste0(paste0("`", d_net_terms$term, "` = ",
                            d_net_terms$expr), collapse=", ")
  d_aggregate <- paste0("d_net[!is.na(..id..), .(", agg_funs, "), by='..id..']")
  out <- eval(str2lang(d_aggregate))

  return(out)
}

## process all net() terms of a node one by one and merge results to the data
#' @importFrom data.table :=
#' @importFrom data.table setkey
add_network_info <- function(data, d_net_terms, networks) {

  ..id.. <- name <- NULL

  # add temporary id
  data <- copy(data)
  data[, ..id.. := seq_len(nrow(data))]

  d_combs <- unique(d_net_terms, by=c("name", "mode", "order"))
  for (i in seq_len(nrow(d_combs))) {

    # impose network structure on generated data
    d_net_i <- get_net_info(g=networks[[d_combs$name[i]]]$net,
                            data=data,
                            net_name=d_combs$name[i],
                            mode=d_combs$mode[i],
                            order=d_combs$order[i])

    # aggregate it according to the defined aggregation functions mentioned
    # in the formula call
    d_net_terms_i <- subset(d_net_terms, name==d_combs$name[i] &
                              mode==d_combs$mode[i] &
                              order==d_combs$order[i])
    out_i <- aggregate_neighbors(d_net=d_net_i,
                                 d_net_terms=d_net_terms_i)
    data <- merge.data.table(data, out_i, by="..id..", all.x=TRUE)
  }

  # set NA values to specified "na" value
  if (anyNA(data)) {
    for (i in seq_len(nrow(d_net_terms))) {
      col <- d_net_terms$term[i]
      val <- d_net_terms$na[i]
      if (!is.na(val)) {
        set(data, which(is.na(data[[col]])), col, val)
      }
    }
  }

  # remove temporary id
  data[, ..id.. := NULL]

  return(data)
}

## initiates / updates networks if needed
create_networks <- function(networks, n_sim, data=NULL, sim_time,
                            past_states=NULL, past_networks=NULL) {

  for (i in seq_len(length(networks))) {

    # only initiate / update network if:
    # 1. first time and it should be initiated
    # 2. > first time and it should be updated
    if (!(!is.function(networks[[i]]$net_fun) ||
          (sim_time > 0 & !networks[[i]]$time_varying) ||
          (sim_time==0 & !networks[[i]]$create_at_t0))) {

      fun_args <- names(formals(networks[[i]]$net_fun))
      args <- list(n_sim=n_sim)

      if ("data" %in% fun_args) {
        args$data <- data
      }
      if ("sim_time" %in% fun_args) {
        args$sim_time <- sim_time
      }
      if ("past_states" %in% fun_args & sim_time > 0) {
        args$past_states <- past_states
      }
      if ("network" %in% fun_args & sim_time > 0) {
        args$network <- networks[[i]]$net
      }
      if ("past_networks" %in% fun_args & sim_time > 0) {
        args$past_networks <- past_networks
      }

      networks[[i]]$net <- do.call(networks[[i]]$net_fun, args=args)
    }
  }

  return(networks)
}
