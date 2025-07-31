
# TODO:
# - write new unit tests
# - finish new vignette on network simulation
# - add some examples of this to cookbook vignette
# - network() in sim_from_dag() currently not allowed to be data dependent
# - currently, networks are always initiated at the same time, irrespective
#   of position in DAG creation, need to change this
# - further features:
#   - allow directed networks
#   - allow other neighborhoods
#   - allow combinations of these

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
    cat("  -", length(igraph::V(x$net)), "vertices\n")
    cat("  -", length(igraph::E(x$net)), "edges\n")
  } else {
    cat("  - net: A function to generate a custom network\n")
  }
}

## S3 summary method for DAG.network objects
#' @export
summary.DAG.network <- function(object, ...) {
  print.DAG.network(x=object, ...)
}

## check inputs for the network() and network_td() functions
check_inputs_network <- function(name, net, time_varying) {

  if (!(length(name)==1 & is.character(name))) {
    stop("'name' must be a single character string.", call.=FALSE)
  } else if (!time_varying & !((igraph::is_igraph(net) &&
                                !igraph::is_directed(net)) |
                               is.function(net))) {
    stop("'net' must be an igraph object containing only undirected edges",
         " or a function that creates such an object.", call.=FALSE)
  } else if (time_varying & !is.function(net)) {
    stop("'net' must be a function creating an igraph object when using",
         " network_td(). See documentation.", call.=FALSE)
  }
}

## this function is used in the formula interface to specify that
## the content of a variable is supposed to be
## the aggregated information of neighbors in a network
#' @importFrom data.table data.table
#' @export
net <- function(expr, net=NULL, na=NA) {

  if (is.null(net)) {
    name <- NA_character_
  } else {
    name <- net
  }

  out <- data.table(
    expr=deparse(substitute(expr)),
    name=name,
    na=na
  )
  return(out)
}

## extract net() terms from parsed formula parts
get_net_terms <- function(formula_parts) {
  net_terms <- formula_parts[startsWith(formula_parts, "net(")]
  return(net_terms)
}

## get a data.table of all undirected edges of an igraph object
#' @importFrom data.table :=
#' @importFrom data.table as.data.table
#' @importFrom data.table setnames
#' @importFrom data.table copy
get_all_edges <- function(g) {

  ..id.. <- ..neighbor.. <- NULL

  d_con <- as.data.table(igraph::as_data_frame(g, what="edges"))

  old <- c("from", "to")
  new <- c("..id..", "..neighbor..")

  # change names
  if (!is.null(igraph::E(g)$weight)) {
    old <- c(old, "weight")
    new <- c(new, "..weight..")
  }
  setnames(d_con, old=old, new=new)

  # add reverse of connections as well
  d_con2 <- copy(d_con)
  setnames(d_con2, old=c("..id..", "..neighbor.."),
           new=c("..neighbor..", "..id.."))
  d_con <- rbind(d_con, d_con2)

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
get_net_info <- function(g, data, net_name) {

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

  d_con <- get_all_edges(g)
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

  # identify those without connections
  d_unconnected <- subset(d_net, is.na(..id..))
  d_unconnected[, ..id.. := ..neighbor..]

  # add them to aggregated data
  d_unconnected <- d_unconnected[, c("..id.."), with=FALSE]
  out <- rbind(out, d_unconnected, fill=TRUE)

  # set NA values to specified "na" value
  if (anyNA(out)) {
    for (i in seq_len(nrow(d_net_terms))) {
      col <- d_net_terms$term[i]
      val <- d_net_terms$na[i]
      if (!is.na(val)) {
        set(out, which(is.na(out[[col]])), col, val)
      }
    }
  }

  return(out)
}

## process all net() terms of a node one by one and merge results to the data
#' @importFrom data.table :=
#' @importFrom data.table setkey
add_network_info <- function(data, d_net_terms, networks) {

  ..id.. <- name <- NULL

  # add temporary id
  data[, ..id.. := seq_len(nrow(data))]

  net_names <- unique(d_net_terms$name)
  out <- vector(mode="list", length=length(net_names))
  for (i in seq_len(length(net_names))) {

    # impose network structure on generated data
    d_net_i <- get_net_info(g=networks[[net_names[i]]]$net,
                            data=data,
                            net_name=net_names[i])

    # aggregate it according to the defined aggregation functions mentioned
    # in the formula call
    d_net_terms_i <- subset(d_net_terms, name==net_names[i])
    out_i <- aggregate_neighbors(d_net=d_net_i,
                                 d_net_terms=d_net_terms_i)
    setkey(out_i, ..id..)
    out_i[, ..id.. := NULL]

    out[[i]] <- out_i
  }

  # put together in one dataset
  out <- do.call(cbind, out)

  # add it to the existing data, remove temporary id
  data[, ..id.. := NULL]
  data <- add_node_to_data(data=data, new=out, name=colnames(out))

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
