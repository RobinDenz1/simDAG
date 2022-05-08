
## get information for linear node from data
model_node_gaussian <- function(name, parents, data, return_model=FALSE,
                                ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="gaussian")
  args <- c(args, ...)

  # fit model
  model <- do.call(glm, args)

  # extract coef, intercept, sigma
  out <- list(name=name,
              parents=parents,
              type="gaussian",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]),
              error=sd(model$residuals))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## get information for binomial node from data
model_node_binomial <- function(name, parents, data, return_model=FALSE,
                                ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="binomial")
  args <- c(args, ...)

  # fit model
  model <- do.call(glm, args)

  # extract coef, intercept, sigma
  out <- list(name=name,
              parents=parents,
              type="binomial",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## get information for poisson node from data
model_node_poisson <- function(name, parents, data, return_model=FALSE,
                               ...) {

  # create formula
  form <- stats::as.formula(paste0(name, " ~ ",
                                   paste0(parents, collapse=" + ")))
  # create list of arguments
  args <- list(formula=form, data=data, family="poisson")
  args <- c(args, ...)

  # fit model
  model <- do.call(glm, args)

  # extract coef, intercept, sigma
  out <- list(name=name,
              parents=parents,
              type="poisson",
              betas=as.vector(model$coefficients[-1]),
              intercept=as.vector(model$coefficients[1]))

  if (return_model) {
    out$model <- model
  }
  return(out)
}

## gaussian root node from data
root_gaussian <- function(data, name, na.rm) {
  out <- list(name=name,
              dist="rnorm",
              params=list(mean=mean(data[, name], na.rm=na.rm),
                          sd=sd(data[, name], na.rm=na.rm)))
  return(out)
}

## binomial root node from data
root_binomial <- function(data, name, na.rm) {
  out <- list(name=name,
              dist="rbernoulli",
              params=list(p=mean(data[, name], na.rm=na.rm)))
  return(out)
}

## multinomial root node from data
root_multinomial <- function(data, name, na.rm) {
  tab <- prop.table(table(data[, name]))
  out <- list(name=name,
              dist="rcategorical",
              params=list(labels=names(tab), probs=tab))
}

## given minimal information on node type and the causal structure,
## create lists for the root_nodes and child_nodes from observed data
## by fitting appropriate models
nodes_from_data <- function(data, nodes, return_models=FALSE,
                            na.rm=FALSE) {

  # count number of children, roots
  n_roots <- sum(vapply(nodes, FUN=function(x){is.null(x$parents)},
                        FUN.VALUE=logical(1)))
  n_children <- length(nodes) - n_roots

  # initialize lists for children and root nodes
  root_nodes <- vector(mode="list", length=n_roots)
  child_nodes <- vector(mode="list", length=n_children)

  # loop over all node definitions and estimate models
  root_count <- 1
  child_count <- 1
  for (i in seq_len(length(nodes))) {

    if (is.null(nodes[[i]]$parents)) {
      # call associated root function
      root_fun <- get(paste0("root_", nodes[[i]]$type))
      root_nodes[[root_count]] <- root_fun(data, name=node[[i]]$name)

      root_count <- root_count + 1
    } else {
      # get associated model_node function
      model_fun <- get(paste0("model_node_", nodes[[i]]$type))

      # call it, getting a fully specified node
      args <- list(data=data,
                   name=nodes[[i]]$name,
                   parents=nodes[[i]]$parents,
                   return_model=return_models)
      args <- c(args, nodes[[i]]$args)
      child_nodes[[child_count]] <- do.call(model_fun, args)

      child_count <- child_count + 1
    }
  }

  if (return_models) {
    # extract models from children
    models <- lapply(child_nodes, FUN=function(x){x$model})
    names(models) <- vapply(child_nodes, FUN=function(x){x$name},
                            FUN.VALUE=character(1))

    # remove model from children
    for (i in seq_len(length(child_nodes))) {
      child_nodes[[i]]$model <- NULL
    }

    out <- list(root_nodes=root_nodes,
                child_nodes=child_nodes,
                models=models)
  } else {
    out <- list(root_nodes=root_nodes,
                child_nodes=child_nodes)
  }
  return(out)
}

# TODO:
# - finish this stuff
# - document it
# - allow more node types

library(adjustedCurves)
inp_data <- sim_confounded_surv(n=500)

nodes <- list(list(parents=NULL,
                   name="x1",
                   type="binomial"),
              list(parents=NULL,
                   name="x2",
                   type="gaussian"),
              list(parents=c("x1", "x2"),
                   name="group",
                   type="binomial"),
              list(parents=c("x1", "x2", "group"),
                   name="x3",
                   type="gaussian"))

test <- nodes_from_data(data=inp_data, nodes=nodes, return_models=TRUE)
test$child_nodes
